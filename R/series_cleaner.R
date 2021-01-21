#' @title series_cleaner
#'
#' @description This function corrects time series of gas concentrations within a certain depth at one site.
#' The basic assumption is that gas data of different samplers within a depth are strictly ordered due to spatial
#' heterogeneity or differences in the exact installation depth, i.e. always one sampler shows the highest
#' concentration, so that the difference of each sampler on each day to the respective mean
#' of all samplers is more or less the same.
#'
#' This function does not apply if the data either has only one observation per depth or if
#' the data does not show any intrinsic ordering inside each depth. Check these preconditions first.
#'
#' The function outputs a dataframe that has the same structure as the input, so that the original input can be removed.
#' The function can be used using dplyr group_modify()-function
#'
#' @param
#' df (dataframe)
#' @param
#' mode (character) "data" returns the corrected dataset. "plots" returns before / after plots.
#'
#'
#' @return dataframe
#'
#' @examples
#' co2_clean<-gasdata %>%
#' dplyr::filter(gas == "CO2") %>%
#' dplyr::group_by(Plot,depth_cat) %>%
#' dplyr::group_modify(~series_cleaner(.x))
#'
#' gasdata<-gasdata %>%
#' dplyr::filter(!gas == "CO2") %>%
#' dplyr::bind_rows(co2_clean)
#'
#' %Exactly the same but editing data directly
#' gasdata <- gasdata %>%
#' dplyr::group_by(Plot,depth_cat,gas) %>%
#' dplyr::group_modify(~{
#'            if(.y$gas == "CO2"){
#'                df <- series_cleaner(.x)
#'            } else {
#'                df <- .x}
#'            return(df)
#'                       })
#'
#' @family gasdata
#'
#' @import lubridate
#' @import dplyr
#'
#' @export



series_cleaner<-function(df,mode="data"){

  #backup of input df
  df_orig <- df

  #Clear entries without Date
  df <- df %>%
    dplyr::filter(is.na(Date)==F) %>%
    dplyr::mutate(year = lubridate::year(Date))

  meandiff<-function(df){
    df<- df %>%
      dplyr::select(!contains("mean")) %>%
      #calculating mean by Date
      dplyr::left_join(df %>%
                         dplyr::group_by(Date) %>%
                         dplyr::summarise(mean=mean(NRESULT_ppm,na.rm = T))) %>%
      #Calculating difference to mean
      dplyr::mutate(Ndiff=NRESULT_ppm-mean)
    df <- df %>%
      #calculating mean difference
      dplyr::left_join(df %>%
                         dplyr::group_by(Date,depth) %>%
                         dplyr::summarise(med_diff=median(Ndiff,na.rm=T))) %>%
      #calculating relative difference to mean difference
      dplyr::mutate(diff_sd=Ndiff/med_diff)
    return(df)
  }

  df<-meandiff(df)

  #Finding outliers per year and MST_ID and setting them NA.
  df_new <- df %>%
    dplyr::group_by(year, MST_ID) %>%
    dplyr::group_modify(~{
      bs<-boxplot.stats(.x$Ndiff)
      lims<-range(bs$stats)
      .x <- .x %>%
        dplyr::mutate(NRESULT_ppm  = ifelse(Ndiff < !!lims[1] | Ndiff > !!lims[2],NA,NRESULT_ppm))
      return(.x)
    })

  #Recalculate new means and Ndiffs
  df_new<-meandiff(df_new)

  # calculate mean difference of each MST_ID to mean by month
  #only from dates with full observations (and hence no outliers)
  mst_diffs <-  df_new %>%
    dplyr::mutate(n=length(unique(df_new$MST_ID))) %>% #n = number of MST in each depth
    dplyr::full_join(df_new %>%
                       dplyr::filter(is.na(NRESULT_ppm)==F) %>%
                       dplyr::group_by(Date) %>%
                       dplyr::count(name="m")) %>% #m = number of non-NA obs per depth and Day
    dplyr::mutate(s=n-m) %>%
    dplyr::filter(!s>0 & is.na(s)==F) %>% #only use days with full obs
    dplyr::select(!contains(c("n","m","s"))) %>%
    dplyr::left_join(df_new) %>%
    dplyr::mutate(month=month(Date)) %>%
    dplyr::group_by(MST_ID,month) %>%
    dplyr::summarise(mean_diff=mean(NRESULT_ppm-mean),sd_diff=sd(NRESULT_ppm-mean),depth=depth[1]) #calculate mean diff to mean for each MST and Month


  #counts number of groups for offset correction
  n_groups<-df_new %>%
    group_by(Date) %>%
    count() %>%
    nrow()


  #Correction of df_new$mean with mean differences of missing datapoints
  meanoffset<-function(df){

    #The base equation for this is:
    #    mean(x) = 1/s * sum(dt) + mean(xs)
    #or: mean(x) = 1/s * sum(dt) + 1/s * sum(xs)

    #with: xs = existing (non-NA) datapoints
    #      s  = length(xs)
    #      xt = missing datapoints
    #      dt = xt - mean(x)

    i <- as.numeric(df$i[1])
    depth <- df$depth[1]
    mon <- month(df$Date[1])

    if (i %in% round(seq(1,n_groups,length.out = 11))){
      print(paste(depth,df$Date[1],round(i/n_groups,digits = 2)*100,"%"))
    }


    if (length(which(is.na(df$mean)==F))<1){
      return(df)
    }

    notna<-unique(df$MST_ID[is.na(df$NRESULT_ppm)==F])
    if (length(notna) == length(unique(df$MST_ID))){
      return(df)
    }
    #first try month specific
    offset<-mst_diffs %>%
      dplyr::filter(!MST_ID %in% notna & month == mon & depth == depth) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(sum=sum(mean_diff)) %>%
      dplyr::pull(sum)

    #if unsuccesfull, i.e. if no mean diffs in that month,
    #try with whole complete dataset (more robust, but greater error)
    if(length(offset)==0){
      t<-1
    } else if(is.na(offset)==T){
      t<-1
    } else{
      t<- 0
    }
    #t<-1
    if (t==1){
      offset <- mst_diffs %>%
        dplyr::filter(!MST_ID %in% notna & depth == depth) %>%
        dplyr::group_by(MST_ID) %>%
        dplyr::summarise(mean_diff=mean(mean_diff,na.rm=T)) %>%
        dplyr::summarise(sum=sum(mean_diff,na.rm = T)) %>%
        dplyr::pull(sum)
    }
    offset<-offset/(length(notna))
    df$mean<-df$mean+offset
    df$Ndiff<-df$NRESULT_ppm-df$mean
    return(df)
  }

  ##recalculate mean with offset correction
  df_new <- df_new %>%
    dplyr::left_join(df_new %>%
                       dplyr::group_by(Date) %>%
                       dplyr::count() %>%
                       tibble::rownames_to_column(var = "i") %>%
                       dplyr::select(!"n")) %>%
    dplyr::group_by(Date) %>%
    dplyr::group_map(~meanoffset(.x),.keep = T) %>%
    dplyr::bind_rows()




  na_interpol<-function(Date, Ndiff){
    if(length(which(is.na(Ndiff)==F))<2){
      return(Ndiff)
    } else {
      return(approxfun(Date,Ndiff)(Date))
    }
  }

  df_new2<-df_new %>%
    dplyr::group_by(MST_ID) %>%
    dplyr::mutate(Ndiff=na_interpol(Date,Ndiff)) %>%
    #pull(diff)
    dplyr::mutate(NRESULT_ppm=mean+Ndiff)

  return(df_new2)
}

