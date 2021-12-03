#' @title proflux_error
#'
#' @description A set of functions that can be called on an
#' PFres object (the result of a call to pro_flux) to assess
#' the quality of the model.
#'
#' @param PROFLUX A PFres object, that is returned by a call
#' to pro_flux()
#'
#' @param param_cols The columns that, together, define different parameters (e.g. different gases)
#' for which NRMSEs should be calculated separately (e.g. "gas").
#'
#' @inheritParams rmse
#'
#' @name proflux_error
NULL
#'
#' @rdname proflux_error

  error_gasdata <- function(
    PROFLUX,
    param_cols,
    normer
  ) {
    id_cols <- pf_id_cols(PROFLUX)
    gasdata <- pf_gasdata(PROFLUX)
    profiles <- pf_profiles(PROFLUX)

    gasdata <-
      gasdata %>%
      dplyr::select(dplyr::any_of(
        {c(id_cols,
           "NRESULT_ppm",
           "depth")})) %>%
      dplyr::rename(upper = depth,
                    conc_ref = NRESULT_ppm)

    PROFLUX %>%
      dplyr::select(dplyr::any_of(
        {c(id_cols,
           "upper",
           "conc",
           "rho_air")}
      )) %>%
      dplyr::mutate(conc_ppm = conc/rho_air) %>%
      dplyr::left_join(gasdata) %>%

      # grouping by upper first
      # because different depths have different
      # value ranges (lower are higher).
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of({c(param_cols,"upper")}))) %>%
      dplyr::summarise(NRMSE = nrmse(conc/rho_air,
                                     conc_ref,
                                     norm = !!normer)) %>%

      # then by only the param_cols provided
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of({param_cols}))) %>%

      # then calculation of mean of all depth-NRMSEs
      dplyr::summarise(NRMSE = mean(NRMSE,
                                    na.rm = T))
  }


#' @rdname proflux_error

  error_efflux <- function(PROFLUX,
                           param_cols,
                           EFFLUX,
                           normer){

    id_cols <- pf_id_cols(PROFLUX)

    EFFLUX <-
      EFFLUX %>%
      data.frame() %>%
      dplyr::select(
        dplyr::any_of({c(id_cols,"efflux")})) %>%
      dplyr::rename(efflux_ref = efflux)

    PROFLUX %>%
      pf_efflux() %>%
      dplyr::left_join(EFFLUX) %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of({param_cols}))) %>%
      dplyr::summarise(NRMSE = nrmse(efflux,
                                     efflux_ref,
                                     norm = !!normer))

  }

#' @rdname proflux_error

  error_compare_models <- function(PROFLUX,
                                   PF_summary,
                                   param_cols
  ){

    PF_summary_new <-
      proflux_summarise(PROFLUX)

    id_cols <- pf_id_cols(PROFLUX)
    id_cols <- id_cols[id_cols %in% names(PF_summary[[1]])]  %>%
      c("layer")

    print(id_cols)

    df_ret <-
    lapply(names(PF_summary),function(name){
      df_new <- PF_summary_new[[name]]
      df_old <- PF_summary[[name]]

      cols <- names(df_new)[!names(df_new) %in% id_cols]
      print(cols)

      df_old <-
        df_old %>%
        dplyr::rename_with(.cols = cols,
                    .fn = ~paste0(.,"_old"))

      id_cols_tmp <- id_cols[id_cols %in% names(df_new)]

      df_ret <-
      df_new %>%
        dplyr::left_join(df_old,
                         by = id_cols_tmp) %>%
        dplyr::mutate(
          dplyr::across(cols,
            ~.x-
              {get(paste0(cur_column(),"_old"))}
            )) %>%
        dplyr::select(dplyr::any_of(c(id_cols,cols)))

      # get layer into column name if present
      if ("layer" %in% id_cols_tmp){
        df_ret <-
          df_ret %>%
          tidyr::pivot_wider(id_cols = id_cols_tmp[!id_cols_tmp == "layer"],
                             names_from = "layer",
                             values_from = cols )
      }

    df_ret
    }) %>%
      list_full_join()

    stretch_cols <- names(df_ret)[!names(df_ret) %in% param_cols]

    df_ret <-
      df_ret %>%
      tidyr::pivot_longer(cols = stretch_cols,
                          names_to = "MC_var",
                          values_to = "NRMSE")

    df_ret
  }

###### HELPERS ######

list_full_join <- function(l){
  if(length(l) == 1){
    return(l[[1]])
  }

  df1 <- l[[1]]
  df2 <- l[[2]]

  join_by <- names(df1)[names(df1) %in% names(df2)]

  if (length(join_by) == 0){
    join_by <- character()
  }

  l[[2]] <- dplyr::full_join(df1,df2,by = join_by)
  l <- l[-1]
  list_full_join(l)
}




