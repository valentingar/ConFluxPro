#' @title input_classes
#'
#' @description This provides the framework for multiple input data classes that
#' make it easier to set up a ConFluxPro model by checking for data integrity first.
#'
#' @param x sosso
#'
#' @export
# helper
gdat <- function(tbl,
                 id_cols){

  x <- new_gdat(tbl,
                id_cols)

  validate_gdat(x)
}

#'
# constructor
new_gdat <- function(tbl,
                     id_cols){

  structure(tbl,
            class = c("gdat","data.frame"),
            id_cols = id_cols)
}


#'
# validator
validate_gdat <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"gdat"))
  stopifnot(inherits(x,"data.frame"))

  # are the necessary columns present?
  base_cols <- c("NRESULT_ppm","gas","depth")
  id_cols <- pf_id_cols(x)

  stopifnot("data.frame lacks obligatory coluns" = base_cols %in% names(x),
            "id_cols must be present in the data.frame" = id_cols %in% names(x)
  )

  #check for NAs in id_cols
  stopifnot("id_cols cannot contain NAs" =
    anyNA(x[id_cols]) == FALSE)

  #check that at least two depths per group are present
  problem_groups <-
  x %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::summarise(n_depths = length(unique(depth[!is.na(NRESULT_ppm)]))) %>%
    dplyr::filter(n_depths < 2)

  print(problem_groups)
  stopifnot("There are combinations of id_cols with less than 2 non-NA depths" =
              nrow(problem_groups) == 0 )


  x
}



#' @export
#helper
spdat <- function(tbl,
                  id_cols = c()){
  x <- new_spdat(tbl,
            id_cols
            )

  x <- validate_spdat(x)
}

#constructor
new_spdat <- function(tbl,
                      id_cols){
  structure(tbl,
            class = c("spdat","data.frame"),
            id_cols = id_cols)

}

#validator
validate_spdat <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"spdat"))
  stopifnot(inherits(x,"data.frame"))

  # are the necessary columns present?
  base_cols <- c("upper","lower")
  id_cols <- pf_id_cols(x)

  # is the data frame upper/lower consistent?
  stopifnot("The data is not unique and upper/lower consistent!" = is_ul_consistent(x,id_cols))

  x
}




# methods  -------------------
#' @exportS3Method
print.spdat <- function(x){
  cat("A soilphys object (spdat) \n")
  print_id_cols(x)
  NextMethod()
}

#' @exportS3Method
print.gdat <- function(x){
  cat("A gasdata object (gdat) \n")
  print_id_cols(x)
  NextMethod()
}

print_id_cols <- function(x){
  id_cols <- pf_id_cols(x)
  unique_groups <- x[id_cols] %>% distinct() %>% nrow()
  cat("id_cols:", pf_id_cols(x), "\n")
  cat(unique_groups, " unique profiles", "\n")
}



