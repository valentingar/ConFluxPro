

#' @export
#helper
cfp_soilphys <- function(soilphys,
                         id_cols){


  stopifnot("id_cols must be provided!" = !missing(id_cols))

  if (!"gas" %in% id_cols){
    message("added 'gas' to id_cols")
    id_cols <- c(id_cols,"gas")
  }

  x <- new_cfp_soilphys(soilphys,
                        id_cols
  )

  x <- validate_cfp_soilphys(x)
}

#constructor
new_cfp_soilphys <- function(soilphys,
                             id_cols){
  x <- structure(soilphys,
                 class = c("cfp_soilphys","data.frame"),
                 id_cols = id_cols)
  x
}

#validator
validate_cfp_soilphys <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"cfp_soilphys"))
  stopifnot(inherits(x,"data.frame"))

  # are the necessary columns present?
  base_cols <- c("upper","lower","DS","rho_air","gas")
  id_cols <- cfp_id_cols(x)

  stopifnot("data.frame lacks obligatory coluns" = base_cols %in% names(x),
            "id_cols must be present in the data.frame" = id_cols %in% names(x)
  )

  # is the data frame upper/lower consistent?
  stopifnot("The data is not unique and upper/lower consistent!" = is_ul_consistent(x,id_cols))

  x
}
