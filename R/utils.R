#' @title utility functions
#'
#' @description A group of functions that help in the internals of the package.
#'


chunk_lapply <- function(X,
                         FUN,
                         fun_process = function(a) {a},
                         n_per_chunk = 1){

  n_chunks <- ceiling(length(X) / n_per_chunk)

  X_runs <- cut(X,
                breaks = seq(0,n_chunks) * n_per_chunk,
                labels = seq(1:n_chunks))

  res <-
    lapply(1:n_chunks,function(i){

      res <- lapply(X[X_runs == i],
                    FUN = FUN)

      fun_process(res)
    })

  sapply(1:n_chunks,
         function(i) res[[i]])

}




# is a data.frame of upper/lower type consistent

is_ul_consistent <- function(df,
                             id_cols){

  stopifnot(all(c("upper","lower") %in% names(df)))
  stopifnot(all(id_cols %in% names(df)))

  any_nas <-
    df %>%
    dplyr::filter(dplyr::if_any(dplyr::any_of(c("upper","lower",id_cols)),
                                is.na)) %>%
    nrow()

  if (any_nas > 0 ){
    return(FALSE)
  }

  any_invalid_rows <-
    df %>%
    dplyr::filter(!(upper > lower)) %>%
    nrow()

  if (any_invalid_rows > 0){
    return(FALSE)
  }


  df <- df %>%
    dplyr::arrange(upper) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols)))

  n_no_fit <-
    df %>%
    dplyr::mutate(is_lowest = lower == min(lower)) %>%
    dplyr::filter(!(lower == lag(upper) |
                    is_lowest)) %>%
    nrow()

  n_no_fit == 0
}



#' @export
uplow <- function(upper,lower, layer = NULL){


  stopifnot(is.numeric(upper),
            is.numeric(lower))
  stopifnot(length(upper) == length(lower),
            length(upper) == length(layer) | is.null(layer))
  stopifnot(all(upper > lower))
  stopifnot(length(unique(upper)) == length(upper))
  stopifnot(length(unique(lower)) == length(lower))

  u_ord <- order(upper)
  lower <- lower[u_ord]
  upper <- upper[u_ord]
  layer <- layer[u_ord]

  x <- new_uplow(upper,lower,layer)

  x <- validate_uplow(x)
}


new_uplow <- function(upper,lower, layer = NULL){

  x <- structure(.Data = upper,
                 class = c("uplow","list"),
                 lower = lower,
                 layer = layer)

}

validate_uplow <- function(x){
  stopifnot(inherits(x,"uplow"))

  upper <- attr(x,"upper")
  lower <- attr(x,"lower")


  x
}

'%within_uplow%' <- function(x,y){

  stopifnot(inherits(x,"uplow"),
            inherits(y,"uplow"))
  stopifnot(length(x) == 1,
            length(y) == 1)

  l_x <- attr(x,"lower")
  l_y <- attr(y,"lower")

  (y >= x & l_y <= l_x)
}


