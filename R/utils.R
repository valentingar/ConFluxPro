#' @importFrom rlang .data
# internal functions only



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
# i.e. no gaps, no duplicates, no overlap of steps per profile
# profile identified by id_cols
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
    dplyr::arrange(.data$upper) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols)))

  n_no_fit <-
    df %>%
    dplyr::mutate(is_lowest = (.data$lower == min(.data$lower))) %>%
    dplyr::filter(!(lower == dplyr::lag(.data$upper) |
                    .data$is_lowest)) %>%
    nrow()

  n_no_fit == 0
}


# reduce size of cfp_dat to necessary only
trim_cfp_dat <- function(x){

  x$soilphys <- x$soilphys[,c(
                                "upper",
                                "lower",
                                "depth",
                                "height",
                                "sp_id",
                                "pmap",
                                "step_id",
                                "DS",
                                "c_air")]
  x$gasdata <- x$gasdata[,c(
                                  "gd_id",
                                  "depth",
                                  "x_ppm")]
  x$layers_map <- x$layers_map[,c(
                                  "group_id",
                                  "pmap",
                                  "layer",
                                  "layer_couple",
                                  "highlim",
                                  "lowlim",
                                  "upper",
                                  "lower")]
  x

}




# For printing methods
print_id_cols <- function(x){
  id_cols <- cfp_id_cols(x)
  unique_groups <- x[id_cols] %>% dplyr::distinct() %>% nrow()
  cat("id_cols:", id_cols, "\n")
  cat(unique_groups, " unique profiles", "\n")
}


#linear extrapolation with single target value
lin_extrap<-function(x,
                     y,
                     x_new){
  if(!length(x)==2 | !length(y)==2){
    stop("length of x and y must be 2!")
  }
  y_new <- (x_new-x[1])*(diff(y) / diff(x)) + y[1]
  return(y_new)
}


# any negatives

any_negative_values <- function(x){
  x <- x[is.finite(x)]
  if (length(x) == 0) return(FALSE)
  min(x) < 0
}


## sample function that can handle 1 length vectors
sample.vec <- function(x, ...) x[sample(length(x), ...)]

# 1:nrow alternative
seq_nrow <- function(x){
  n_rows <- nrow(x)
  if(n_rows < 1) return(integer())
  1:n_rows
}
