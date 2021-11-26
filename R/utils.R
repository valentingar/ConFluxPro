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
