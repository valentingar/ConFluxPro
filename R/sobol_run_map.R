#' @title sobol_run_map
#'
#' @description Modify an existing run_map for sobol indice estimation.
#'
#' @param x An object of class cfp_run_map created by a call to run_map().
#'
#'
#' @export

sobol_run_map <- function(x, ...){
  UseMethod("sobol_run_map")
}

#' @exportS3Method
sobol_run_map.default <- function(x, ...){
  x <- run_map(x, ...)
  .Class <- "cfp_run_map"
  NextMethod()
}

#' @exportS3Method
sobol_run_map.cfp_run_map <- function(x, ...){

  n_runs <- cfp_n_runs(x)
  params_df <- cfp_params_df(x)

  stopifnot("At least 2 runs needed! (Hundred(s) recommended!)" = n_runs > 1)
  stopifnot("choose method = 'random'" = attr(x,"method") == "random")


  runs <- sort(unique(x$run_id))

  if (abs(round(n_runs/2) - n_runs/2 ) > 1e-10){
    message("Uneven n_runs, discarding last.")
    runs <- runs[-n_runs]
  }

  runs_A <- runs[1:(n_runs/2)]
  runs_B <- runs[(1+ n_runs/2):n_runs]

  run_map_A <- x[x$run_id %in% runs_A,] %>%
    dplyr::mutate(run_id_og = run_id,
                  run_id_sobol = run_id,
                  sobol_mat = "A")
  run_map_B <- x[x$run_id %in% runs_B,]%>%
    dplyr::mutate(run_id_og = run_id,
                  run_id_sobol = run_id - n_runs/2,
                  sobol_mat = "B")

  run_map_BA <-
    lapply(params_df$param_id,
           function(i){

             part_B <-
               run_map_B %>%
               dplyr::anti_join(params_df[i, ]) %>%
               dplyr::mutate(param_id = i)

             part_A <-
               params_df[i, ] %>%
               dplyr::left_join(run_map_A)

             out <- dplyr::bind_rows(part_B, part_A)
           }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(sobol_mat = "BA")


  run_map_AB <-
    lapply(params_df$param_id,
           function(i){

             part_A <-
               run_map_A %>%
               dplyr::anti_join(params_df[i, ]) %>%
               dplyr::mutate(param_id = i)

             part_B <-
               params_df[i, ] %>%
               dplyr::left_join(run_map_B)

             out <- dplyr::bind_rows(part_B, part_A)
           }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(sobol_mat = "AB")

  run_map_all <-
    run_map_A %>%
    dplyr::bind_rows(run_map_B)%>%
    dplyr::bind_rows(run_map_BA)%>%
    dplyr::bind_rows(run_map_AB)

  run_ids <-
  run_map_all %>%
    dplyr::select(sobol_mat, param_id, run_id_sobol) %>%
    dplyr::distinct() %>%
    dplyr::mutate(run_id = dplyr::row_number())

  run_map_all <-
  run_map_all %>%
    dplyr::select(!run_id) %>%
    dplyr::left_join(run_ids)

  x <- new_cfp_run_map(run_map_all,
                       id_cols = cfp_id_cols(x),
                       params = attr(x, "params"),
                       method = attr(x, "method"),
                       type = attr(x, "type"),
                       n_runs = max(run_map_all$run_id),
                       layers_different = attr(x, "layers_different"),
                       runmap_type = "sobol",
                       params_df = params_df)

  x
}
