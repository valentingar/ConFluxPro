#' @title Filter profiles
#'
#' @description
#' Filter profiles by their `id_cols` or (where available) by their `prof_id`.
#' This is built on [dplyr::filter()].
#'
#' @param .data A [cfp_dat()] object or its derivatives.
#'
#' @inheritParams dplyr::filter
#'
#' @returns A subset of the original data.
#'
#' @examples
#' base_dat |>
#'   filter(site == "site_a")
#'
#' base_dat |>
#'   filter(Date > "2022-03-01")
#'
#' @rdname filter
#' @export
filter.cfp_dat <- function(.data,
                           ...,
                           .preserve = FALSE){
  tables <- names(.data)
  tables <- tables[tables == "profiles"]

  .data$profiles <- .data$profiles %>%
    dplyr::filter(...)

  possible_cols <- names(.data$profiles)

  out <-
    lapply(.data, function(t){

      col_names <- names(t)
      merger <- col_names[col_names %in% possible_cols]
      deselector <- possible_cols[!possible_cols %in% merger]

      t_new <-
        t %>%
        dplyr::right_join(.data$profiles %>%
                            dplyr::select({merger}) %>%
                            dplyr::distinct(),
                          by = merger) %>%
        dplyr::select(!dplyr::any_of(deselector))

      old_atr <- attributes(t)
      new_atr <- old_atr[!names(old_atr) %in% names(attributes(data.frame()))]
      attributes(t_new) <- c(attributes(t_new), new_atr)
      class(t_new) <- class(t)
      t_new
    })

  attributes(out) <- attributes(.data)

  out
}
