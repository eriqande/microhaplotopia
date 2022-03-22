#' Identify duplicate samples
#'
#' This function will identify samples that have identical 'indiv.ID' but
#' were run on multiple sequencing runs (denoted in "group" column of microhaplot
#' outputs).
#' @param long_genos genetic data in long format. This dataframe could be the output
#' from 'filter_raw_microhap_data'
#' @export
find_duplicates <- function(long_genos) {

  dups <- long_genos %>%
    distinct(group, indiv.ID) %>%
    group_by(indiv.ID) %>%
    mutate(n_reps = n()) %>%
    filter(n_reps > 1) %>%
    ungroup() %>%
    arrange(indiv.ID) %>%
    dplyr::select(indiv.ID, group)

  if (nrow(dups) == 0) {
    message("No duplicates present. Congratulations!")
  } else {
    dups
  }

}
