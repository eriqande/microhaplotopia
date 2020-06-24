#' Identify duplicate samples
#'
#' This function will identify samples that have identical 'indiv.ID' but
#' were run on multiple sequencing runs.
#' @param long_genos genetic data in long format where each sample
#' has 2 rows per locus. This dataframe could be the output from
#' 'filter_raw_microhap_data'
#' @export
find_duplicates <- function(long_genos) {

  dups <- long_genos %>%
    group_by(indiv.ID, locus) %>%
    mutate(n_reps = n()) %>%
    ungroup() %>%
    distinct(source, indiv.ID) %>%
    arrange(indiv.ID) %>%
    dplyr::select(indiv.ID, source)

  dups
}
