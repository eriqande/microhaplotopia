#' Determine which samples did not pass filtering criteria
#'
#' Some samples may have failed completely and they will not be present in the
#' long_genos dataframe produced by filter_raw_microhap_data(). This function is
#' written to identify samples that were present in the raw data, but did not have
#' any loci that passed the filtering criteria. If a sample failed completely and did
#' not have any reads assigned to it, that sample will not be identified by this function.
#' @param raw_data The raw data from a single, or multiple sequencing runs. This could be the output from
#'  read_unfiltered_observed()
#'
 #' @param filtered_data datafile of filtered haplotypes. This is the output from
#'  the "filter_raw_microhap_data" function.
#' @export
find_missing_samples <- function(raw_data, filtered_data) {

  missing_samples <- raw_data %>%
    filter(!indiv.ID %in% filtered_data$indiv.ID) %>%
    distinct(indiv.ID, .keep_all = TRUE) %>%
    dplyr::select(source, indiv.ID, group)

  missing_samples
}
