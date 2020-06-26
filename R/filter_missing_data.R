#' Remove samples with too much missing data
#'
#' This function removes samples from a data frame that
#' have too much missing data.
#' @param long_genos A dataframe in long format (i.e. tidy format) that samples
#' are to be removed from. This could be the output dataframe from
#' 'filter_raw_microhap_data'
#' @param n_miss The maximum number of loci with missing data for a sample
#' to be retained. Samples with more than 'n_miss' loci with missing
#' data are removed.
#' @export
filter_missing_data <- function(long_genos, n_miss) {

  #insert error check, does long_genos has 2 rows per individual per locus?
  test_df <- long_genos %>%
    count(indiv.ID,locus) %>%
    distinct(n) %>% pull(n)

  if (!test_df == 2) {
    stop("input dataframe does not have 2 rows per locus per individual")
  }

  n_loci <- long_genos %>% distinct(locus) %>% nrow(.)

  missing_df <- long_genos %>%
    distinct(indiv.ID,locus) %>%
    count(indiv.ID) %>%
    mutate(n_locimiss = n_loci - n) %>%
    rename(n_loci = n)

  samps2remove <- missing_df %>% filter(n_locimiss > n_miss) %>% pull(indiv.ID)

  outp <- long_genos %>%
    filter(!indiv.ID %in% samps2remove)

  outp

}
