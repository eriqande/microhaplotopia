#' Remove samples with too much missing data
#'
#' This function removes samples from a data frame that
#' have too much missing data.
#' @param long_genos A dataframe in long format (i.e. tidy format) that samples
#' are to be removed from. This could be the output dataframe from
#' 'filter_raw_microhap_data'
#' @param n_locs The minmum number of loci a sample must be genotyped at to be kept
#' in final dataset. Samples with fewer than "n_locs" loci genotyped will be removed.
#' @export
filter_missing_data <- function(long_genos, n_locs) {

  #insert error check, does long_genos has 2 rows per individual per locus?
#  test_df <- long_genos %>%
#    count(indiv.ID,locus) %>%
#    distinct(n) %>% pull(n)

#  if (!test_df == 2) {
#    stop("input dataframe does not have 2 rows per locus per individual")
#  }

long_genos %>%
    group_by(indiv.ID) %>%
    mutate(n_loci = n_distinct(locus)) %>%
    filter(n_loci >= n_locs) %>%
    select(-n_loci) %>%
    ungroup()

}
