#' Calculate number of loci with and without missing data.
#'
#' Calculate the number of loci with missing data and the number
#' of loci with genotype data from a long format dataframe.
#' Both missing and non-missing loci are provided here to allow flexibility for the user.
#' @param long_genos genetic data in long format where each sample has 2 rows per locus.
#' the sample name must be in the 1st column of the data.
#' @export
calculate_missing_data <- function(long_genos) {

  #insert error check, does long_genos has 2 rows per individual per locus?
  test_df <- long_genos %>%
    count(source,indiv.ID,locus) %>%
    distinct(n) %>% pull(n)

  if (!test_df == 2) {
    stop("input dataframe does not have 2 rows per locus per individual")
  }

    n_loci <- long_genos %>% distinct(locus) %>% nrow(.)

    missing_df <- long_genos %>%
      distinct(source,indiv.ID,locus) %>%
      count(indiv.ID) %>%
      mutate(n_miss = n_loci - n) %>%
      rename(n_loci = n)

    missing_df

}
