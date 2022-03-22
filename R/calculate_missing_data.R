#' Calculate number of loci with and without missing data.
#'
#' Calculate the number of loci total in the dataset (n_loci), the total number
#' of loci typed in each individual (n_typed), and the difference between the two
#' (n_miss).
#' @param long_genos genetic data in long format (can be filtered, unfiltered, and
#' with or without second homozygote alleles added, but MUST have the column names
#' from microhaplot).
#' @export
calculate_missing_data <- function(long_genos) {
    missing_df <- long_genos %>%
      mutate(n_loci = n_distinct(locus)) %>%
      group_by(group, indiv.ID, n_loci) %>%
      summarize(n_typed = n_distinct(locus)) %>%
      mutate(n_miss = n_loci - n_typed)

    missing_df
}
