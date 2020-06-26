#' Remove loci from data frame
#'
#' Removes loci that the user determined were not useful, or
#' do not pass hardy-weinberg equilibrium, or you just don't
#' like them. Just be sure that manuscript reviewers will buy your
#' reason for removing these loci!
#' @param long_genos genetic data
#' @param bad_loci character vector of locus names to remove
#' @export
filter_bad_loci <- function(long_genos, bad_loci) {

  outp <- long_genos %>%
    filter(!locus %in% bad_loci)

  outp
}
