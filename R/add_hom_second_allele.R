#' Add second line to homozygote genotypes.
#'
#' Given a dataset, detect the individuals that have only one line at a locus and
#' add a second line that is a copy of the first with rank set to rank of original
#' copy+1, so you have a minimum of two lines (i.e. alleles) for each individual
#' at each locus.
#' @param hap_raw is the raw (or lightly filtered) haplotypes you wish to add second
#' homozygote lines to
#' @export
add_hom_second_allele <- function(hap_raw) {
  hap_raw %>%
    filter(NAlleles < 2) %>%
    mutate(rank = rank + 1) %>%
    bind_rows(hap_raw)
}
