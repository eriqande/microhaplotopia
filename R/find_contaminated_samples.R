#' Find potentially contaminated samples
#'
#' This function identifies genotypes with more than "max_alleles" haplotypes per
#' sample at a locus.  The max_alleles parameter is set to 2 (for diploid individuals)
#' by default, but can be adjusted/edited as needed.  Input for this function is
#' usually output from filter_raw_microhap_data() and used to identify potentially
#' contaminated samples.  However, you can run this function with your raw data if
#' you want to get a sense for presence of low-read-depth bleedthrough in your
#' sequencing run.
#' @param haplo_data The haplotype from a single, or multiple sequencing runs. This
#' could be the output from filter_raw_microhap_data() or read_unfiltered_observed()
#' @param max_alleles The maximum number of alleles you'd expect any samples to have
#' at a locus (set as 2 by default for diploid individuals)
#' @export
find_contaminated_samples <- function(haplo_data, max_alleles = 2) {

  if (!("NAlleles" %in% names(haplo_data))) {
    haplo_data <- haplo_data %>%
    group_by(group, indiv.ID, locus) %>%
    mutate(NAlleles = n()) %>%
    ungroup()}

  haplo_data %>%
    filter(NAlleles > max_alleles) %>%
    arrange(group, indiv.ID, locus)

}
