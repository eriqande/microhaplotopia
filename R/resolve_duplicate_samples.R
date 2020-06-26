#' Resolves duplicate samples
#'
#' If a sample was sequenced on multiple sequencing runs (i.e.
#' it's present with multiple 'source' variables) this function
#' identifies the sequencing run with more missing data and removes
#' that sample-by-source combination. This function does not test
#' if the duplicates are genotypically identical.If you want to
#' confirm that they are truly duplicates with identical
#' genotype data go have a crack at 'CKMRsim::find_close_matching_genotypes'.
#' @param long_genos genetic data in long format where each sample
#' has 2 rows per locus. This dataframe could be the output from
#' 'filter_raw_microhap_data'
#' @export
resolve_duplicate_samples <- function(long_genos) {

  dups <- long_genos %>%
    distinct(source, indiv.ID) %>%
    group_by(indiv.ID) %>%
    mutate(n_reps = n()) %>%
    filter(n_reps > 1) %>%
    ungroup() %>%
    arrange(indiv.ID) %>%
    dplyr::select(indiv.ID, source) %>%
    mutate(id2join = paste0(indiv.ID, "-", source)) %>%
    pull(indiv.ID)

  n_loci <- long_genos %>% distinct(locus) %>% nrow(.)

  dups2chuck <- long_genos %>%
    filter(indiv.ID %in% dups) %>%
    distinct(source, indiv.ID, locus) %>%
    count(indiv.ID, source) %>%
    mutate(n_miss = n_loci - n) %>%
    rename(n_loci = n) %>%
    group_by(indiv.ID) %>%
    arrange(desc(n_miss)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(id2join = paste0(indiv.ID, "-", source)) %>%
    pull(id2join)

  outp <- long_genos %>%
    mutate(id2join = paste0(indiv.ID, "-", source)) %>%
    filter(!id2join %in% dups2chuck) %>%
    dplyr::select(-id2join)

  outp

}
