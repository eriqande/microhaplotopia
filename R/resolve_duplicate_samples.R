#' Resolves duplicate samples
#'
#' This function will resolve duplicate samples (same indiv.ID)
#' by removing the haplotype data for the run that had more missing data.
#' If you want to confirm that they are truly duplicates with identical
#' genotype data go have a crack at 'CKMRsim::find_close_matching_genotypes'.
#' This function assumes that the duplicate fish have identical genotypes.
#' @param long_genos genetic data in long format where each sample
#' has 2 rows per locus. This dataframe could be the output from
#' 'filter_raw_microhap_data'
#' @export
resolve_duplicate_samples <- function(long_genos) {

  dups <- long_genos %>%
    group_by(indiv.ID, locus) %>%
    mutate(n_reps = n()) %>%
    ungroup() %>%
    filter(n_reps > 2) %>%
    distinct(source, indiv.ID) %>%
    arrange(indiv.ID) %>%
    dplyr::select(indiv.ID, source) %>%
    mutate(id2join = paste0(indiv.ID, "-", source))

  n_loci <- long_genos %>% distinct(locus) %>% nrow(.)

  dups2chuck <- long_genos %>%
    distinct(source, indiv.ID, locus) %>%
    count(indiv.ID, source) %>%
    mutate(n_miss = n_loci - n) %>%
    rename(n_loci = n) %>%
    filter(indiv.ID %in% dups$indiv.ID) %>%
    group_by(indiv.ID) %>%
    arrange(desc(n_miss)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(id2join = paste0(indiv.ID, "-", source))

  outp <- long_genos %>%
    mutate(id2join = paste0(indiv.ID, "-", source)) %>%
    filter(!id2join %in% dups2chuck)

}
