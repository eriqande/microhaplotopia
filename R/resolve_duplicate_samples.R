#' Resolves duplicate samples
#'
#' If a sample was sequenced on multiple sequencing runs (i.e.
#' it's present with multiple 'source' variables) this function
#' identifies the sequencing run with more missing data and removes
#' that sample-by-source combination. This function does not test
#' if the duplicates are genotypically identical.If you want to
#' confirm that they are truly duplicates with identical
#' genotype data go have a crack at 'CKMRsim::find_close_matching_genotypes'.
#' @param haplo_data genetic data output from 'filter_raw_microhap_data'
#' @param resolve Enter "rename" if you wish to keep both runs of this sample and
#' "drop" if you want to drop the run with fewer loci genotyped.  The "rename"
#' option will sort your reruns by their "group" column in your datset and append
#' an "_1" to the first run's name, "_2" to the next, and so on.  The "drop" option
#' will not rename your samples, simply drop the run with fewer loci genotyped--if
#' they are equal, it will keep whichever one appears first in your dataset.
#' @export
resolve_duplicate_samples <- function(haplo_data, resolve) {

  dups <- haplo_data %>%
    distinct(group, indiv.ID) %>%
    group_by(indiv.ID) %>%
    mutate(n_reps = n()) %>%
    filter(n_reps > 1) %>%
    ungroup() %>%
    arrange(indiv.ID) %>%
    dplyr::select(indiv.ID, group)

  if (resolve == "rename"){
    dups <- arrange(dups, group) %>% # Arrange duplicates in descending order by group name
      group_by(indiv.ID) %>%
      mutate(indiv.ID_new = paste(indiv.ID, seq(1:n()), sep = "_")) # Make new indiv.ID for duplicates that's indiv_ID_n where n is 1 for the one with the earliest group, 2 for second earliest and so on

    outp <- left_join(haplo_data, dups) %>% # Add the new indiv.ID name info to your full dataset
      mutate(indiv.ID = if_else(is.na(indiv.ID_new), indiv.ID, indiv.ID_new)) %>% # for individuals with a new indiv.ID, replace the old indiv.ID with new, otherwise keep old
      select(-indiv.ID_new) # drop the column of new indiv.IDs

    outp
  } else if (resolve == "drop"){
    JustDups <- left_join(dups, haplo_data) #Grab genos for just duplicates

    #Find the ones to drop
    DropThese <- JustDups %>%
      group_by(group, indiv.ID) %>%
      summarize(N_Loci = n_distinct(locus)) %>% # Summarize number of loci typed at each rerun
      arrange(desc(N_Loci)) %>% # Arrange from most loci typed to least
      group_by(indiv.ID) %>%
      slice(1) %>% # Keep a list of the runs for each individual with the most loci typed
      select(-N_Loci) %>%
      anti_join(JustDups, .) # Keep genotypes for each individual run that IS NOT the individual run with the most loci typed

    outp <- anti_join(haplo_data, DropThese) # drop the genotypes for the individuals that did not have the most loci typed for the samples that were rerun

    outp
  } else{
    stop("Please enter 'rename' or 'drop' as the resolve parameter input.")
  }

}
