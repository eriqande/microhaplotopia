#' Filter raw microhaplot output from read_unfiltered_observed().
#'
#' Given a set of filtering criteria on individual haplotype depth, total read depth for
#' the genotype and allelle balance (the ratio of read 2 depth to read 1 depth) this
#' function will filter raw amplicon sequencing data. The input file here could be
#' what was loaded into R using the read_unfiltered_observed() function from this package.
#' @param hap_raw dataframe of unfiltered haplotypes. This could be from a single run or a
#'  large number of runs that have been joined together.
#' @param haplotype_depth remove haplotypes with less than depth specified here. This is for removing what's
#'  considered potential genotyping errors or depths you don't feel confident about.
#' @param total_depth minimum number of reads required for the genotype to be retained.
#' @param allele_balance For heterozygotes only. The ratio of the second haplotype read depth to the first
#'  haplotype read depth. For example, genotype with read depth of 30 and 20, the allele balance is 0.66
#' @param retain_x_haps Argument to specify if the user wants to retain haplotypes with 'X' in them.
#' We've determined the 'X' is likely an indel. The default is for all haplotypes with 'X' in them
#' to be removed. If you want to retain (not filter out) haplotypes with 'X' in them set this argument
#' to TRUE
#' @export
filter_raw_microhap_data <- function(hap_raw,
                                     haplotype_depth,
                                     total_depth,
                                     allele_balance,
                                     retain_x_haps = FALSE) {

  if (retain_x_haps == FALSE) {

    hap_fil1 <- hap_raw %>%
      filter(!str_detect(haplo, "N|X"),
             depth >= haplotype_depth) %>%
      arrange(indiv.ID, locus, desc(depth)) %>%
      group_by(indiv.ID, locus) %>%
      mutate(rank = row_number(),
             allele.balance = depth / depth[1]) %>%
      filter(allele.balance >= allele_balance) %>%
      mutate(gt_type = ifelse(n() > 1, "het", "hom"),
             depth_total = ifelse(gt_type == "het", sum(depth[1], depth[2]), depth)) %>%
      ungroup() %>%
      filter(depth_total >= total_depth)
  } else {
    hap_fil1 <- hap_raw %>%
      filter(!str_detect(haplo, "N"),
             depth >= haplotype_depth) %>%
      arrange(indiv.ID, locus, desc(depth)) %>%
      group_by(indiv.ID, locus) %>%
      mutate(rank = row_number(),
             allele.balance = depth / depth[1]) %>%
      filter(allele.balance >= allele_balance) %>%
      mutate(gt_type = ifelse(n() > 1, "het", "hom"),
             depth_total = ifelse(gt_type == "het", sum(depth[1], depth[2]), depth)) %>%
      ungroup() %>%
      filter(depth_total >= total_depth)
  }

  homozygotes2add <- hap_fil1 %>% filter(gt_type == "hom") %>% mutate(rank = 2)

  long_genos <- bind_rows(hap_fil1, homozygotes2add) %>%
    group_by(source, indiv.ID, locus) %>%
    mutate(n_rows = n()) %>%
    ungroup() %>%
    filter(n_rows == 2) %>%
    dplyr::select(-n_rows) %>%
    arrange(indiv.ID, locus)

  long_genos
}
