#' Find potentially contaminated samples
#'
#' This function identifies samples that have more than 2 haplotypes per locus
#' that pass filtering on haplotype depth, total depth and allele balance.
#' @param raw_data The raw data from a single, or multiple sequencing runs. This could be the output from
#'  read_unfiltered_observed()
#' @export
find_contaminated_samples <- function(raw_data, haplotype_depth, total_depth, allele_balance) {

  hap_fil1 <- raw_data %>%
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

  homozygotes2add <- hap_fil1 %>% filter(gt_type == "hom") %>% mutate(rank = 2)

  long_genos <- bind_rows(hap_fil1, homozygotes2add) %>%
    group_by(indiv.ID, locus) %>%
    mutate(n_rows = n()) %>%
    ungroup() %>%
    filter(n_rows > 2) %>%
    distinct(source, indiv.ID, locus, .keep_all = TRUE)

  long_genos

}
