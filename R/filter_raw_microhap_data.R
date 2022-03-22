#' Filter raw microhaplot output from read_unfiltered_observed().
#'
#' Given a set of filtering criteria on individual haplotype depth, total read depth for
#' the genotype and allelle balance (the ratio of a particular microhaplotype read depth
#' to read depth of the most common microhaplotype for an individual at a locus) this
#' function will filter raw haplotype data. The input file here could be what
#' was loaded into R using the read_unfiltered_observed() function from this package.
#' If you choose to use data from a different source, you column names must be the
#' same as data read in with read_unfiltered_observed() or read_haplot_rds().
#' @param hap_raw dataframe of unfiltered haplotypes. This could be from a single run or a
#'  large number of runs that have been joined together.
#' @param haplotype_depth remove haplotypes with less than depth specified here. This is for
#' removing potential genotyping errors or alleles with depths you don't feel confident about.
#' @param total_depth minimum number of reads required for the genotype to be retained.
#' @param allele_balance Ratio of the number of reads for a given microhaplotype divided
#' by the number of reads of the most common microhaplotype for that individual at that
#' locus.  For example, if you have a heterozygote with 60 reads for the haplotype AA and
#' 100 reads for the haplotype TC, the allele balance for the haplotype AA for that individual
#' at that locus is 0.6 and is 1.0 for TC.
#' @export
filter_raw_microhap_data <- function(hap_raw,
                                     haplotype_depth,
                                     total_depth,
                                     allele_balance) {
  hap_raw %>%
    filter(allele.balance >= allele_balance) %>% #Allele balance filter
    filter(depth >= haplotype_depth) %>% #individual allele depth filter
    group_by(group, indiv.ID, locus) %>%
    mutate(TotalDepth = sum(depth), NAlleles = n()) %>% #Add columns for total depth and number of alleles per locus per individual
    ungroup() %>%
    filter(TotalDepth >= total_depth)
}
