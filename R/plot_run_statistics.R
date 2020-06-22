#' Summarize data files grouped by group_id.
#'
#' Plot summaries of read depth per locus, reads per sample, and haplotypes per
#' locus by sequencing run. This function can be used for raw or filtered data, but
#' we think it would be most useful as a quick check of the filtered data before
#' proceeding to more advanced analyses or data transformations for different programs.
#' @param datafile dataframe of microhaplotype data. This can be filtered or unfiltered data. For example,
#'  unfiltered data such as hap_raw or filtered data such as long_genos can be used.
#'  @param num_runs The number of sequencing runs in the datafile.
#' @export
plot_run_statistics <- function(datafile, num_runs) {

  if (num_runs <= 5){
    num_runs <- 1
  } else {
    num_runs <- round(num_runs/2, 0)
  }

  total_reads_per_locus <- datafile %>%
    mutate(pos2chop = stri_locate_last(source, regex = "(\\/)") %>% .[1],
           pos2end = str_length(source),
           source = str_sub(source, pos2chop, pos2end)) %>%
    dplyr::select(-pos2chop, -pos2end) %>%
    group_by(indiv.ID, locus, source) %>%
    slice(1) %>%
    group_by(locus, source) %>%
    summarise(sum_reads = sum(depth_total)) %>%
    ggplot(., aes(x = fct_reorder(locus, sum_reads), y = sum_reads)) +
    geom_col() +
    scale_x_discrete(name = "locus") +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(. ~ source, nrow = num_runs)

  total_reads_per_indiv <- datafile %>%
    mutate(pos2chop = stri_locate_last(source, regex = "(\\/)") %>% .[1],
           pos2end = str_length(source),
           source = str_sub(source, pos2chop, pos2end)) %>%
    dplyr::select(-pos2chop, -pos2end) %>%
    group_by(indiv.ID, locus, source) %>%
    slice(1) %>%
    group_by(indiv.ID, source) %>%
    summarise(sum_reads = sum(depth_total)) %>%
    ggplot(., aes(x = fct_reorder(indiv.ID, sum_reads), y = sum_reads)) +
    geom_col() +
    scale_x_discrete(name = "individual") +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(. ~ source, nrow = num_runs)

  haps_per_locus <- datafile %>%
    mutate(pos2chop = stri_locate_last(source, regex = "(\\/)") %>% .[1],
           pos2end = str_length(source),
           source = str_sub(source, pos2chop, pos2end)) %>%
    dplyr::select(-pos2chop, -pos2end) %>%
    group_by(locus, source) %>%
    summarise(n_haps = n_distinct(haplo)) %>%
    ggplot(., aes(x = fct_reorder(locus, n_haps), y = n_haps)) +
    geom_point() +
    scale_x_discrete(name = "locus") +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(. ~ source, nrow = num_runs)

  cowplot::plot_grid(total_reads_per_locus, total_reads_per_indiv, haps_per_locus, ncol = 1)

}
