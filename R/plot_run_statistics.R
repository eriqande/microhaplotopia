#' Summarize data grouped by sequencing run.
#'
#' Plot summaries of read depth per locus, reads per sample, and haplotypes per
#' locus by sequencing run. This function can be used for raw or filtered data, but
#' we think it would be most useful as a quick check of the filtered data before
#' proceeding to more advanced analyses or data transformations for different programs.
#' @param datafile dataframe of microhaplotype data. This can be filtered or unfiltered data. For example,
#' unfiltered data such as hap_raw or filtered data such as long_genos can be used.
#' @param output_summary summary statistic to plot. Choose one of "total_reads_per_locus",
#' "total_reads_per_indiv" or "unique_haps_per_locus". Plotting all 3 summary statistics
#' in a single window is uninterpretable. So look at each summary statistic separately.
#' @export
plot_run_statistics <- function(datafile, output_summary) {

  if (output_summary == "total_reads_per_locus") {
    datafile %>%
      mutate(pos2chop = ifelse(str_detect(source, pattern = "(\\/)") == TRUE,
                               stringi::stri_locate_last(source, regex = "(\\/)") %>% .[1], 0),
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
      facet_wrap(. ~ source, nrow = 1)

  } else if (output_summary == "total_reads_per_indiv") {

    datafile %>%
      mutate(pos2chop = ifelse(str_detect(source, pattern = "(\\/)") == TRUE,
                               stringi::stri_locate_last(source, regex = "(\\/)") %>% .[1], 0),
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
      facet_wrap(. ~ source, nrow = 1)
  } else if (output_summary == "unique_haps_per_locus") {

    datafile %>%
      mutate(pos2chop = ifelse(str_detect(source, pattern = "(\\/)") == TRUE,
                               stringi::stri_locate_last(source, regex = "(\\/)") %>% .[1], 0),
             pos2end = str_length(source),
             source = str_sub(source, pos2chop, pos2end)) %>%
      dplyr::select(-pos2chop, -pos2end) %>%
      group_by(locus, source) %>%
      summarise(n_haps = n_distinct(haplo)) %>%
      ggplot(., aes(x = fct_reorder(locus, n_haps), y = n_haps)) +
      geom_point() +
      scale_x_discrete(name = "locus") +
      theme(axis.text.x = element_text(angle = 90)) +
      facet_wrap(. ~ source, nrow = 1)
  } else {
    stop("check spelling of output_summary argument")
  }
}
