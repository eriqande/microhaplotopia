#' Summarize data files grouped by group_var.
#'
#' Calculate the number of individuals, runs, loci, etc for a data file. This function
#' could be applied to either a raw dataframe or a filtered data frame. A table output is
#' produced by this function, for graphical output see plot_run_statistics() in this package.
#'
#' @param datafile dataframe of microhaplotype data. This can be filtered or unfiltered data. For example,
#'  unfiltered data such as hap_raw or filtered data such as long_genos can be used.
#'
#' @param group_var Column(s) to summarize input data by. This could be sequencing run,
#'   individual.ID, locus, etc. This must be a quoted column name or a character vector of column names.
#' @export
summarize_data <- function(datafile, group_var) {

  datafile %>%
    dplyr::group_by_at(vars(group_var)) %>%
    summarise(n_samples = n_distinct(indiv.ID),
              n_loci = n_distinct(locus),
              n_haplotypes = n_distinct(haplo),
              group_ids = paste(unique(group), collapse = ", "),
              mean_depth = mean(depth),
              median_depth = median(depth),
              range_haplodepth = range(depth) %>% paste(collapse = ", ")) %>%
    mutate(pos2chop = ifelse(str_detect(source, pattern = "(\\/)") == TRUE,
                                        stringi::stri_locate_last(source, regex = "(\\/)") %>% .[1], 0),
           pos2end = str_length(source),
           source = str_sub(source, pos2chop, pos2end)) %>%
    dplyr::select(-pos2chop, -pos2end)

}
