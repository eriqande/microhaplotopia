#' Read unfiltered observed microhaplot output from a directory or file.
#'
#' blah blah blah long descrip here
#' @param datapath path to microhaplot output files. If a directory, then all csv files within it are
#' processed. If a single file, with a csv extension then only that file will be loaded.
#' @export
read_unfiltered_observed <- function(datapath) {

  filepath <- fs::dir_ls(datapath, regexp = "\\.csv$|\\csv.gz$")

  hap_raw <- vroom::vroom(filepath, id = "source", col_names = TRUE) %>%
    dplyr::select("source", "group","indiv.ID", "locus", "haplo", "depth", "allele.balance", "rank")

  hap_raw
}
