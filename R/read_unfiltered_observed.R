#' Read unfiltered observed microhaplot output from a directory or file.
#'
#' This function is designed to identify all raw data files within a folder and
#' join them together in R for further data processing. A single file or an entire
#' folder of data files can be read and joined with this function. If a subset of files
#' within a folder is desired, the user should supply the function with a list of file
#' paths.
#' @param datapath path to microhaplot output files. If this is a directory, then all the files within it
#' ending with `.csv` or `.csv.gz` will be
#' processed. If `datapath` is single file with a csv extension then only that file will be loaded. If it is
#' a vector of file paths, at least one of which has a `.csv` or a `.csv.gz` extension, then
#' the function will attempt to load all of the files.
#' @export
read_unfiltered_observed <- function(datapath) {

  if (any(stringr::str_detect(datapath, ".csv$|csv.gz$"))) {
    filepath <- datapath
  } else
    filepath <- fs::dir_ls(datapath, regexp = ".csv$|csv.gz$")

  hap_raw <- vroom::vroom(filepath, id = "source", col_names = TRUE) %>%
    dplyr::select("source", "group","indiv.ID", "locus", "haplo", "depth", "allele.balance", "rank")

  hap_raw
}
