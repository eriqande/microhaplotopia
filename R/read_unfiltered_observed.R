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
#' @param microhaplot_columns a logical.  Set to TRUE if you are reading output from microhaplot
#' and you can use FALSE if you are reading a file that you made yourself that might have
#' different column types than the microhaplot output.
#' @export
read_unfiltered_observed <- function(datapath, microhaplot_columns = TRUE) {

  if (microhaplot_columns == TRUE) {
    our_col_types <- cols(
      X1 = col_double(),
      group = col_character(),
      indiv.ID = col_character(),
      locus = col_character(),
      haplo = col_character(),
      depth = col_double(),
      allele.balance = col_double(),
      rank = col_double()
    )
    our_col_names <- c(
      "X1",
      "group",
      "indiv.ID",
      "locus",
      "haplo",
      "depth",
      "allele.balance",
      "rank"
    )
  } else {
    our_col_types = NULL
    our_col_names <- TRUE
  }

  if (any(stringr::str_detect(datapath, ".csv$|csv.gz$"))) {
    filepath <- datapath
  } else
    filepath <- fs::dir_ls(datapath, regexp = ".csv$|csv.gz$")

  hap_raw <- vroom::vroom(
    filepath,
    id = "source",
    col_names = our_col_names,
    col_types = our_col_types
  ) %>%
    dplyr::select("source", "group","indiv.ID", "locus", "haplo", "depth", "allele.balance", "rank")

  hap_raw
}
