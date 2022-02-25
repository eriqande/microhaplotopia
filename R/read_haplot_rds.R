#' Read raw microhaplot rds files from a directory or file.
#'
#' This function is designed to identify all raw rds data files within a folder and
#' join them together in R for further data processing. A single file or an entire
#' folder of data files can be read and joined with this function. If a subset of files
#' within a folder is desired, the user should supply the function with a list of file
#' paths.
#' @param datapath path to microhaplot rds files. If this is a directory, then all the files within it
#' ending with `.rds` but not '_posinfo.rds' will be processed. If `datapath` is single file with an
#' rds extension then only that file will be loaded. If it is  a vector of file paths, at least one of
#' which has a `.rds` extension, then the function will attempt to load all of the files.
#' @export
read_haplot_rds <- function(datapath) {

  if (any(stringr::str_detect(datapath, ".rds$"))) {
    filepath <- datapath
  } else
    filepath <- filepath <- filepath <- fs::dir_ls(datapath, regexp = "\\.rds$") %>%
      .[stringr::str_detect(., "_posinfo", negate = TRUE)]

  Genos <- purr::map_dfr(filepath, read_rds, .id = "source") %>%
    rename(indiv.ID = id) %>%
    select(source, group, indiv.ID, locus, haplo, depth, allele.balance, rank) %>%
    filter(!is.na(indiv.ID)) %>%
    mutate(source = as.character(source),
           group = as.character(group),
           indiv.ID = as.character(indiv.ID),
           locus = as.character(locus),
           haplo = as.character(haplo),
           depth = as.double(depth),
           allele.balance = as.double(allele.balance),
           rank = as.double(rank))

  Genos
}
