#' Read sample sheet information from Illumina sequencing run(s)
#'
#' This function is designed to identify all summary data files within a folder and
#' join them together in R for further data processing. A single file or an entire
#' folder of data files can be read and joined with this function. If a subset of files
#' within a folder is desired, the user should supply the function with a list of file
#' paths.
#' @param datapath path to Illumina Sample sheet files that are loaded onto sequencers for
#' demultiplexing. If this is a directory, then all the files within it
#' ending with `.csv` or `.csv.gz` will be processed.
#' If `datapath` is single file with a csv extension then only that file will be loaded.
#' If it is a vector of file paths, at least one of which has a `.csv` or a `.csv.gz` extension, then
#' the function will attempt to load all of the files.
#' @param n_skip is the number of lines to skip. The sample sheets loaded onto Illumina
#' sequencing machines have a header of 'n_skip' lines above the useful metadata. Open
#' one of the SampleSheet files that is in your datapath folder and identify the
#' appropriate number of lines to skip. The first line read should be column names of
#' the metadata. For example at the NOAA SWFSC the first columns of metadata are:
#' Sample_ID, Sample_Name, Sample_Plate...etc.
#' @export
read_samplesheets <- function(datapath, n_skip) {

  if (any(stringr::str_detect(datapath, "\\.csv$|\\csv.gz$"))) {
    filepath <- datapath
  } else
    filepath <- fs::dir_ls(datapath, regexp = "\\.csv$|\\csv.gz$")

  meta <- vroom::vroom(filepath, id = "source", col_names = TRUE, skip = n_skip)

  meta
}
