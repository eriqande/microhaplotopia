#' Transform long genos to different formats for other analysis programs
#'
#' Takes a long genotype dataframe and transforms it to be the initial format
#' required for different genetic analysis programs. Current options for genetic
#' analysis programs include, CKMRsim, rubias, snppit(?), franz(?), adegenet,
#' structure (?), WHAT ELSE DO PEOPLE COMMONLY USE HERE....
#' @param long_genos datafile of filtered haplotypes. This is the output from
#'  the "filter_raw_microhap_data" function.
#'
#' @param program The program you want to create an input file for. This could be CKMRsim, rubias,
#'  franz, ADD OTHERS HERE.
#' @export
mhap_transform <- function(long_genos, program) {
  if(program == "CKMRsim") {
    STATMENT
  } else if (program == "rubias") {
    STATEMENT
  } else if (filetype %in% c("csv","txt","tsv") ) {
    STATEMENT
  } else
    stop("program file type not supported in this function, change program to CKMRsim, rubias, franz, etc,
         if you think you supplied the correct program name, check your spelling.")

}
