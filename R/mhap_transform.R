#' Transform long genos to different formats for downstream analyses
#'
#' Takes a long genotype dataframe and transforms it to be the initial format
#' required for different genetic analysis programs. Current options for genetic
#' analysis programs include, CKMRsim, rubias, snppit(?), franz(?), adegenet,
#' structure (?), WHAT ELSE DO PEOPLE COMMONLY USE HERE....
#' The `2columnformat` is a standard wide dataframe that has 2 columns per locus
#' with 1 row per sample. Missing data is coded as `NA`
#' @param long_genos dataframe of filtered haplotypes. This is the output from
#'  the "filter_raw_microhap_data" function.
#' @param program The program you want to create an input file for. This could be CKMRsim,
#' rubias, franz, colony, 2columnformat, ADD OTHERS HERE.
#' @export
mhap_transform <- function(long_genos, program) {

  if(program == "CKMRsim") {
    STATMENT
  } else if (program == "rubias") {
    STATEMENT
  } else if (program == "franz" ) {
    STATEMENT
  } else if (program == "colony" ) {
    STATEMENT
  } else if (program == "2columnformat" ) {
    STATEMENT
  } else {
    stop("program file type not supported in this function, change program to CKMRsim, rubias, franz, etc,
         if you think you supplied the correct program name, check your spelling.")
  }

}
