#' Find alleles with Ns or Xs in them
#'
#' This function identifies lines in your dataset where the haplotype contains an
#' N or X.  N signifies a base that could not be called confidently and X notes
#' that the target base is deleted compared to the reference (this could be a point
#' deletion or a longer deletion that spans your target base).  An excess of Ns
#' on the ends of haplotypes can be an indicator of degraded samples, while Ns in
#' the middle of haplotypes can indicate something weird in your read processing
#' (we've found this usually means we need to adjust flash parameters).  An excess
#' of Xs can indicate a mis-ID'd species.  If across a whole run and in no consistent
#' spots in a haplotype across loci and samples, lots of Xs can be a sign of a bad
#' miseq run (although your run would have to be REALLY bad for this to happen).
#' @param haplo_data The raw or processed data from a single, or multiple sequencing runs.
#' This could be the output from read_unfiltered_observed() or filter_raw_microhap_data().
#' @param NX Indicates if you're interested in alleles with Ns ("N"), Xs ("X"), or both
#' ("NX").  Is set to "NX" by default.
#' @export
find_NXAlleles <- function(haplo_data, NX = "NX") {
  # check for rea
  if (NX == "NX"){
    haplo_data %>%
      filter(grepl("N|X", haplo))
  } else if (NX == "N") {
    haplo_data %>%
      filter(grepl("N", haplo))
  } else if (NX == "X") {
    haplo_data %>%
      filter(grepl("X", haplo))
  } else {
    stop("Please enter 'N', 'X', or 'NX' as NX parameter input.")
  }
}
