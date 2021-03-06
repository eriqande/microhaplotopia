% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_duplicate_samples.R
\name{find_duplicates}
\alias{find_duplicates}
\title{Identify duplicate samples}
\usage{
find_duplicates(long_genos)
}
\arguments{
\item{long_genos}{genetic data in long format where each sample
has 2 rows per locus. This dataframe could be the output from
'filter_raw_microhap_data'}
}
\description{
This function will identify samples that have identical 'indiv.ID' but
were run on multiple sequencing runs.
}
