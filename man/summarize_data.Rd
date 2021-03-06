% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_data.R
\name{summarize_data}
\alias{summarize_data}
\title{Summarize long genotype data.}
\usage{
summarize_data(datafile, group_var)
}
\arguments{
\item{datafile}{dataframe of microhaplotype data in long format. This can be filtered or unfiltered data. For example,
unfiltered data such as hap_raw or filtered data such as long_genos can be used.}

\item{group_var}{Column(s) to summarize input data by. This could be sequencing run,
individual.ID, locus, etc. This must be a quoted column name or a character vector of column names.}
}
\description{
Calculate the number of individuals, runs, loci, etc for a data frame. This function
could be applied to either a raw dataframe or a filtered data frame. A table output is
produced by this function, for graphical output see plot_run_statistics() in this package.
}
