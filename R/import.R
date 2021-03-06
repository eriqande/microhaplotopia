#' @importFrom dplyr arrange case_when ends_with everything filter group_by left_join mutate recode rename select summarize ungroup
#' @importFrom ggplot2 aes facet_wrap geom_label geom_line geom_point ggplot guide_legend scale_colour_manual scale_fill_manual
#' @importFrom lubridate mdy
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom stringr str_c
#' @importFrom tibble tibble
#' @importFrom tidyr gather spread unite
#' @importFrom utils write.table
NULL


# quiets concerns of R CMD check re: the . and other column names
# that appear in dplyr chains
if (getRversion() >= "2.15.1")  {
  utils::globalVariables(
    c(
      "."
    ))
}
