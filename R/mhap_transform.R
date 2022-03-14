#' Transform long genos to different formats for downstream analyses
#'
#' Takes a long genotype dataframe and transforms it to be the initial format
#' required for different genetic analysis programs.
#' Current options for genetic analysis programs include:
#' CKMRsim, rubias, snppit(?), franz(?), adegenet,
#' structure (?), WHAT ELSE DO PEOPLE COMMONLY USE HERE....
#' The `2columnformat_haplotype` is a standard wide dataframe that has 2 columns
#' per locus with 1 row per sample. Missing data is coded as `NA`
#' The `2columnformat_numeric` has haplotypes coded as integers, it returns a list
#' of dataframes, the first being the 2 column format named 'data', and the second dataframe
#' (named 'key') is the key for haplotype to integer conversion done within the function.
#' The 'adegenet_2col' produces a dataframe to be passed into 'adegenet::df2genind'
#' @param long_genos dataframe of filtered haplotypes. This is the output from
#'  the "filter_raw_microhap_data" function.
#' @param program The program you want to create an input file for. This could be 'CKMRsim',
#' 'rubias', 'franz', '2columnformat_haplotype', '2columnformat_numeric', '2columnformat_ndigit',
#' 'adegenet'ADD OTHERS HERE.
#' @param metadata Not required. If supplied, metadata will be joined to genotype data
#' before the output file is written. For franz, metadata must be in the following order:
#' indiv.ID, birth_year, death_year, sex. Sex is M, F or ? (for unknown).
#' For rubias, metadata is in the following order:
#' indiv.ID, sample_type, repunit, collection (for details see 'vignette("rubias-overview")')
#' @export
mhap_transform <- function(long_genos, program, metadata = NULL) {

  #insert error check, does long_genos has 2 rows per individual per locus?
  test_df <- long_genos %>%
    count(indiv.ID,locus) %>%
    distinct(n) %>% pull(n)

  if (!test_df == 2) {
    stop("input dataframe does not have 2 rows per locus per individual")
  }

  if (program == "CKMRsim") {

    outp_ckmr <- long_genos %>%
      dplyr::select(indiv.ID, locus, rank, haplo) %>%
      rename(Indiv = indiv.ID,
             Locus = locus,
             gene_copy = rank,
             Allele = haplo)

    outp_ckmr

  } else if (program == "rubias") {

    outp_rubias <- long_genos %>%
      dplyr::select(indiv.ID, locus, rank, haplo) %>%
      rename(indiv = indiv.ID) %>%
      unite(tmp, locus,rank) %>%
      spread(tmp, haplo)

    names(outp_rubias) <- c("indiv", str_sub(names(outp_rubias[2:ncol(outp_rubias)]), 1,-3))

    outp_rubias

  } else if (program == "franz" ) {

    meta_gt_check <- df_fil1 %>% mutate(indiv_check = indiv.ID %in% metadata[,1]) %>% distinct(indiv_check)

    if (!meta_gt_check == TRUE) {
      stop("metadata does not have all individuals present in genotype data")
    }

    haplo2numeric <- long_genos %>% distinct(haplo) %>% mutate(haplo_numeric = 1:nrow(.))

    tmp1 <-  long_genos  %>%
      left_join(., haplo2numeric, "haplo") %>%
      dplyr::select(indiv.ID, locus, haplo_numeric, rank) %>%
      unite(tmp, locus, rank) %>%
      spread(tmp, haplo_numeric) %>%
      dplyr::select(indiv.ID, everything())

    tmp1[is.na(tmp1)] <- "?"

    tmp2 <- tmp1 %>% gather(locus, haplo_numeric, -1) %>%
      mutate(locus = str_sub(locus, 1, -3)) %>%
      group_by(indiv.ID, locus) %>%
      mutate(gt = paste(haplo_numeric, collapse = "/")) %>%
      ungroup() %>%
      dplyr::select(-haplo_numeric) %>%
      distinct(indiv.ID, locus, .keep_all = TRUE) %>%
      spread(locus, gt) %>%
      group_by(indiv.ID) %>%
      mutate(n_rep = n()) %>% ungroup() %>%
      dplyr::select(indiv.ID, n_rep, everything()) %>%
      left_join(., metadata, by = c("indiv.ID" = names(metadata[1]))) %>%
      dplyr::select("indiv.ID", "n_rep", "birth_year", "death_year", "sex", everything())

    n_loci <- n_distinct(long_genos$locus)
    n_samples <- n_distinct(tmp2$indiv.ID)

    cat(paste0("1 ", n_loci, " / franzinputdata\n", n_samples, "\n"), file = "franz_input_data.tsv")
    write_tsv(tmp2, path = "franz_input_data.tsv", append = TRUE, col_names = FALSE)

  } else if (program == "2columnformat_haplotype" ) {

    outp <- long_genos %>%
      dplyr::select(indiv.ID, locus, rank, haplo) %>%
      rename(indiv = indiv.ID) %>%
      unite(tmp, locus,rank) %>%
      spread(tmp, haplo)

    outp

  } else if (program == "2columnformat_numeric") {

    haplo2numeric <- long_genos %>% distinct(haplo) %>% mutate(haplo_numeric = 1:nrow(.))

    outp <- long_genos %>%
      dplyr::select(indiv.ID, locus, rank, haplo) %>%
      mutate(rank = paste0("a",rank)) %>%
      left_join(., haplo2numeric, "haplo") %>%
      dplyr::select(-haplo) %>%
      rename(indiv = indiv.ID) %>%
      unite(tmp, locus,rank) %>%
      spread(tmp, haplo_numeric)

    outp_list <- list(outp, haplo2numeric)
    names(outp_list) <- c("data", "key")
    outp_list

  } else if (program == "2columnformat_ndigit") {
    outp <- long_genos %>%
       dplyr::select(indiv.ID, locus, rank, haplo) %>%
       mutate(hap.var = chartr("ACGT", "1234", haplo)) %>%
       select(-haplo) %>%
       rename(indiv = indiv.ID) %>%
       unite(loc_rank, locus, rank) %>%
       spread(loc_rank, hap.var)

    outp

      } else if (program == "adegenet" ) {

    tmp <- long_genos %>%
      dplyr::select(indiv.ID, locus, rank, haplo) %>%
      group_by(indiv.ID, locus) %>%
      mutate(gt = paste(haplo, collapse = ",")) %>%
      dplyr::select(-haplo, -rank) %>%
      distinct(indiv.ID, locus, .keep_all = TRUE) %>%
      spread(locus, gt)  %>% ungroup()

    tmp1 <- tmp %>% dplyr::select(-indiv.ID)

    tmp2 <- as.matrix(tmp1, rownames.value = tmp$indiv.ID)

    outp <- adegenet::df2genind(tmp2, sep = ",")

    outp

  } else {
    stop("program file type not supported in this function, change program to CKMRsim, rubias, franz, etc,
         if you think you supplied the correct program name, check your spelling.")
  }
}
