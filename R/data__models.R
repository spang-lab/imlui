#' @name nordmo_2020_os_signature
#' @title miRNA Overall Survival signature for DLBCL patients
#' @description The overall survival signature described in Nordmo, Carmen,
#' Gunther Glehr, Michael Altenbuchinger, Rainer Spang, Marita Ziepert, Heike
#' Horn, Annette M. Staiger, et al. "Identification of a MiRNA Based Model to
#' Detect Prognostic Subgroups in Patients with Aggressive B-Cell Lymphoma".
#' Leukemia & Lymphoma 62, no. 5 (16 April 2021): 1107-15.
#' https://doi.org/10.1080/10428194.2020.1861268.
#' @format A named vector of length 6.
#' @examples
#' nordmo_os_signature = c(
#'     "hsa_miR-106b-5p" = +0.189710,
#'     "hsa_miR-130a-3p" = -0.233681,
#'     "hsa_miR-186-5p"  = +0.069890,
#'     "hsa_miR-374b-5p" = +0.003091,
#'     "hsa_miR-423-5p"  = -0.069890,
#'     "hsa_miR-590-5p"  = +0.040880
#' )
#' @source \url{https://doi.org/10.1080/10428194.2020.1861268}
utils::globalVariables("nordmo_2020_os_signature")
data__nordmo_2020_os_signature <- function() {
  # Description nordmo_2020::
  #   2 miRNA signatures predictive for OS and PFS in aBCL
  # Model Formula:
  #   Survival ~
  #     Cox(lasso(log2(miRNA))
  #     + IPI-features
  #     + Rituximab-Yes-No
  #     + NSNC-assay-verison
  # Notes:
  #   Samples are from RICOVER-60 and MegaCHOEP trials.
  #   Measurements done with Nanostring nCounter System on FFPE tissue of aBCL
  #   biopsies.
  # Signature below copied from:
  #   https://www.tandfonline.com/doi/suppl/10.1080/10428194.2020.1861268/suppl_file/ilal_a_1861268_sm6104.docx
  # scoreOS ==
  #     log2(miR-130a-3p / miR-106b-5p) * (-0.18971) +
  #     log2(miR-423-5p  / miR-186-5p ) * (-0.06989) +
  #     log2(miR-130a-3p / miR-590-5p ) * (-0.04088) +
  #     log2(miR-374b-5p / miR-130a-3p) * (0.003091)
  #   ==
  #     + log2(miR-130a-3p) * (-0.18971)  # write log(a/b) as log(a) - log(b)
  #     - log2(miR-106b-5p) * (-0.18971)
  #     + log2(miR-423-5p ) * (-0.06989)
  #     - log2(miR-186-5p ) * (-0.06989)
  #     + log2(miR-130a-3p) * (-0.04088)
  #     - log2(miR-590-5p ) * (-0.04088)
  #     + log2(miR-374b-5p) * (0.003091)
  #     - log2(miR-130a-3p) * (0.003091)
  #   ==
  #     + log2(miR-130a-3p) * (-0.18971)  # -*- == +*+, -*+ == +*-
  #     + log2(miR-106b-5p) * (+0.18971)
  #     + log2(miR-423-5p ) * (-0.06989)
  #     + log2(miR-186-5p ) * (+0.06989)
  #     + log2(miR-130a-3p) * (-0.04088)
  #     + log2(miR-590-5p ) * (+0.04088)
  #     + log2(miR-374b-5p) * (+0.003091)
  #     + log2(miR-130a-3p) * (-0.003091)
  #   ==
  #     + log2(miR-106b-5p) * (+0.18971)  # sort
  #     + log2(miR-130a-3p) * (-0.003091) <-- same feature
  #     + log2(miR-130a-3p) * (-0.04088)  <-- same feature
  #     + log2(miR-130a-3p) * (-0.18971)  <-- same feature
  #     + log2(miR-186-5p ) * (+0.06989)
  #     + log2(miR-374b-5p) * (+0.003091)
  #     + log2(miR-423-5p ) * (-0.06989)
  #     + log2(miR-590-5p ) * (+0.04088)
  #   ==
  #     + log2(miR-106b-5p) * (+0.18971)  # sum up
  #     + log2(miR-130a-3p) * (-0.233681)
  #     + log2(miR-186-5p ) * (+0.06989)
  #     + log2(miR-374b-5p) * (+0.003091)
  #     + log2(miR-423-5p ) * (-0.06989)
  #     + log2(miR-590-5p ) * (+0.04088)
  nordmo_2020_os_signature = c(
      "hsa-miR-106b-5p" = +0.189710,
      "hsa-miR-130a-3p" = -0.233681,
      "hsa-miR-186-5p"  = +0.069890,
      "hsa-miR-374b-5p" = +0.003091,
      "hsa-miR-423-5p"  = -0.069890,
      "hsa-miR-590-5p"  = +0.040880
  )
  usethis::use_data(nordmo_2020_os_signature, overwrite = TRUE)
}


#' @name nordmo_2020_pfs_signature
#' @title miRNA Progression Free Survival signature for DLBCL patients
#' @description The progression free survival signature described in Nordmo,
#' Carmen, Gunther Glehr, Michael Altenbuchinger, Rainer Spang, Marita Ziepert,
#' Heike Horn, Annette M. Staiger, et al. "Identification of a MiRNA Based
#' Model to Detect Prognostic Subgroups in Patients with Aggressive B-Cell
#' Lymphoma". Leukemia & Lymphoma 62, no. 5 (16 April 2021): 1107-15.
#' https://doi.org/10.1080/10428194.2020.1861268.
#' @format A named vector of length 6.
#' @examples
#' nordmo_pfs_signature = c(
#'     "miR-106b-5p" = +0.967200,
#'     "miR-130a-3p" = -0.153873,
#'     "miR-365a-3p" = -0.945600,
#'     "miR-374a-5p" = +0.098353,
#'     "miR-423-5p"  = -0.138210,
#'     "miR-590-5p"  = +0.172130
#' )
#' @source \url{https://doi.org/10.1080/10428194.2020.1861268}
utils::globalVariables("nordmo_2020_pfs_signature")
data__nordmo_2020_pfs_signature <- function() {
  # Signature below copied from:
  # https://www.tandfonline.com/doi/suppl/10.1080/10428194.2020.1861268/suppl_file/ilal_a_1861268_sm6104.docx
  # scorePFS =
  #     + log2 (miR-423-5p  / miR-590-5p ) * (-0.13821)
  #     + log2 (miR-365a-3p / miR-106b-5p) * (-0.9456)
  #     + log2 (miR-130a-3p / miR-590-5p ) * (-0.03392)
  #     + log2 (miR-130a-3p / miR-106b-5p) * (-0.0216)
  #     + log2 (miR-374a-5p / miR-130a-3p) * (0.098353)
  # ==  write log(a/b) as log(a) - log(b)
  #     + log2(miR-423-5p ) * (-0.13821) - log2(miR-590-5p ) * (-0.13821)
  #     + log2(miR-365a-3p) * (-0.9456 ) - log2(miR-106b-5p) * (-0.9456 )
  #     + log2(miR-130a-3p) * (-0.03392) - log2(miR-590-5p ) * (-0.03392)
  #     + log2(miR-130a-3p) * (-0.0216 ) - log2(miR-106b-5p) * (-0.0216 )
  #     + log2(miR-374a-5p) * (0.098353) - log2(miR-130a-3p) * (0.098353)
  # == # -*- == +*+, -*+ == +*-
  #     + log2(miR-423-5p ) * (-0.13821)
  #     + log2(miR-590-5p ) * (+0.13821)
  #     + log2(miR-365a-3p) * (-0.9456 )
  #     + log2(miR-106b-5p) * (+0.9456 )
  #     + log2(miR-130a-3p) * (-0.03392)
  #     + log2(miR-590-5p ) * (+0.03392)
  #     + log2(miR-130a-3p) * (-0.0216 )
  #     + log2(miR-106b-5p) * (+0.0216 )
  #     + log2(miR-374a-5p) * (+0.098353)
  #     + log2(miR-130a-3p) * (-0.098353)
  # == # sort
  #     + log2(miR-106b-5p) * (+0.0216  ) <--A
  #     + log2(miR-106b-5p) * (+0.9456  ) <--A
  #     + log2(miR-130a-3p) * (-0.0216  ) <--B
  #     + log2(miR-130a-3p) * (-0.03392 ) <--B
  #     + log2(miR-130a-3p) * (-0.098353) <--B
  #     + log2(miR-365a-3p) * (-0.9456  )
  #     + log2(miR-374a-5p) * (+0.098353)
  #     + log2(miR-423-5p ) * (-0.13821 )
  #     + log2(miR-590-5p ) * (+0.03392 ) <--C
  #     + log2(miR-590-5p ) * (+0.13821 ) <--C
  # == # sum up
  #     + log2(miR-106b-5p) * (+0.967200) <--A
  #     + log2(miR-130a-3p) * (-0.153873) <--B
  #     + log2(miR-365a-3p) * (-0.945600)
  #     + log2(miR-374a-5p) * (+0.098353)
  #     + log2(miR-423-5p ) * (-0.138210)
  #     + log2(miR-590-5p ) * (+0.172130) <--C
  nordmo_2020_pfs_signature = c(
      "hsa-miR-106b-5p" = +0.967200,
      "hsa-miR-130a-3p" = -0.153873,
      "hsa-miR-365a-3p" = -0.945600,
      "hsa-miR-374a-5p" = +0.098353,
      "hsa-miR-423-5p"  = -0.138210,
      "hsa-miR-590-5p"  = +0.172130
  )
  usethis::use_data(nordmo_2020_pfs_signature, overwrite = TRUE)
}


#' @name reinders_2020_coo_signature
#' @title Protein COO signature for DLBCL patients
#' @description The COO signature described in *Reinders, Jörg, et al.
#' "Platform independent protein-based cell-of-origin subtyping of diffuse
#' large B-cell lymphoma in formalin-fixed paraffin-embedded tissue."
#' Scientific Reports 10.1 (2020): 1-11.*. For details see
#' [https://doi.org/10.1038/s41598-020-64212-z](https://doi.org/10.1038/s41598-020-64212-z).
#' The exact signature beta values were copied from [Table
#' 1](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7217957/table/Tab1/?report=objectonly)
#' of the paper.
#' @format A named vector of length 9. See example for the full vector.
#' @examples
#' reinders_2020_coo_signature <- c(
#'     "sp|P04233|HG2A_HUMAN" = 4.489047570,
#'     "sp|Q15063|POSTN_HUMAN" = 1.551003753,
#'     "sp|P51884|LUM_HUMAN" = 1.490571730,
#'     "sp|Q9NS69|TOM22_HUMAN" = 1.019845999,
#'     "sp|P62841|RPS15_HUMAN" = -0.888995137,
#'     "sp|P16070|CD44_HUMAN" = -1.624465232,
#'     "sp|P01871|IGHM_HUMAN" = -2.556118873,
#'     "sp|P18031|PTN1_HUMAN" = -3.480889810,
#'     "Intercept" = -1.25855429
#' )
#' @source \url{https://doi.org/10.1080/10428194.2020.1861268}
utils::globalVariables("reinders_2020_coo_signature")
data__reinders_2020_coo_signature <- function() {
  # Description Reinder 2020:
  #   8 protein betas `reinders_2020_COO_sig`
  #   First Swath-MS DLBCL COO protein signature
  #   Measurement done on FFPE tissue of DLBCL biopsies.
  # Signature below copied from Table 1 of paper
  #   "Platform independent protein-based cell-of-origin subtyping of diffuse
  #   large B-cell lymphoma in formalin-fixed paraffin-embedded tissue" from
  #   Joerg Reinders et al.
  reinders_2020_coo_signature <- c(
      "sp|P04233|HG2A_HUMAN"=4.489047570,
      "sp|Q15063|POSTN_HUMAN"=1.551003753,
      "sp|P51884|LUM_HUMAN"=1.490571730,
      "sp|Q9NS69|TOM22_HUMAN"=1.019845999,
      "sp|P62841|RPS15_HUMAN"=-0.888995137,
      "sp|P16070|CD44_HUMAN"=-1.624465232,
      "sp|P01871|IGHM_HUMAN"=-2.556118873,
      "sp|P18031|PTN1_HUMAN"=-3.480889810,
      "Intercept"=-1.25855429
  )
  usethis::use_data(reinders_2020_coo_signature, overwrite = TRUE)
}


#' @name staiger_2020_survival_signature_lamis
#' @title The LAMIS signature
#' @description The LAMIS signature as described in *Staiger, A.M.,
#' Altenbuchinger, M., Ziepert, M. et al. A novel lymphoma-associated
#' macrophage interaction signature (LAMIS) provides robust risk
#' prognostication in diffuse large B-cell lymphoma clinical trial cohorts of
#' the DSHNHL. Leukemia 34, 543-552 (2020)*. For details see
#' [https://doi.org/10.1038/s41375-019-0573-y](https://doi.org/10.1038/s41375-019-0573-y).
#' The beta values were taken for Supplemnatry [Table
#' S2](https://static-content.springer.com/esm/art%3A10.1038%2Fs41375-019-0573-y/MediaObjects/41375_2019_573_MOESM3_ESM.docx)
#' of the publication.
#' @format A named vector of length 17.
#' @examples \dontrun{
#' round(staiger_2020_survival_signature_lamis, 3)
#' # BCL2A1    BSG   CCL5  CCND2  CD163   CD47  CPNE3  CPT1A   CSF1  CXCR4 DPYSL3
#' # -0.134  0.089 -0.146  0.040  0.155  0.079 -0.031  0.685 -0.685  0.122 -0.049
#' #    FYN   IL16    MME   MPST  PSAT1    TBP
#' # -0.058 -0.065 -0.089  0.050  0.005  0.031
#' }
#' @source \url{https://www.nature.com/articles/s41375-019-0573-y/}
utils::globalVariables("staiger_2020_survival_signature_lamis")
data__staiger_2020_survival_signature_lamis <- function() {
  staiger_2020_survival_signature_lamis <- c(
    BCL2A1 = -0.134235982788409,
    BSG = 0.0887843836592496,
    CCL5 = -0.146116306603617,
    CCND2 = 0.0403534316872738,
    CD163 = 0.154535048218656,
    CD47 = 0.0786604535409905,
    CPNE3 = -0.0306004878389403,
    CPT1A = 0.685413919678888,
    CSF1 = -0.685413919678888,
    CXCR4 = 0.122383294269516,
    DPYSL3 = -0.0487721733023127,
    FYN = -0.0575989931097156,
    IL16 = -0.0647843011597999,
    MME = -0.0887843836592496,
    MPST = 0.0502093802494925,
    PSAT1 = 0.00536614899792646,
    TBP = 0.0306004878389403
  )
  usethis::use_data(staiger_2020_survival_signature_lamis, overwrite = TRUE)
}


#' @name ipi
#' @title The International Prognostic Index (IPI)
#' @description The International Prognostic Index (IPI) as described in
#' *A predictive model for aggressive non-Hodgkin's lymphoma. The International
#' Non-Hodgkin's Lymphoma Prognostic Factors Project. N Engl J Med
#' 1993;329(14):987-94*.
#'
#' The IPI works by assigning one point for each the following factors:
#'
#' * Age greater than 60 years
#' * Stage III or IV disease
#' * Elevated serum LDH
#' * ECOG/Zubrod performance status of 2, 3, or 4
#' * More than 1 extranodal site
#'
#' The sum of points can be used to separate samples into following risk groups:
#'
#' * Low risk (0-1 points)
#' * Low-intermediate risk (2 points)
#' * High-intermediate risk (3 points)
#' * High risk (4-5 points)
#' @format A named vector of length 5. See Examples for details
#' @examples \dontrun{
#' as.data.frame(ipi)
#' #                   ipi
#' # age_gt_60           1  # Age greater than 60 years
#' # stage_3_4           1  # Stage III or IV disease
#' # lhd_elevated        1  # Elevated serum LDH
#' # ecog_2_3_4          1  # ECOG/Zubrod performance status of 2, 3, or 4
#' # ex_nod_sites_gt_1   1  # More than 1 extranodal site
#' }
#' @source \url{https://doi.org/10.1056/NEJM199309303291402}
utils::globalVariables("ipi")
data__ipi <- function() {
  ipi <- c(
    age_gt_60 = 1.0, # Age greater than 60 years
    stage_3_4 = 1.0, # Stage III or IV disease
    lhd_elevated = 1.0, # Elevated serum LDH
    ecog_2_3_4 = 1.0, # ECOG/Zubrod performance status of 2, 3, or 4
    ex_nod_sites_gt_1 = 1.0 # More than 1 extranodal site
  )
  usethis::use_data(ipi, overwrite = TRUE)
}


#' @name aaipi
#' @title The Age Adjusted International Prognostic Index (aaIPI)
#' @description The Age Adjusted International Prognostic Index (aaIPI) as
#' described in *A predictive model for aggressive non-Hodgkin's lymphoma. The
#' International Non-Hodgkin's Lymphoma Prognostic Factors Project. N Engl J
#' Med 1993;329(14):987-94*.
#'
#' The aaIPI works by assigning one point for each the following factors:
#'
#' * Stage III or IV disease
#' * Elevated serum LDH
#' * ECOG/Zubrod performance status of 2, 3, or 4
#'
#' The sum of points can be used to separate samples into following risk groups:
#'
#' * Low risk (0 points)
#' * Low-intermediate risk (1 points)
#' * High-intermediate risk (2 points)
#' * High risk (3 points)
#' @format A named vector of length 3. See Examples for details
#' @examples \dontrun{
#' as.data.frame(aaipi)
#' #                   ipi
#' # stage_3_4           1  # Stage III or IV disease
#' # lhd_elevated        1  # Elevated serum LDH
#' # ecog_2_3_4          1  # ECOG/Zubrod performance status of 2, 3, or 4
#' }
#' @source \url{https://doi.org/10.1056/NEJM199309303291402}
utils::globalVariables("aaipi")
data__aaipi <- function() {
  aaipi <- c(
    stage_3_4 = 1.0, # Stage III or IV disease
    lhd_elevated = 1.0, # Elevated serum LDH
    ecog_2_3_4 = 1.0 # ECOG/Zubrod performance status of 2, 3, or 4
  )
  usethis::use_data(aaipi, overwrite = TRUE)
}