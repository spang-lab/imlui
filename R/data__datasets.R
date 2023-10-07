generate_all_datasets <- function() {
  data__dummy_staiger_2020_test2_dataset()
  data__dummy_staiger_2020_test1_dataset()
  data__dummy_staiger_2020_training_dataset()
  data__dummy_nordmo_2020_training_dataset()
  data__dummy_reinders_2020_test_dataset()
  data__dummy_reinders_2020_training_dataset()
}


simulate_dataset_from_signature <- function(sig,
                                            n,
                                            mu = 10,
                                            nu = 1,
                                            seed = 1,
                                            sig_pfs = sig) {

  # Simulate log gene expression as random numbers scattered around 10
  set.seed(seed)
  sig <- sig
  features <- unique(c(names(sig), names(sig_pfs)))
  p <- length(features)
  X <- matrix(stats::rnorm(n * p, mu, nu), nrow = n, ncol = length(features))
  colnames(X) <- features
  X <- as.data.frame(X)

  # Simulate overall survival in months based on gene expression data as
  # os_hat = h0 * exp(sig * X)
  # os = os_hat + stats::rnorm(n, sd = sd(os_hat))
  loghr <- stats::predict(sig, X) # log hazard ratio for each samples
  loghr <- loghr + stats::rnorm(n, stats::sd(loghr))
  loghr <- scale(loghr) / 10 # scale log hazard ratio to prevent extreme values
  hr <- exp(loghr) # hazard ratio for each samples
  h0 <- 0.02 # base hazard rate
  h <- h0 * hr # hazard rate for each sample
  os <- os_raw <- stats::rgeom(n, h) # overall survival in months

  # Set max OS to 144 months (12 years)
  idx <- os > 144
  os_stat[idx] <- 0
  os[idx] <- 144

  # Simulate progression free survival the same way but increase base hazard
  # rate (because PFS is lower or equal than OS), i.e.
  # pfs_hat = h0 * exp(sig_pfs * X)
  # pfs = pfs_hat + stats::rnorm(n, sd = sd(pfs_hat))
  loghr <- stats::predict(sig_pfs, X) # log hazard ratio for each samples
  loghr <- loghr + stats::rnorm(n, stats::sd(loghr))
  loghr <- scale(loghr) / 10 # scale log hazard ratio to prevent extreme values
  hr <- exp(loghr) # hazard ratio for each samples
  h0 <- 0.025 # base hazard rate
  h <- h0 * hr # hazard rate for each sample
  pfs <- pfs_raw <- stats::rgeom(n, h) # overall survival in months

  # Check if PFS is higher than OS for a sample. If it is, set that samples PFS
  # to a value between 0 and OS.
  idx <- pfs > os
  pfs[idx] <- sapply(pfs[idx], function(n) sample(1:n, 1))

  # Censor samples as follows: randomly draw 0=censored or 1=death for each
  # sample. If censored (0), draw a random number between 0 and the sample's OS
  # and set that number as new OS endpoint. If that number is smaller than the
  # sample's pfs, set pfs to that number as well and set pfs_stat to 0 as well
  # (censored).
  os_stat <- sample(c(0, 1), n, replace = TRUE, prob = c(0.45, 0.55))
  idx <- os_stat == 0
  os[idx] <- sapply(os[idx], function(x) sample(1:x, 1))
  idx <- os < pfs
  pfs[idx] <- os[idx]
  pfs_stat <- rep(0, length(os))
  idx <- pfs < os
  pfs_stat[idx] <- 1

  # Compare censoring times for PFS and OS
  X$OS <- os
  X$OS_STAT <- os_stat
  X$PFS <- pfs
  X$PFS_STAT <- pfs_stat
  # hist(os, 100)
  # hist(pfs, 100)

  # Simulate IPI values based on OS. Increase chance of "positive" attributes,
  # e.g. "younger than 60" for patients with high survival from 0.5 up to 0.833
  # (0.5 + 1/3).
  max_os <- max(os)
  sample_func <- function(s) {
    factr <- s/max_os * 1/3
    sample(c(0, 1), 1, prob = c(0.5 + factr , 0.5 - factr))
  }
  X$IPI_AGE <- sapply(os, sample_func)   # [0:age<=60, 1:age>60]
  X$IPI_LDH <- sapply(os, sample_func)   # [0:LDH<N, 1:LDH>N]
  X$IPI_ECOG <- sapply(os, sample_func)  # [0:ECOG<=1, 1:ECOG>1]
  X$IPI_STAGE <- sapply(os, sample_func) # [0=Stage I/II, 1=III/IV]
  X$IPI_EXBM <- sapply(os, sample_func)  # [0:EL<=1, 1:EL>1]

  rownames(X) <- paste0("Sample", 1:n)

  return(X)
}


#' @name dummy_staiger_2020_test2_dataset
#' @title Dummy Dataset
#' @description The LAMIS signature as described in *Staiger, A.M.,
#' Altenbuchinger, M., Ziepert, M. et al. A novel lymphoma-associated
#' macrophage interaction signature (LAMIS) provides robust risk
#' prognostication in diffuse large B-cell lymphoma clinical trial cohorts of
#' the DSHNHL. Leukemia 34, 543-552 (2020)* was tested on two different test
#' sets, called validation set I and validation set II. This dataset is
#' **neither** of those two datasets, but instead **simulated completely from
#' scratch** to allow for a test of the imlui application without having access
#' to the real datasets. This dataset is meant as dummy replacement for the real
#' *Validation set II* and has the amount of samples (466) as described in the
#' publication.
#' @format A data.frame with 466 obs. of 19 variables. For details see section
#' Examples.
#' @examples \dontrun{
#' head(dummy_staiger_2020_test2_dataset)
#' #      BCL2A1       BSG      CCL5     CCND2     CD163      CD47     CPNE3
#' # 1  9.373546  9.294094  9.828148 10.099903  9.926718 10.477717 10.592971
#' # 2 10.183643 10.628017  7.597769 10.736960 10.863403 10.074683 11.274842
#' # 3  9.164371 11.480214 10.795907 11.546881 11.074707 11.942431 11.006990
#' # 4 11.595281 11.083430 12.169116 10.178921  9.794147 10.505147  7.638170
#' # 5 10.329508  9.186756 10.058383  9.717453  9.566386  9.741538  7.895940
#' # 6  9.179532  8.381123  8.645086  9.232701  9.817776  8.737528  9.915561
#' #       CPT1A      CSF1     CXCR4    DPYSL3       FYN      IL16       MME
#' # 1  9.613627 11.558511  9.501991 10.926738 11.126300  9.555998 10.293949
#' # 2  9.153181 10.860637  9.383986  9.498876  9.528962 11.584762 12.995920
#' # 3  7.827073 10.142137 10.020728 10.057797 10.437422  9.975349 10.102538
#' # 4  9.282094 10.202363  9.587168  9.635093 10.422757  9.485306  9.694996
#' # 5 11.699783  9.499748 10.412128 10.119857  9.988803  9.486007 10.277728
#' # 6  9.930807  8.849308  8.661739  9.344161 11.317913  9.861915  8.500039
#' #        MPST     PSAT1       TBP OS OS_STAT IPI_AGE IPI_LDH IPI_ECOG
#' # 1  9.740872  9.608714 11.814033 24       1       1       1        0
#' # 2 10.132574 10.217192  9.426317  2       0       0       1        0
#' # 3 10.223062 10.578993  9.320186  1       1       0       0        1
#' # 4 11.411904  9.432382  9.861952  2       1       0       1        0
#' # 5  9.742137 10.939025 11.597184  9       0       1       0        0
#' # 6  9.541816 10.117497 11.003334 79       0       0       0        1
#' #   IPI_STAGE IPI_EXBM
#' # 1         1        1
#' # 2         1        0
#' # 3         1        1
#' # 4         0        1
#' # 5         0        0
#' # 6         0        0
#' }
#' @source
#' \url{https://www.nature.com/articles/s41375-019-0573-y/}
utils::globalVariables("dummy_staiger_2020_test2_dataset")
data__dummy_staiger_2020_test2_dataset <- function() {
  dummy_staiger_2020_test2_dataset <- simulate_dataset_from_signature(
    sig = staiger_2020_survival_signature_lamis,
    n = 466,
    seed = 1
  )
  usethis::use_data(dummy_staiger_2020_test2_dataset, overwrite = TRUE)
}


#' @name dummy_staiger_2020_test1_dataset
#' @title Dummy Dataset
#' @description The LAMIS signature as described in *Staiger, A.M.,
#' Altenbuchinger, M., Ziepert, M. et al. A novel lymphoma-associated
#' macrophage interaction signature (LAMIS) provides robust risk
#' prognostication in diffuse large B-cell lymphoma clinical trial cohorts of
#' the DSHNHL. Leukemia 34, 543-552 (2020)* was tested on two different test
#' sets, called validation set I and validation set II. This dataset is
#' **neither** of those two datasets, but instead **simulated completely from
#' scratch** to allow for a test of the imlui application without having access
#' to the real datasets. This dataset is meant as dummy replacement for the real
#' *Validation set I* and has the amount of samples (181) as described in the
#' publication.
#' @format A data.frame with 181 obs. of 19 variables. For details see section
#' Examples.
#' @examples \dontrun{
#' head(dummy_staiger_2020_test1_dataset)
#' #     BCL2A1      BSG     CCL5     CCND2    CD163     CD47    CPNE3    CPT1A
#' # 1 11.37355 12.98390 11.36446 12.018607 11.02445 10.96710 13.26201 11.49082
#' # 2 12.18364 12.21992 11.57002 13.318449 12.07066 12.65920 11.59423 11.14299
#' # 3 11.16437 10.53275 11.83068 11.934168 10.48140 12.23783 12.66676 13.61617
#' # 4 13.59528 12.52102 12.61222 11.299704 12.86378 12.71528 12.16464 12.99387
#' # 5 12.32951 11.84125 12.67834 12.537326 12.50157 11.06152 13.78152 12.69685
#' # 6 11.17953 13.46459 12.56795  9.798218 11.64522 12.09535 12.71121 13.69997
#' #       CSF1    CXCR4   DPYSL3      FYN     IL16      MME      MPST    PSAT1
#' # 1 12.23356 11.98254 11.92833 12.84547 11.45736 11.89782 12.805547 12.11641
#' # 2 11.73096 11.75903 11.86686 12.46725 13.97869 12.16720 11.787799 11.53369
#' # 3 11.34145 10.20801 11.91192 11.59794 10.63389 12.45661 11.147571 13.84623
#' # 4 12.06335 14.04969 12.91779 12.92353 12.42490 11.49843 10.703325 10.70860
#' # 5 12.05448 10.88278 12.03144 11.99194 11.02369 12.22694  9.455339 12.96074
#' # 6 12.24701 10.64645 13.35893 13.03378 13.19297 11.29137 12.536258 12.23254
#' #        TBP  OS OS_STAT IPI_AGE IPI_LDH IPI_ECOG IPI_STAGE IPI_EXBM
#' # 1 12.44005 120       1       0       0        0         1        0
#' # 2 10.47804  51       0       0       0        0         0        0
#' # 3 12.10962  17       1       0       1        1         0        0
#' # 4 12.18904   2       1       0       1        0         0        0
#' # 5 12.78104  12       0       1       0        0         1        0
#' # 6 11.95349   4       0       0       1        0         0        1
#' }
#' @source
#' \url{https://www.nature.com/articles/s41375-019-0573-y/}
utils::globalVariables("dummy_staiger_2020_test1_dataset")
data__dummy_staiger_2020_test1_dataset <- function() {
  dummy_staiger_2020_test1_dataset <- simulate_dataset_from_signature(
    sig = staiger_2020_survival_signature_lamis,
    n = 181,
    mu = 12,
    seed = 2
  )
  usethis::use_data(dummy_staiger_2020_test1_dataset, overwrite = TRUE)
}


#' @name dummy_staiger_2020_training_dataset
#' @title Dummy Dataset
#' @description The LAMIS signature as described in *Staiger, A.M.,
#' Altenbuchinger, M., Ziepert, M. et al. A novel lymphoma-associated
#' macrophage interaction signature (LAMIS) provides robust risk
#' prognostication in diffuse large B-cell lymphoma clinical trial cohorts of
#' the DSHNHL. Leukemia 34, 543-552 (2020)* was trained on a dataset consisting
#' of 233 samples. This dataset is **not** the dataset from the publication,
#' but a instead a dataset that was **simulated completely from scratch** to
#' allow for a test of the imlui application without having access to the real
#' dataset.
#' @format A data.frame with 233 obs. of 19 variables. For details see section
#' Examples.
#' @examples \dontrun{
#' head(dummy_staiger_2020_training_dataset)
#' #     BCL2A1      BSG     CCL5     CCND2    CD163     CD47    CPNE3    CPT1A
#' # 1 11.37355 12.98390 11.36446 12.018607 11.02445 10.96710 13.26201 11.49082
#' # 2 12.18364 12.21992 11.57002 13.318449 12.07066 12.65920 11.59423 11.14299
#' # 3 11.16437 10.53275 11.83068 11.934168 10.48140 12.23783 12.66676 13.61617
#' # 4 13.59528 12.52102 12.61222 11.299704 12.86378 12.71528 12.16464 12.99387
#' # 5 12.32951 11.84125 12.67834 12.537326 12.50157 11.06152 13.78152 12.69685
#' # 6 11.17953 13.46459 12.56795  9.798218 11.64522 12.09535 12.71121 13.69997
#' #       CSF1    CXCR4   DPYSL3      FYN     IL16      MME      MPST    PSAT1
#' # 1 12.23356 11.98254 11.92833 12.84547 11.45736 11.89782 12.805547 12.11641
#' # 2 11.73096 11.75903 11.86686 12.46725 13.97869 12.16720 11.787799 11.53369
#' # 3 11.34145 10.20801 11.91192 11.59794 10.63389 12.45661 11.147571 13.84623
#' # 4 12.06335 14.04969 12.91779 12.92353 12.42490 11.49843 10.703325 10.70860
#' # 5 12.05448 10.88278 12.03144 11.99194 11.02369 12.22694  9.455339 12.96074
#' # 6 12.24701 10.64645 13.35893 13.03378 13.19297 11.29137 12.536258 12.23254
#' #        TBP  OS OS_STAT IPI_AGE IPI_LDH IPI_ECOG IPI_STAGE IPI_EXBM
#' # 1 12.44005 120       1       0       0        0         1        0
#' # 2 10.47804  51       0       0       0        0         0        0
#' # 3 12.10962  17       1       0       1        1         0        0
#' # 4 12.18904   2       1       0       1        0         0        0
#' # 5 12.78104  12       0       1       0        0         1        0
#' # 6 11.95349   4       0       0       1        0         0        1
#' }
#' @source
#' \url{https://www.nature.com/articles/s41375-019-0573-y/}
utils::globalVariables("dummy_staiger_2020_training_dataset")
data__dummy_staiger_2020_training_dataset <- function() {
  dummy_staiger_2020_training_dataset <- simulate_dataset_from_signature(
    sig = staiger_2020_survival_signature_lamis,
    n = 233,
    mu = 12,
    seed = 3
  )
  usethis::use_data(dummy_staiger_2020_training_dataset, overwrite = TRUE)
}


#' @name dummy_nordmo_2020_training_dataset
#' @title Dummy Dataset
#' @description This is **not** the training dataset used in Nordmo, Carmen,
#' Gunther Glehr, Michael Altenbuchinger, Rainer Spang, Marita Ziepert, Heike
#' Horn, Annette M. Staiger, et al. "Identification of a MiRNA Based Model to
#' Detect Prognostic Subgroups in Patients with Aggressive B-Cell Lymphoma".
#' Leukemia & Lymphoma 62, no. 5 (16 April 2021): 1107-15.
#' URL: <https://doi.org/10.1080/10428194.2020.1861268>. Instead, this dataset
#' was **simulated completely from scratch** to allow for a test of the
#' imlui application without having access to the real dataset.
#' @format See section Details.
#' @examples \dontrun{
#' head(dummy_nordmo_2020_training_dataset)
#' #   hsa-miR-106b-5p hsa-miR-130a-3p hsa-miR-365a-3p hsa-miR-374a-5p
#' # 1        5.373546        6.510108        4.570007        5.753530
#' # 2        6.183643        5.835624        7.709121        5.680214
#' # 3        5.164371        6.420695        7.435070        7.362644
#' # 4        7.595281        5.599753        5.289629        4.772117
#' # 5        6.329508        4.629792        5.934932        5.488781
#' # 6        5.179532        6.987838        4.240531        5.268805
#' #   hsa-miR-423-5p hsa-miR-590-5p OS OS_STAT IPI_AGE IPI_LDH IPI_ECOG
#' # 1       8.236323       6.881278 12       1       1       0        1
#' # 2       6.302265       6.742082  0       0       1       1        0
#' # 3       4.957493       6.147573  3       1       0       1        0
#' # 4       5.016458       6.485389  2       1       0       1        1
#' # 5       8.005719       6.151856  4       0       1       1        0
#' # 6       3.929429       6.041999  2       0       0       1        0
#' #   IPI_STAGE IPI_EXBM
#' # 1         1        0
#' # 2         1        0
#' # 3         0        1
#' # 4         1        1
#' # 5         0        0
#' # 6         0        1
#' }
#' @source \url{https://doi.org/10.1080/10428194.2020.1861268}
utils::globalVariables("dummy_nordmo_2020_training_dataset")
data__dummy_nordmo_2020_training_dataset <- function() {
  dummy_nordmo_2020_training_dataset <- simulate_dataset_from_signature(
    sig = nordmo_2020_os_signature,
    n = 209,
    mu = 6,
    seed = 4,
    sig_pfs = nordmo_2020_pfs_signature
  )
  usethis::use_data(dummy_nordmo_2020_training_dataset, overwrite = TRUE)
}


#' @name dummy_reinders_2020_test_dataset
#' @title Dummy Dataset
#' @description This is **not** the test dataset used in *Reinders, Jörg, et
#' al. "Platform independent protein-based cell-of-origin subtyping of diffuse
#' large B-cell lymphoma in formalin-fixed paraffin-embedded tissue."
#' Scientific Reports 10.1 (2020): 1-11.*. URL:
#' [https://doi.org/10.1038/s41598-020-64212-z](https://doi.org/10.1038/s41598-020-64212-z).
#' Instead, this dataset was **simulated completely from scratch** to allow for
#' a test of the imlui application without having access to the real dataset.
#' @format See section Examples.
#' @examples \dontrun{
#' head(dummy_reinders_2020_test_dataset)
#' #   sp|P04233|HG2A_HUMAN sp|Q15063|POSTN_HUMAN sp|P51884|LUM_HUMAN
#' # 1             18.74709              20.79621            18.75927
#' # 2             20.36729              18.77595            20.08423
#' # 3             18.32874              20.68224            18.17816
#' # 4             23.19056              17.74127            20.31606
#' # 5             20.65902              22.86605            18.69083
#' # 6             18.35906              23.96080            23.53457
#' #   sp|Q9NS69|TOM22_HUMAN sp|P62841|RPS15_HUMAN sp|P16070|CD44_HUMAN
#' # 1              20.90037              20.81880             20.27244
#' # 2              19.96288              23.37775             20.81434
#' # 3              19.36386              23.17318             19.86069
#' # 4              18.14128              19.33818             19.50467
#' # 5              17.02508              15.42953             21.39110
#' # 6              17.84962              24.99532             22.29246
#' #   sp|P01871|IGHM_HUMAN sp|P18031|PTN1_HUMAN Intercept  OS OS_STAT IPI_AGE
#' # 1             21.78735             22.94776  22.14888 113       1       0
#' # 2             17.90540             21.35454  23.79131  22       0       0
#' # 3             23.94267             20.75993  18.79401  32       1       0
#' # 4             19.23274             19.61440  19.21826   9       1       0
#' # 5             23.30829             23.15578  19.16756   8       0       1
#' # 6             23.02443             21.19247  19.24869   1       0       0
#' #   IPI_LDH IPI_ECOG IPI_STAGE IPI_EXBM
#' # 1       0        0         0        0
#' # 2       1        1         0        1
#' # 3       1        0         1        0
#' # 4       1        1         1        0
#' # 5       1        0         0        0
#' # 6       0        1         1        0
#' }
#' @source \url{https://doi.org/10.1080/10428194.2020.1861268}
utils::globalVariables("dummy_reinders_2020_test_dataset")
data__dummy_reinders_2020_test_dataset <- function() {
  dummy_reinders_2020_test_dataset <- simulate_dataset_from_signature(
    sig = reinders_2020_coo_signature,
    n = 50,
    mu = 20,
    nu = 2,
    seed = 5
  )
  usethis::use_data(dummy_reinders_2020_test_dataset, overwrite = TRUE)
}

#' @name dummy_reinders_2020_training_dataset
#' @title Dummy Dataset
#' @description This is **not** the training dataset used in *Reinders, Jörg, et
#' al. "Platform independent protein-based cell-of-origin subtyping of diffuse
#' large B-cell lymphoma in formalin-fixed paraffin-embedded tissue."
#' Scientific Reports 10.1 (2020): 1-11.*. URL:
#' [https://doi.org/10.1038/s41598-020-64212-z](https://doi.org/10.1038/s41598-020-64212-z).
#' Instead, this dataset was **simulated completely from scratch** to allow for
#' a test of the imlui application without having access to the real dataset.
#' @format See section Examples.
#' @examples \dontrun{
#' head(dummy_reinders_2020_training_dataset)
#' #   sp|P04233|HG2A_HUMAN sp|Q15063|POSTN_HUMAN sp|P51884|LUM_HUMAN
#' # 1             18.74709              20.79621            18.75927
#' # 2             20.36729              18.77595            20.08423
#' # 3             18.32874              20.68224            18.17816
#' # 4             23.19056              17.74127            20.31606
#' # 5             20.65902              22.86605            18.69083
#' # 6             18.35906              23.96080            23.53457
#' #   sp|Q9NS69|TOM22_HUMAN sp|P62841|RPS15_HUMAN sp|P16070|CD44_HUMAN
#' # 1              20.90037              20.81880             20.27244
#' # 2              19.96288              23.37775             20.81434
#' # 3              19.36386              23.17318             19.86069
#' # 4              18.14128              19.33818             19.50467
#' # 5              17.02508              15.42953             21.39110
#' # 6              17.84962              24.99532             22.29246
#' #   sp|P01871|IGHM_HUMAN sp|P18031|PTN1_HUMAN Intercept  OS OS_STAT IPI_AGE
#' # 1             21.78735             22.94776  22.14888 113       1       0
#' # 2             17.90540             21.35454  23.79131  22       0       0
#' # 3             23.94267             20.75993  18.79401  32       1       0
#' # 4             19.23274             19.61440  19.21826   9       1       0
#' # 5             23.30829             23.15578  19.16756   8       0       1
#' # 6             23.02443             21.19247  19.24869   1       0       0
#' #   IPI_LDH IPI_ECOG IPI_STAGE IPI_EXBM
#' # 1       0        0         0        0
#' # 2       1        1         0        1
#' # 3       1        0         1        0
#' # 4       1        1         1        0
#' # 5       1        0         0        0
#' # 6       0        1         1        0
#' }
#' @source \url{https://doi.org/10.1080/10428194.2020.1861268}
utils::globalVariables("dummy_reinders_2020_training_dataset")
data__dummy_reinders_2020_training_dataset <- function() {
  dummy_reinders_2020_training_dataset <- simulate_dataset_from_signature(
    sig = reinders_2020_coo_signature,
    n = 50,
    mu = 20,
    nu = 2,
    seed = 6
  )
  usethis::use_data(dummy_reinders_2020_training_dataset, overwrite = TRUE)
}
