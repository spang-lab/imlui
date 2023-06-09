% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data__datasets.R
\name{dummy_staiger_2020_test2_dataset}
\alias{dummy_staiger_2020_test2_dataset}
\title{Dummy Dataset}
\format{
A data.frame with 466 obs. of 19 variables. For details see section
Examples.
}
\source{
\url{https://www.nature.com/articles/s41375-019-0573-y/}
}
\description{
The LAMIS signature as described in \emph{Staiger, A.M.,
Altenbuchinger, M., Ziepert, M. et al. A novel lymphoma-associated
macrophage interaction signature (LAMIS) provides robust risk
prognostication in diffuse large B-cell lymphoma clinical trial cohorts of
the DSHNHL. Leukemia 34, 543-552 (2020)} was tested on two different test
sets, called validation set I and validation set II. This dataset is
\strong{neither} of those two datasets, but instead \strong{simulated completely from
scratch} to allow for a test of the imlui application without having access
to the real datasets. This dataset is meant as dummy replacement for the real
\emph{Validation set II} and has the amount of samples (466) as described in the
publication.
}
\examples{
\dontrun{
head(dummy_staiger_2020_test2_dataset)
#      BCL2A1       BSG      CCL5     CCND2     CD163      CD47     CPNE3
# 1  9.373546  9.294094  9.828148 10.099903  9.926718 10.477717 10.592971
# 2 10.183643 10.628017  7.597769 10.736960 10.863403 10.074683 11.274842
# 3  9.164371 11.480214 10.795907 11.546881 11.074707 11.942431 11.006990
# 4 11.595281 11.083430 12.169116 10.178921  9.794147 10.505147  7.638170
# 5 10.329508  9.186756 10.058383  9.717453  9.566386  9.741538  7.895940
# 6  9.179532  8.381123  8.645086  9.232701  9.817776  8.737528  9.915561
#       CPT1A      CSF1     CXCR4    DPYSL3       FYN      IL16       MME
# 1  9.613627 11.558511  9.501991 10.926738 11.126300  9.555998 10.293949
# 2  9.153181 10.860637  9.383986  9.498876  9.528962 11.584762 12.995920
# 3  7.827073 10.142137 10.020728 10.057797 10.437422  9.975349 10.102538
# 4  9.282094 10.202363  9.587168  9.635093 10.422757  9.485306  9.694996
# 5 11.699783  9.499748 10.412128 10.119857  9.988803  9.486007 10.277728
# 6  9.930807  8.849308  8.661739  9.344161 11.317913  9.861915  8.500039
#        MPST     PSAT1       TBP OS OS_STAT IPI_AGE IPI_LDH IPI_ECOG
# 1  9.740872  9.608714 11.814033 24       1       1       1        0
# 2 10.132574 10.217192  9.426317  2       0       0       1        0
# 3 10.223062 10.578993  9.320186  1       1       0       0        1
# 4 11.411904  9.432382  9.861952  2       1       0       1        0
# 5  9.742137 10.939025 11.597184  9       0       1       0        0
# 6  9.541816 10.117497 11.003334 79       0       0       0        1
#   IPI_STAGE IPI_EXBM
# 1         1        1
# 2         1        0
# 3         1        1
# 4         0        1
# 5         0        0
# 6         0        0
}
}
