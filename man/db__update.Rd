% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{db__update}
\alias{db__update}
\title{Update a single value in a table}
\usage{
db__update(tbl, col, val, id)
}
\arguments{
\item{tbl}{Name of table that should be updated}

\item{col}{Name of column that should be updated}

\item{val}{New Value}

\item{id}{ID of row that should be updated}
}
\description{
This functions upates a single value in a table that already
exists. The given colum and id must also exist. Translated to R, this
function does the same as \verb{idx <- which(tbl$id == id); tbl[idx, col] <- val}
}
