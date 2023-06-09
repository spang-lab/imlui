% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{db__get_table}
\alias{db__get_table}
\title{Get one table from IMLUI_DATABASE}
\usage{
db__get_table(tbl)
}
\arguments{
\item{tbl}{String, name of table}
}
\description{
Get table \code{tbl} from IMLUI_DATABASE as dataframe
}
\examples{
\dontrun{
db__connect()
db__get_table("users")
}
}
