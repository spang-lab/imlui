% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{db__execute}
\alias{db__execute}
\title{Execute SQL query for IMLUI_DATABASE}
\usage{
db__execute(query, params = NULL)
}
\arguments{
\item{query}{SQL query as charactern string (can include ?)}

\item{params}{parameters for query used as substitute for ? in \code{query}}
}
\description{
Execute SQL query
}
\examples{
\dontrun{
execute("INSERT INTO cars (speed, dist) VALUES (1, 1), (2, 2), (3, 3)")
query <- "UPDATE Appstate SET resource_value = ? WHERE
          user_id = ? AND resource_id = ?"
params <- list(url, user_id, "url")
execute(query, params)
}
}
