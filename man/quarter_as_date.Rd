% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{quarter_as_date}
\alias{quarter_as_date}
\title{quarter_as_date}
\usage{
quarter_as_date(year, quarter, day = 1)
}
\arguments{
\item{year}{a year}

\item{quarter}{a quarter, 1 to 4}

\item{day}{a day}
}
\value{
a date variable according to the start date of the quarter
}
\description{
Convert discrete year and quarter to date for plotting purposes.
}
\examples{
quarter_as_date(2008, 1, 15)
}
