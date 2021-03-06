% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/memory.R
\name{ls.objects}
\alias{ls.objects}
\alias{lsos}
\title{improved list of objects}
\source{
Tony Breyal's post attributed to JD Long at:

https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
}
\usage{
ls.objects(pos = 1, pattern, order_by, decreasing = FALSE, head = FALSE,
  n = 5)

lsos(..., n = 10)
}
\arguments{
\item{pos}{(integer) position in search list}

\item{pattern}{(character) an optional regular expression}

\item{order_by}{(character or integer) which column to order by}

\item{decreasing}{(logical) whether to sort by decreasing}

\item{head}{(logical) whether to return the first part only}

\item{n}{Numeric of how many objects to return}

\item{...}{Other optional parameters:
\itemize{
\item \code{pos(integer)}: position in search list
\item \code{pattern(character)}: an optional regular expression
\item \code{order.by(character or integer)}: which column to order by
\item \code{decreasing(logical)}: whether to sort by decreasing
\item \code{head(logical)}: whether to return the first part only
}}
}
\value{
printed description of objects in memory
}
\description{
improved list of objects

Describe objects in memory
}
\examples{

df1 <- iris
df2 <- CO2
lsos()

}
