\name{checkConsistence}
\alias{checkConsistence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check a dataset for inconsistencies in Timestamp or Recordnumber}
\description{The function will search through the dataset and checks whether the Timestamp is equidistant and the Recordnumber is growing by an increment of 1. Creates an inconsistency.txt when finding any inconsistencies}
\usage{
checkConsistence(data, info=TRUE)
}
\arguments{
  \item{data}{    Data from the \link{read.logger} function .Their has to be one column named "TIMESTAMP" and one called "RECORD"}
  \item{info}{    Logical. More Info and inconsistency.txt}
}
\details{This function uses the most frequent distance between TIMESTAMPs. In very short data.frames this could lead to an wrong TIMESTAMP.}
\value{
  \item{logical}{Returning TRUE if everything was fine, returning FALSE otherwise.}
  \item{Console message}{If info = TRUE and there is something fishy going on, you'll get a message.}
  \item{inconsistency.txt}{When info = TRUE: every row listed here is damaged. Visiting these directly often gives insight to the type of error.}
}
\references{www.klima.tu-berlin.de}
\author{Carsten Vick (carsten.vick@campus.tu-berlin.de)}
\note{last update: 2015-02-18 TO DO: getting clean messages and a better formated .txt item !!!}
\seealso{
  \code{\link{read.logger}}
}