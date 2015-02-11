\name{check.consistence}
\alias{check.consistence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check a dataset for inconsistencies in Timestamp or Recordnumber}
\description{The function will search through the dataset and checks whether the Timestamp is equidistant and the Recordnumber is growing by an increment of 1. Creates an inconsistency.txt when finding any inconsistencies}
\usage{
check.consistence(data)
}
\arguments{
  \item{data}{    Data from the \link{read.logger} function .Their has to be one column named "TIMESTAMP" and one called "RECORD"}
}
\details{This function uses the most frequent distance between TIMESTAMPs. In very short data.frames this could lead to an FALSE}
\value{
  \item{logical}{Returning TRUE if everything was fine, returning FALSE otherwise.}
  \item{Console message}{If there is something fishy going on you'll get a message}
  \item{inconsistency.txt}{Every row listed here is damaged. Visiting these directly often gives insight to the type of error.}
}
\references{www.klima.tu-berlin.de}
\author{Carsten Vick (carsten.vick@campus.tu-berlin.de)}
\note{last update: 2015-02-11 TO DO: getting clean messages and a better formated .txt item !!!}
\seealso{
  \code{\link{read.logger}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
