\name{check.consistence}
\alias{check.consistence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check a dataset for inconsistencies in Timestamp or Recordnumber}
\description{Searches through the dataset and controlls whether the Timestamp is equidistant and the Recordnumber is growing by an increment of 1. Creates an .txt when finding any inconsistency}
\usage{
check.consistence(data)
}
\arguments{
  \item{data}{Data from the read.loggerdat function. The first column has to be "TIMESTAMP", the second has to be "RECORD"}
}
\details{This function uses the distance between the first two TIMESTAMPs. If these are not representative you have to preprocess your data. A look into str(data) can prevent this kind of mistake by having an eye on the TIMESTAMP column.
}
\value{
  \item{Console message}{If there is something fishy going on you'll get a message}
  \item{.txt data}{Every row listed here is damaged. Visiting these directly often gives insight to the type of error.}
}
\references{www.klima.tu-berlin.de}
\author{Carsten Vick (carsten.vick@campus.tu-berlin.de)}
\note{last update: 2014-05-14 TO DO: getting clean messages and a better formated .txt item}
\seealso{
  \code{\link{read.loggerdat}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
