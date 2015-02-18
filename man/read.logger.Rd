\name{read.logger}
\alias{read.logger}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read in datasets from Campbell Loggers which have a special presetting (TOA5 long header)}
\description{Creates a data.frame and alters the Timestamp into a date-time Format}
\usage{
read.logger(file, info = TRUE, ...)
}
\arguments{
  \item{file}{character, indicating the Directory path to your data e.g. "/home/myRProject/mydata.txt"}
  \item{info}{logical, whether or not you want to print str(data) with reading the data.}
  \item{\dots}{More arguments passed to read.table}
}
\value{ Gives a data.frame with following columns:
  \item{TIMESTAMP}{The timestamp in date-time format}
  \item{RECORD}{The recordnumber in sequence (best case)}
  \item{Temp, RH, IRTS, Rad, ShortIn, Windspeed, Rain, ...}{Different meteorological elements like temperature or metadata as from the inclinometer or Supersonic-Anemometer}
}
\references{www.klima.tu-berlin.de}
\author{Carsten Vick (carsten.vick@campus.tu-berlin.de)}
\note{last update: 2015-02-18}

\seealso{ \code{\link{checkConsistence}}
}
\keyword{ Campbell Scientific }
\keyword{ LoggerNet }