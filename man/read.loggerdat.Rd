\name{read.loggerdat}
\alias{read.loggerdat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read in datasets from Campbell Loggers which have a special presetting}
\description{Creates a data.frame, tests the first input in the Recordnumber and alters the Timestamp into a date-time Format}
\usage{
readin_loggerdat(file, info = TRUE)
}
\arguments{
  \item{file}{Directory path to your data}
  \item{info}{logical, whether or not you want a str(data) with reading the data.
}
\value{
  \item{TIMESTAMP}{The timestamp in date-time format}
  \item{RECORD}{The recordnumber in sequence (best case)}
  \item{Temp, RH, IRTS, Rad, ShortIn, Windspeed, Rain, ...}{Different meteorological elements or metadata like from the inclinometer}
}
\references{www.klima.tu-berlin.de}
\author{ Carsten Vick (carsten.vick@campus.tu-berlin.de)}
\note{ last update: 2014-05-08 TO DO: combine with check.consistence (?)}

\seealso{ \code{\link{help}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }