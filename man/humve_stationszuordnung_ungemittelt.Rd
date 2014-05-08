\name{humve_stationszuordnung_ungemittelt}
\alias{humve_stationszuordnung_ungemittelt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{AWS station
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
humve_stationszuordnung_ungemittelt(humvedata_meteo, humvedata_wind, humvedata_gill, protokolldata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{humvedata_meteo}{
%%     ~~Describe \code{humvedata_meteo} here~~
}
  \item{humvedata_wind}{
%%     ~~Describe \code{humvedata_wind} here~~
}
  \item{humvedata_gill}{
%%     ~~Describe \code{humvedata_gill} here~~
}
  \item{protokolldata}{
%%     ~~Describe \code{protokolldata} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (humvedata_meteo, humvedata_wind, humvedata_gill, protokolldata) 
{
    humvedata_meteo <- humvedata_meteo[humvedata_meteo$TIMESTAMP >= 
        min(protokolldata$beginn) & humvedata_meteo$TIMESTAMP <= 
        max(protokolldata$ende), ]
    humvedata_wind <- humvedata_wind[humvedata_wind$TIMESTAMP >= 
        min(protokolldata$beginn) & humvedata_wind$TIMESTAMP <= 
        max(protokolldata$ende), ]
    humvedata_gill <- humvedata_gill[humvedata_gill$TIMESTAMP >= 
        min(protokolldata$beginn) & humvedata_gill$TIMESTAMP <= 
        max(protokolldata$ende), ]
    for (i in seq_along(protokolldata$station)) {
        meteo <- humvedata_meteo[humvedata_meteo$TIMESTAMP >= 
            protokolldata$beginn[i] & humvedata_meteo$TIMESTAMP <= 
            protokolldata$ende[i], ]
        wind <- humvedata_wind[humvedata_wind$TIMESTAMP >= protokolldata$beginn[i] & 
            humvedata_wind$TIMESTAMP <= protokolldata$ende[i], 
            ]
        gill <- humvedata_gill[humvedata_gill$TIMESTAMP >= protokolldata$beginn[i] & 
            humvedata_gill$TIMESTAMP <= protokolldata$ende[i], 
            ]
        station <- rep(protokolldata$station[i], length.out = length(meteo[, 
            1]))
        kt19 <- rep(protokolldata$KT.19[i], length.out = length(meteo[, 
            1]))
        if (!exists("result")) {
            result <- as.data.frame(cbind(meteo[, 1], station, 
                meteo[, 3:7], meteo[, 10:12], wind[, 3:5], gill[, 
                  3:4], gill[, 7:8], kt19))
        }
        else {
            result <- rbind(result, cbind(meteo[, 1], station, 
                meteo[, 3:7], meteo[, 10:12], wind[, 3:5], gill[, 
                  3:4], gill[, 7:8], kt19))
        }
    }
    names(result) <- c("TIMESTAMP", "station", "Ta_150cm", "RH_150cm", 
        "NETRAD", "KWO", "KWU", "IRTS", "ANGX", "ANGY", "WS", 
        "WD", "SIGMA_WD", "U", "V", "W", "Tv", "KT19")
    print(str(result))
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
