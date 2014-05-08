\name{globe2mrt}
\alias{globe2mrt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of MRT from Globe Thermometer
}
\description{
Calculation of the Mean Radiant Temperature (MRT) from the Globe Thermometer temperature, according to Thorsson et al. 2007 (DOI: 10.1002/joc.1537).
}
\usage{
globe2mrt(x, Tg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A table containing Wind Speed and Air Temperature of the HRS. 
}
  \item{Tg}{
Select the column of the Globe Thermometer temperature.}
}
\details{
The body's emissivity is set so 0.97 and the Globe Thermometer diamater is set to 40 mm.}
\value{
Returns the MRT value(s) of the selected input.

}
\references{
%% ~put references to the literature/web site here ~

}
\author{
FG Klimatologie (TU-Berlin), Alexander Krug
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Thorsson et al. (2007), Different methods for estimating the mean radiant temperature in an outdoor urban setting
}
\examples{

## The function is currently defined as
function (x, Tg) 
{
    eps = 0.97
    D = 40
    va = x$Wind_Speed
    Ta = x$Ta
    if (exists("Ta") & exists("va") & exists("Tg")) 
        message("Great, all important data are loaded")
    else message("Please check input table for missing data")
    buff = (Tg + 273.15)^4 + ((1.335 * 10^8) * (va^0.71)/(eps * 
        (D^0.4))) * (Tg - Ta)
    mrt = (buff^0.25) - 273.15
    return(mrt)
    message("Done ;-)")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
