\name{calc_mrt}
\alias{calc_mrt}

\title{Calculation of the Mean Radiant Temperatur (MRT)
}
\description{
This function calculates the mean radiant temperature (mrt) from radiation long- and shortwave fluxes of all directions according to Thorsson et al. 2007. %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_mrt(data=NULL, SW_UP=NA, SW_DOWN=NA, SW_EAST=NA,
                      SW_WEST=NA, SW_NORTH=NA, SW_SOUTH=NA, LW_UP=NA,
                      LW_DOWN=NA, LW_EAST=NA, LW_WEST=NA, LW_NORTH=NA,
                      LW_SOUTH=NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ This functions need an input table with all fluxes from all directions for instance as it comes from a human response station (biomet).}
  \item{SW_XX}{You can give every radiation flux column individually. But you need all of them.}
  \item{LW_XX}{You can give every radiation flux column individually. But you need all of them.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The value is rounded to two digits.
}
\references{
Thorsson S, Lindberg F, Eliasson I, Holmer B. 2007. Different methods for estimating the mean radiant temperature in an outdoor setting. International Journal of Climatology 27: 1983–1993.
}
\author{
AlK akrug@mailbox.tu-berlin.de}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(biomet)
mrt <- calc_mrt(biomet)
}
\keyword{ 'mean radiant temperature'}
\keyword{ mrt }% __ONLY ONE__ keyword per line
