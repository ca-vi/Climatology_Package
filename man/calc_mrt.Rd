\name{calc_mrt}
\alias{calc_mrt}

\title{Calculation of the Mean Radiant Temperatur (MRT)
}
\description{
This function calculates the mean radiant temperature (mrt) from radiation long- and shortwave fluxes of all directions according to Thorsson et al. 2007.
}
\usage{
calc_mrt(data=NULL, SW_UP=NA, SW_DOWN=NA, SW_EAST=NA,
                      SW_WEST=NA, SW_NORTH=NA, SW_SOUTH=NA, LW_UP=NA,
                      LW_DOWN=NA, LW_EAST=NA, LW_WEST=NA, LW_NORTH=NA,
                      LW_SOUTH=NA)
}
\arguments{
  \item{data}{ This functions need an input table with all fluxes from all directions for instance as it comes from a human response station (biomet).}
  \item{SW_UP}{You can give every radiation flux column individually. But you need all of them.}
  \item{SW_DOWN}{You can give every radiation flux column individually. But you need all of them.}
  \item{SW_EAST}{You can give every radiation flux column individually. But you need all of them.}
  \item{SW_WEST}{You can give every radiation flux column individually. But you need all of them.}
  \item{SW_NORTH}{You can give every radiation flux column individually. But you need all of them.}
  \item{SW_SOUTH}{You can give every radiation flux column individually. But you need all of them.}
  \item{LW_UP}{You can give every radiation flux column individually. But you need all of them.}
  \item{LW_DOWN}{You can give every radiation flux column individually. But you need all of them.}
  \item{LW_EAST}{You can give every radiation flux column individually. But you need all of them.}
  \item{LW_WEST}{You can give every radiation flux column individually. But you need all of them.}
  \item{LW_NORTH}{You can give every radiation flux column individually. But you need all of them.}
  \item{LW_SOUTH}{You can give every radiation flux column individually. But you need all of them.}
}
\details{
}
\value{
The value is rounded to two digits.
}
\references{
Thorsson S, Lindberg F, Eliasson I, Holmer B. 2007. Different methods for estimating the mean radiant temperature in an outdoor setting. International Journal of Climatology 27: 1983–1993.
}
\author{
AlK akrug@mailbox.tu-berlin.de}
\examples{
data(biomet)
mrt <- calc_mrt(biomet)
}
\keyword{ mean radiant temperature}
\keyword{ mrt }
