\name{calc_utci}
\alias{calc_utci}
\title{Calculation of the Universal Thermal Climate Index (UTCI) Value
}
\description{
This function calculates the UTCI value for a given input data
}
\usage{
calc_utci(data)
}
\arguments{
  \item{data}{
The input table must contain all relevant Biomet-Data for calculation as all radiation fluxes from all directions, relativ humidity (RH), air temperature (Ta), and wind speed.}
}
\value{
  \item{utci}{A numeric vector containing the calculated utci}
}
\references{
Fiala, D., G. Havenith, P. Broede, B. Kampmann and G. Jendritzky 2012: UTCI-Fiala multi-node model of human heat trans- fer and temperature regulation. – International Journal of Biometeorology 56 (3): 429-441
}
\author{
AlK akrug@mailbox.tu-berlin.de}
\seealso{
www.utci.org
}
\examples{
data(biomet);
utci <- calc_utci(biomet)
}
\keyword{utci}
\keyword{Biomet}
