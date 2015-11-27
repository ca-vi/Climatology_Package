\name{FGClimatology-package}
\alias{FGClimatology-package}
\alias{FGClimatology}
\docType{package}
\title{Package for the climatology institute at TU Berlin. Science and teaching purposes intended.}
\description{
This package handles various functions and examples needed for working with climatological data gathered from different stationstypes at out institute. We use Campbell Scientific Datalogger and the read out .dat files with TOA5 long header can be easily read in and preprocessed using this pacakge.
}
\details{
\tabular{ll}{
Package: \tab FGClimatology\cr
Type: \tab Package\cr
Version: \tab 0.8\cr
Date: \tab 2015-02-18\cr
License: \tab GPL-3\cr
}
}
\author{
Britta Jaenicke;
Alexander Krug;
Carsten Vick;
Maintainer: Carsten Vick <carsten.vick@campus.tu-berlin.de>
}
\keyword{TU Berlin Climatology}
\examples{
data(biomet);
mrt <- calc_mrt(biomet)
}
