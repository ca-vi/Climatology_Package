\name{cart2polar}
\alias{cart2polar}
\title{Function to convert from cartesian coordinates to polar coordinates.
}
\description{The Function takes the u and v vectors of e.g. wind data in WMO convention and converts it into polar coordinates usable for polar plots e.g. windroses. NOTE: the polar coordinates are exactly vice versa in direction, to be usable for a windrose plot. See \link{windrose}
}
\usage{
cart2polar(u, v)
}
\arguments{
  \item{u}{The Vector in u-Direction (WMO convention is East)}
  \item{v}{The Vector in v-Direction (WMO convention is North)}
}
\value{You get a data.frame with two columns. First column named "r" is the Modulus (combined Vectorlength), second column named "theta" is the Argument(degree, where v is 0° and u is 90°)}

\author{Carsten Vick (carsten.vick@campus.tu-berlin.de)}
\examples{
data(AWS_fluxes);
wind_polar <- cart2polar(u = AWS_fluxes$u_Avg, v = AWS_fluxes$v_Avg);
windrose(windspeed = wind_polar$r, winddir = wind_polar$theta)
}