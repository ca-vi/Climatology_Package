\name{humve_stationszuordnung_mittelwerte}
\alias{humve_stationszuordnung_mittelwerte}
\title{HuMVe data will be cut according to the log.}
\description{
The Human Response Meteorological Vehicle HuMVe is a movable meteorological station measuring different weather elements. Due to this movability only few data can be used for e.g. wind analysis. If a consistent log is written the big data table can be cut in a smaller one containing the station number where the vehicle was standing instead of a Recordnumber.}
\usage{
humve_stationszuordnung_mittelwerte(humvedata_meteo, humvedata_wind, humvedata_gill, logdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{humvedata_meteo}{
The "CR1000_HUMVE_Meteo.dat"" which was loaded with \link{read.logger}.
}
  \item{humvedata_wind}{
The "CR1000_HUMVE_Wind.dat"" which was loaded with \link{read.logger}.
}
  \item{humvedata_gill}{
The "CR1000_HUMVE_Gill_Windmaster.dat"" which was loaded with \link{read.logger}.
}
  \item{logdata}{
The logdata which was loaded with \link{read.humvelog}
}
}
\details{
This function gives out values which are aggregated. The WS and WD values were newly calculated using \link{cart2polar} on the U and V Vectors.
}
\value{
Gives a aggregated and cutted data.frame with a 2nd column named station rather then RECORD
}

\author{
Carsten Vick
}
