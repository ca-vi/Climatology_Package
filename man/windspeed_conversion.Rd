\name{windspeed_conversion}
\alias{windspeed_conversion}
\title{Function to convert between different windspeed units.
}
\description{The Function takes a value and converts it into a new unit. By adressing "unit" and "into" the former as well as the new unit are defined.
}
\usage{
windspeed_conversion(x, unit=c("m/s","km/h","B","kn"), into=c("m/s","km/h","B","kn"), num=FALSE)
}
\arguments{
  \item{x}{The numeric value to convert}
  \item{unit}{The input unit. Choose from "m/s","km/h","B","kn"}
  \item{into}{The output unit. Choose from "m/s","km/h","B","kn"}
  \item{num}{A logical parameter indicating whether the returned value should be numeric or a character object with number and unit}
}
\value{If num is FALSE (default) paste() is called to write the number and the unit together in one character string. Else only the number is returned.}

\author{Carsten Vick (carsten.vick@campus.tu-berlin.de)}

\license{GPL3}

