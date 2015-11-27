\name{calc_pmv}
\alias{calc_pmv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the Predicted Mean Vote (PMV) Value
}
\description{
This function calculates the pmv value for a given input data
}
\usage{
calc_pmv(data, clo, met)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The input table must contain all relevant Biomet-Data for calculation as all radiation fluxes from all directions, relativ humidity (RH), air temperature (Ta), and wind speed. }
  \item{clo}{clo-factor for insulation of clothes. See in VDI-Richtlinen for further informations. Optional argument. Default is set to 0.6 for light summer clothes}
  \item{met}{met-factor for metabolic rate depending to humans activity. See in VDI-Richtlinen for further informations. Optional argument. Default is set so 2 for a slowly walking person.}
}
\details{
For fields with NA-Value(s) (missing data) it isn't possible to calculate pmv and leads to an error message. Please clean your Input from NA-Values. See \link{complete.cases}}
\value{
Predicted Mean Vote (PMV) according to Fanger, 1970 
}
\references{
VDI 3787 Blatt 2
}
\author{
AlK akrug@mailbox.tu-berlin.de
}

\examples{
data(biomet);
pmv <- calc_pmv(biomet)
}
\keyword{Biomet}
\keyword{pmv}