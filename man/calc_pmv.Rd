\name{calc_pmv}
\alias{calc_pmv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the Predicted Mean Vote (PMV) Value
}
\description{
This function calculates the pmv value for a given input data}
\usage{
calc_pmv(BIOMET-Data, clo, met)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BIOMET-Data}{
The input table must contain all relevant Biomet-Data for calculation as all radiation fluxes from all directions, relativ humidity (RH), air temperature (Ta), and wind speed. }
  \item{clo}{clo-factor for insulation of clothes. See in VDI-Richtlinen for further informations. Optional argument. Default is set to 0.6 for light summer clothes}
  \item{met}{met-factor for metabolic rate depending to humans activity. See in VDI-Richtlinen for further informations. Optional argument. Default is set so 2 for a slowly walking person.}
}
\details{
For fields with NA-Value(s) (missing data) it isn't possible to calculate pmv and leads to an error message. Please clean your Input from NA-Values.}
\value{
Predicted Mean Vote (PMV) according to Fanger, 1970 
}
\references{
VDI 3787 Blatt 2}
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

## The function is currently defined as

pmv = calc_pmv(BIOMET-Datatable, clo, met)

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Biomet}
\keyword{pmv}%   __ONLY ONE__ keyword per line
