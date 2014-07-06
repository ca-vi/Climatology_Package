\name{calc_utci}
\alias{calc_utci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the Universal Thermal Climate Index (UTCI) Value
%%  ~~function to do ... ~~
}
\description{
This function calculates the UTCI value for a given input data
}
\usage{
calc_utci(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The input table must contain all relevant Biomet-Data for calculation as all radiation fluxes from all directions, relativ humidity (RH), air temperature (Ta), and wind speed.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Fiala, D., G. Havenith, P. Bröde, B. Kampmann and G. Jendritzky 2012: UTCI-Fiala multi-node model of human heat trans- fer and temperature regulation. – International Journal of Biometeorology 56 (3): 429-441
}
\author{
AlK akrug@mailbox.tu-berlin.de}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
www.utci.org
}
\examples{

utci = calc_utci (x) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utci }
\keyword{ Biomet }% __ONLY ONE__ keyword per line
