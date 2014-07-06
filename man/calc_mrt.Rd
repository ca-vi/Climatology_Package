\name{calc_mrt}
\alias{calc_mrt}

\title{Calculation of the Mean Radiant Temperatur (MRT)
}
\description{
This function calculates the mean radiant temperature (mrt) from radiation long- and shortwave fluxes of all directions according to Thorsson et al. 2007. %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_mrt(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
This functions need an input table with all fluxes from all directions for instance as it comes from a human response station.}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ 'mean radiant temperature'}
\keyword{ mrt }% __ONLY ONE__ keyword per line
