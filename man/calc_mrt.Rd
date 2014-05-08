\name{calc_mrt}
\alias{calc_mrt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of the Mean Radiant Temperature (MRT)
}
\description{
Calculates the Mean Radiant Temperature (MRT) from HRS-Data. Based on Thorsson et al. 2007 (DOI: 10.1002/joc.1537)
}
\usage{
calc_mrt(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A table containing radiation fluxes (short- and longwave) from all cardinal points. 
}
}
\details{
The body's emissivity is set so 0.97.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Returns the MRT-Value in degree Celsius
}
\author{
FG Klimatologie (TU-Berlin), Alexander Krug
}
\note{
Please use the "standard" names for the variables of the HRS of 2014. If anything is changed there (update of the logger script), this function as well as the whole biomet-package have to be adapt!}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Thorsson et al. (2007), Different methods for estimating the mean radiant temperature in an outdoor urban setting.
}
\examples{

## The function is currently defined as
function (x) 
{
    F_i_h = 0.22
    F_i_v = 0.06
    alpha = 0.7
    epsylon = 0.97
    sbc = 5.67e-08
    SW_UP = x$SW_upwelling
    SW_DOWN = x$SW_downwelling
    SW_EAST = x$SW_east
    SW_WEST = x$SW_west
    SW_NORTH = x$SW_north
    SW_SOUTH = x$SW_south
    LW_UP = x$LW_upwelling
    LW_DOWN = x$LW_downwelling
    LW_EAST = x$LW_east
    LW_WEST = x$LW_west
    LW_NORTH = x$LW_nouth
    LW_SOUTH = x$LW_sorth
    S_str_SW = alpha * ((F_i_v * (SW_UP + SW_DOWN)) + (F_i_h * 
        (SW_EAST + SW_WEST + SW_NORTH + SW_SOUTH)))
    S_str_LW = epsylon * ((F_i_v * (LW_UP + LW_DOWN)) + (F_i_h * 
        (LW_EAST + LW_WEST + LW_NORTH + LW_SOUTH)))
    S_str = S_str_SW + S_str_LW
    mrt = round(((S_str/(epsylon * sbc))^(0.25)) - 273.15, 2)
    return(mrt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
