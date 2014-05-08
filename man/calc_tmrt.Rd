\name{calc_tmrt}
\alias{calc_tmrt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{tmrt
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_tmrt(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

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
    LW_NORTH = x$LW_south
    LW_SOUTH = x$LW_north
    S_str_SW = alpha * ((F_i_v * (SW_UP + SW_DOWN)) + (F_i_h * 
        (SW_EAST + SW_WEST + SW_NORTH + SW_SOUTH)))
    S_Str_LW = epsylon * ((F_i_v * (LW_UP + LW_DOWN)) + (F_i_h * 
        (LW_EAST + LW_WEST + LW_NORTH + LW_SOUTH)))
    S_str = S_str_SW + S_Str_LW
    Tmrt = ((S_str/(epsylon * sbc))^(0.25)) - 273.15
    return(Tmrt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
