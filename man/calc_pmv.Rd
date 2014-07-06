\name{calc_pmv}
\alias{calc_pmv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pmv
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_pmv(x)
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
    tmrt = calc_tmrt(x)
    pa = x$RH * 10 * exp(16.6536 - 4030.183/(x$Ta + 235))
    clo = 0.6
    met = 1.9
    m = met * 58.15
    wme = 0
    w = wme * 58.15
    mw = m - w
    icl = 0.155 * clo
    fcl = if (icl <= 0.078) {
        1 + 1.29 * icl
    }
    else {
        1.05 + 0.645 * icl
    }
    hcf = 12.1 * sqrt(x$Wind_Speed)
    ta_k = x$Ta + 273.15
    tr_k = tmrt + 273.15
    tcla = ta_k + (35.5 - x$Ta)/(3.5 * (6.45 * icl + 0.1))
    p1 = icl * fcl
    p2 = p1 * 3.96
    p3 = p1 * 100
    p4 = p1 * ta_k
    p5 = 308.7 - 0.028 * mw + p2 * (tr_k/100)^4
    xn = tcla/100
    xf = xn
    n = 0
    eps = 0.00015
    while (abs(xn[1] - xf[1]) < eps) {
        xf = (xf + xn)/2
        hcn = 2.38 * abs(100 * xf - ta_k)^0.25
        hc = ifelse(hcf > hcn, hcf, hcn)
        xn = (p5 + p4 * hc - p2 * xf^4)/(100 + p3 * hc)
        n = n + 1
        if (n > 150) {
            pmv = -9999
        }
    }
    tcl = 100 * xn - 273.15
    hl1 = 3.05 * 0.001 * (5733 - 6.99 - mw - pa)
    if (mw > 58.15) {
        hl2 = 0.42 * (mw - 58.15)
    }
    else {
        hl2 = 0
    }
    hl3 = 1.7 * 1e-05 * m * (5867 - pa)
    hl4 = 0.0014 * m * (34 - x$Ta)
    hl5 = 3.96 * fcl * (xn^4 - (tr_k/100)^4)
    hl6 = fcl * hc * (tcl - x$Ta)
    ts = 0.028 + 0.303 * exp(-0.036 * m)
    pmv = ts * (mw - hl1 - hl2 - hl3 - hl4 - hl5 - hl6)
    return(pmv)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
