\name{windrose}
\alias{windrose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot.windrose
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
windrose(windspeed, winddir, title)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{windspeed}{
%%     ~~Describe \code{windspeed} here~~
}
  \item{winddir}{
%%     ~~Describe \code{winddir} here~~
}
  \item{title}{
%%     ~~Describe \code{winddir} here~~
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
function (windspeed, winddir, title = "Windrose der Station") 
{
    library(climatol)
    data(windfr)
    windv_class <- windfr * 0
    dirup <- c(11.25, 33.75, 56.25, 78.75, 101.25, 123.75, 146.25, 
        168.75, 191.25, 213.75, 236.25, 258.75, 281.25, 303.75, 
        326.25, 348.75)
    dirlow <- c(348.75, 11.25, 33.75, 56.25, 78.75, 101.25, 123.75, 
        146.25, 168.75, 191.25, 213.75, 236.25, 258.75, 281.25, 
        303.75, 326.25)
    speedup <- c(0.5, 1, 1.5, 2)
    speedlow <- (c(0, 0.5, 1, 1.5))
    rownames(windv_class) <- c(paste(toString(speedlow[1]), "-", 
        toString(speedup[1])), paste(toString(speedlow[2]), "-", 
        toString(speedup[2])), paste(toString(speedlow[3]), "-", 
        toString(speedup[3])), paste(">", toString(speedup[3])))
    for (a in seq_along(speedup)) {
        idx <- which(windspeed >= speedlow[a] & windspeed < speedup[a])
        for (b in seq_along(dirlow)) {
            if (b == 1) 
                cnt1 <- length(which(winddir[idx] >= dirlow[b]))
            if (b == 1) 
                cnt2 <- length(which(winddir[idx] < dirup[b]))
            if (b == 1) 
                cnt <- cnt1 + cnt2
            if (b > 1) 
                cnt <- length(which(winddir[idx] >= dirlow[b] & 
                  winddir[idx] < dirup[b]))
            windv_class[a, b] <- cnt
        }
    }
    rosavent(windv_class, 5, 10, ang = -3 * pi/16, main = title)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
