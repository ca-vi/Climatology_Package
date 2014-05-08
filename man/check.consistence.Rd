\name{check.consistence}
\alias{check.consistence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check.consistence(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (data) 
{
    rn_fehler <- 0
    ts_diff <- difftime(data$TIMESTAMP[2], data$TIMESTAMP[1], 
        units = "secs")
    ts_fehler <- 0
    for (i in 1:(length(data$RECORD) - 1)) {
        if (data$RECORD[i + 1] - data$RECORD[i] != 1) 
            rn_fehler <- c(rn_fehler, i)
        if (difftime(data$TIMESTAMP[i + 1], data$TIMESTAMP[i]) != 
            ts_diff) {
            ts_fehler <- c(ts_fehler, i)
        }
        else {
            ts_diff <- difftime(data$TIMESTAMP[i + 1], data$TIMESTAMP[i], 
                units = "secs")
        }
    }
    if (length(rn_fehler) > 1 & info == TRUE) {
        cat("\n Fehler in Recordnumber in ZEILE:\n", rn_fehler, 
            file = "fehlerzeilen.txt")
        print(cat("!ACHTUNG!: Fehler in Recordnumber in ZEILE:", 
            rn_fehler, "\n"))
    }
    if (length(ts_fehler) > 1 & info == TRUE) {
        cat("\n Fehler in Timestamp in ZEILE:\n", ts_fehler, 
            file = "fehlerzeilen.txt", append = TRUE)
        print(cat("!ACHTUNG!: Fehler in Timestamp in ZEILE:", 
            ts_fehler, "\n"))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
