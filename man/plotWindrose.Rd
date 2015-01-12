\name{plotWindrose}
\alias{plotWindrose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotWindrose help function called by windrose
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotWindrose(frec, fnum = 4, fint = 5, flab = 2, ang = 3 * pi/16, col = rainbow(10, 0.5, 0.92, start = 0.33, end = 0.2), margen = c(0, 0, 1, 0), key = TRUE, uni = "m/s", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frec}{
%%     ~~Describe \code{frec} here~~
}
  \item{fnum}{
%%     ~~Describe \code{fnum} here~~
}
  \item{fint}{
%%     ~~Describe \code{fint} here~~
}
  \item{flab}{
%%     ~~Describe \code{flab} here~~
}
  \item{ang}{
%%     ~~Describe \code{ang} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{margen}{
%%     ~~Describe \code{margen} here~~
}
  \item{key}{
%%     ~~Describe \code{key} here~~
}
  \item{uni}{
%%     ~~Describe \code{uni} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (frec, fnum = 4, fint = 5, flab = 2, ang = 3 * pi/16, 
    col = rainbow(10, 0.5, 0.92, start = 0.33, end = 0.2), margen = c(0, 
        0, 1, 0), key = TRUE, uni = "m/s", ...) 
{
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    if (is.matrix(frec)) 
        frec <- as.data.frame(frec)
    if (is.vector(frec)) {
        ndir <- length(frec)
        nr <- 1
    }
    else {
        ndir <- length(frec[1, ])
        nr <- nrow(frec)
    }
    fmax <- fnum * fint
    tot <- sum(frec)
    fr <- 100 * frec/tot
    key <- (nr > 1) && key
    if (key) 
        mlf <- 3
    else mlf <- 1
    par(mar = margen)
    fx <- cos(pi/2 - (2 * pi/ndir * 0:(ndir - 1)))
    fy <- sin(pi/2 - (2 * pi/ndir * 0:(ndir - 1)))
    plot(fx, fy, xlim = c(-fmax - mlf * fint, fmax + fint), ylim = c(-fmax - 
        fint, fmax + fint), xaxt = "n", yaxt = "n", xlab = "", 
        ylab = "", bty = "n", asp = 1, type = "n", ...)
    if (nr == 1) {
        cx <- fx * fr
        cy <- fy * fr
    }
    else {
        f <- apply(fr, 2, sum)
        cx <- fx * f
        cy <- fy * f
        for (i in nr:2) {
            f <- f - fr[i, ]
            cx <- c(cx, NA, fx * f)
            cy <- c(cy, NA, fy * f)
        }
    }
    polygon(cx, cy, col = col[nr:1])
    symbols(c(0 * 1:fnum), c(0 * 1:fnum), circles = c(fint * 
        1:fnum), inches = FALSE, add = TRUE)
    segments(0 * 1:ndir, 0 * 1:ndir, fmax * fx, fmax * fy)
    fmaxi <- fmax + fint/4
    text(0, fmaxi, "N")
    text(0, -fmaxi, "S")
    text(fmaxi, 0, "E")
    text(-fmaxi, 0, "W")
    if (flab == 2) 
        for (i in 1:fnum) text(i * fint * cos(ang), i * fint * 
            sin(ang), paste(i * fint, "\%"))
    else if (flab == 1) 
        text(fmax * cos(ang), fmax * sin(ang), paste(fmax, "\%"))
    if (key) {
        legend(-fmaxi - 2.3 * fint, fmaxi + 2, fill = col, legend = attr(frec, 
            "row.names"))
        text(-fmaxi - 1.4 * fint, fmaxi + 0.9 * fint, uni)
    }
    invisible()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
