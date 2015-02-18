\name{plotWindrose}
\alias{plotWindrose}
\title{plotWindrose help function called by windrose
}
\description{
This function was extracted from the no longer supported climatol package. It creates a windrose with bluish colors.
}
\usage{
plotWindrose(frec, fnum = 4, fint = 5, flab = 2, ang = 3 * pi/16, col = rainbow(10, 0.5, 0.92, start = 0.33, end = 0.2), margen = c(0, 0, 1, 0), key = TRUE, uni = "m/s", ...)
}
\arguments{
  \item{frec}{
This has to be a matrix or a data.frame containing frequencies of different classes (like a histogramm). It can be 2-dimensional. (e.g. windspeed and winddirection)
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
A plot is returned.
}
\references{
José A. Guijarro <jguijarrop@aemet.es>
}
\author{
José A. Guijarro <jguijarrop@aemet.es>
}

\seealso{
\link{cart2polar}
\link{windrose}
}
\examples{

}