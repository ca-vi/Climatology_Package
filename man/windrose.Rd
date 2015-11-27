\name{windrose}
\alias{windrose}
\title{Sorting windspeed and winddirection in classes and ploting a windrose.}
\description{
To plot a windrose the data has to be classified. Here we use 16 winddirection classes and 4 windspeed classes.
}
\usage{
windrose(windspeed, winddir, r=5, p=10, title=NULL, ...)
}
\arguments{
  \item{windspeed}{A vector of windspeeds}
  \item{winddir}{A vector of winddirections}
  \item{r}{The number of circles drawn}
  \item{p}{The percentage jumps between each circle}
  \item{title}{Title in Plotwindow}
}
\details{
A frequency table is generated and given to plotWindrose, which will create a plot.
}
\value{
Plotting takes place
}\author{Britta Jaenicke and Carsten Vick}

\seealso{
\link{read.logger}
}
\examples{
data(AWS_wind)
windrose(windspeed = AWS_wind$Wind_Speed, winddir = AWS_wind$Wind_Direction)
}