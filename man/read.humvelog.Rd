\name{read.humvelog}
\alias{read.humvelog}
\title{Reads in the handwritten and digitalized log for processing HuMVe Data}
\description{The date has to be written in 2nd row, 6th column: (file)[2,6]}
\usage{
read.humvelog(file, info = TRUE, ...)
}
\arguments{
  \item{file}{A .csv file saved from excel. To Do: Create an Excel Connection using package XLConnect}
  \item{info}{If TRUE prints the str()}
  \item{\dots}{More arguments given to read.table}
}
\value{
\item{data}{The data.frame with rows and columns with TIMESTAMPS in \link{POSIXct} structure.}
}
\author{
Carsten Vick
}