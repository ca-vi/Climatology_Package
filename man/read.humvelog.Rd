\name{read.humvelog}
\alias{read.humvelog}
\title{Reads in the handwritten and digitalized log for processing HuMVe Data}
\description{The date has to be written in 2nd row, 6th column: (file)[2,6]}
\usage{
read.humvelog(file, info = TRUE, ...)
}
\arguments{
  \item{file}{Excel file, where all humve-standing times are logged. Dont forget to fill in the Date.}
  \item{info}{If TRUE prints the str()}
  \item{\dots}{More arguments given to read.table}
}
\value{
\item{data}{The data.frame with rows and columns with TIMESTAMPS in \link{POSIXct} structure.}
}
\author{
Carsten Vick
}