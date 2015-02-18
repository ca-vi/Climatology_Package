\name{read.biomet}
\alias{read.biomet}
\title{
Reads in the three pieces of datasets which is created by our biomet-station.
}
\description{
The function takes the different tables and merges them. Then the less important or redundant columns get sorted out.
}
\usage{
read_BIOMET(file_bm1, file_bm2, file_wm, all = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_bm1}{
The path (relative or absolute) to the CR1000_BIOMET1.dat file.
}
  \item{file_bm2}{
The path (relative or absolute) to the CR1000_BIOMET2.dat file.
}
  \item{file_wm}{
The path (relative or absolute) to the CR1000_BIOMET2_Gill_Windmaster.dat file.
}
  \item{all}{
If all=TRUE there will be no sorting out the redundant and less important columns.
}
}
\details{
There can be some NAs in the first rows, due to merging the data.frames by TIMESTAMP. You should check the result in any cases.
}
\value{
A data.frame containing everything for \link{calc_mrt} , \link{calc_pmv} and \link{calc_utci}.
}
\references{

}
\author{
Alexander Krug
}
