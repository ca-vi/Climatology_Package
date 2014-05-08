\name{read_BIOMET}
\alias{read_BIOMET}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{BioMet Station
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read_BIOMET(file_BM1, file_BM2, file_WM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_BM1}{
%%     ~~Describe \code{file_BM1} here~~
}
  \item{file_BM2}{
%%     ~~Describe \code{file_BM2} here~~
}
  \item{file_WM}{
%%     ~~Describe \code{file_WM} here~~
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
function (file_BM1, file_BM2, file_WM) 
{
    data_BM1 <- read.table(file_BM1, sep = ",", dec = ".", skip = 4, 
        col.names = scan(file_BM1, skip = 1, nlines = 1, sep = ",", 
            what = character()))
    data_BM2 <- read.table(file_BM2, sep = ",", dec = ".", skip = 4, 
        col.names = scan(file_BM2, skip = 1, nlines = 1, sep = ",", 
            what = character()))
    data_WM <- read.table(file_WM, sep = ",", dec = ".", skip = 4, 
        col.names = scan(file_WM, skip = 1, nlines = 1, sep = ",", 
            what = character()))
    data1 = merge(data_BM1, data_BM2, by = "TIMESTAMP", all.x = T, 
        )
    data = merge(data1, data_WM, by = "TIMESTAMP", all.x = T)
    LW_west <- data$LW_west_meas + (5.67e-08 * ((data$T_CNR4_SN121043 + 
        273.15)^4))
    data <- cbind(data, LW_west)
    return(data)
    message("Done :-)")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
