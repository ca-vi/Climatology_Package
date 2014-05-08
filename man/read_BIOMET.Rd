\name{read_BIOMET}
\alias{read_BIOMET}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read the raw files of the HRS
}
\description{
This function reads the raw files of the Human Response Station (HRS). It only needs the pathes of the three logger files of the Station to create a table containing all variables. It automatically finds the common timestamp to combine these files.
}
\usage{
read_BIOMET(file_BM1, file_BM2, file_WM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_BM1}{
Path to the logger Biomet1 file.
}
  \item{file_BM2}{
Path to the logger Biomet2 file.
}
  \item{file_WM}{
Path to the logger Gill-Master-Wind file.
}
}
\details{
It doesn't actually matter witch file is set first, but it is recommented to use this file with the longest written time period.
}
\value{
Returns a table with the merged timestamp as y-axis and the data variables as x-axis.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
FG Klimatologie (TU-Berlin), Alexander Krug
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## The function is currently defined as
function (file_BM1, file_BM2, file_WM) 
{
    if (exists("file_BM1") & exists("file_BM2") & exists("file_WM")) 
        message(":) Great, all needed Files were loaded!")
    else message(":(  Please Check Input Files")
    data_BM1 <- read.table(file_BM1, sep = ",", dec = ".", skip = 4, 
        na.strings = "NAN", col.names = scan(file_BM1, skip = 1, 
            nlines = 1, sep = ",", what = character()))
    data_BM2 <- read.table(file_BM2, sep = ",", dec = ".", skip = 4, 
        na.strings = "NAN", col.names = scan(file_BM2, skip = 1, 
            nlines = 1, sep = ",", what = character()))
    data_WM <- read.table(file_WM, sep = ",", dec = ".", skip = 4, 
        na.strings = "NAN", col.names = scan(file_WM, skip = 1, 
            nlines = 1, sep = ",", what = character()))
    data1 = merge(data_BM1, data_BM2, by = "TIMESTAMP", all.x = T)
    data = merge(data1, data_WM, by = "TIMESTAMP", all.x = T)
    LW_west <- data$LW_west_meas + 5.67e-08 * (data$T_CNR4_SN121043 + 
        273.15)^4
    data <- cbind(data, LW_west)
    return(data)
    message("Done :-)")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
