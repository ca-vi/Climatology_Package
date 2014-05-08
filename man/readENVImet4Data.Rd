\name{readENVImet4Data}
\alias{readENVImet4Data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to read ENVImet V4 files}
\description{
Function reads in data structure of .edi- and edt.- files and outputs a datacontainer with the dimensions x-,y-,z,variable}
\usage{
readENVImet4Data(metafile, datafile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{metafile}{
path to the metafile
}
  \item{datafile}{
path to the datafile
}
}
\details{
%%  If necessary, more details than the description above 
}
\value{
Data Matrix with Dimensions: x,y,z,variable
}
\references{
www.envimet.info}
\author{
AlK}
\note{
ToDo
- Merge functions for Envi-met 3 and 4 with if request 
- Include (early Stage) Code for reading whole file structures (AlK)
}


\seealso{

}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (metafile, datafile) 
{
    meta = readLines(metafile)
    cat(meta[2])
    n_x_grids = strtoi(meta[3])
    n_y_grids = strtoi(meta[4])
    n_z_grids = strtoi(meta[5])
    n_variables = strtoi(meta[6])
    dim = c(n_x_grids, n_y_grids, n_z_grids, n_variables)
    EMrawData = readBin(datafile, what = "numeric", n = file.info(datafile)$size, 
        size = 4)
    EMdata = array(EMrawData, dim = dim)
    cat(" OK Variables read:", n_variables, "\n")
    return(EMdata)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
