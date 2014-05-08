\name{readENVImet3Data}
\alias{readENVImet3Data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to read ENVImet V3.1 files}
\description{
Function reads in data structure of .edi- and edt.- files and outputs a datacontainer with the dimensions x-,y-,z,variable}
\usage{
readENVImet3Data(metafile, datafile)
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
\examples{

## The function is currently defined as
function (metafile, datafile) 
{
    meta = readLines(metafile)
    cat(meta[1])
    n_x_grids = strtoi(meta[2])
    n_y_grids = strtoi(meta[3])
    n_z_grids = strtoi(meta[4])
    n_variables = strtoi(meta[5])
    dim = c(n_x_grids, n_y_grids, n_z_grids, n_variables)
    EMrawData = readBin(datafile, what = "numeric", n = file.info(datafile)$size, 
        size = 4)
    EMdata = array(EMrawData, dim = dim)
    cat(" OK Variables read:", n_variables, "\n")
    return(EMdata)
  }
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
