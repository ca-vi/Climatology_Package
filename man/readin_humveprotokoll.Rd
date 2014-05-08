\name{readin_humveprotokoll}
\alias{readin_humveprotokoll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{protokoll
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readin_humveprotokoll(file, info = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{info}{
%%     ~~Describe \code{info} here~~
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
function (file, info = TRUE) 
{
    data <- read.csv2(file, skip = 5)
    datum <- as.Date(read.csv2(file)[2, 6], format = "\%d.\%m.\%Y")
    data$beginn <- strptime(paste(datum, data$beginn), format = "\%Y-\%m-\%d \%H:\%M:\%S")
    data$ende <- strptime(paste(datum, data$ende), format = "\%Y-\%m-\%d \%H:\%M:\%S")
    data$bemerkungen <- as.character(data$bemerkungen)
    if (info == TRUE) 
        print(str(data))
    data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
