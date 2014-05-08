\name{calc_utci}
\alias{calc_utci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{utci
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_utci(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    Tmrt = calc_tmrt(x)
    g = c(-2836.5744, -6028.076559, 19.54263612, -0.02737830188, 
        1.6261698e-05, 7.0229056e-10, -1.8680009e-13, 2.7150305)
    tk = x$Ta + 273.15
    es = g[8] * log(tk)
    for (i in 1:7) {
        es = g[i] * (tk^(i - 3)) + es
    }
    es = exp(es) * 0.01
    ehPa = es * x$RH/100
    Pa = ehPa/10
    va = (log(10/0.01)/log(1.77/0.01)) * x$Wind_Speed
    Ta = x$Ta
    D_Tmrt = Tmrt - Ta
    UTCI = Ta + (0.607562052) + (-0.0227712343) * Ta + (0.000806470249) * 
        Ta * Ta + (-0.000154271372) * Ta * Ta * Ta + (-3.24651735e-06) * 
        Ta * Ta * Ta * Ta + (7.32602852e-08) * Ta * Ta * Ta * 
        Ta * Ta + (1.35959073e-09) * Ta * Ta * Ta * Ta * Ta * 
        Ta + (-2.2583652) * va + (0.0880326035) * Ta * va + (0.00216844454) * 
        Ta * Ta * va + (-1.53347087e-05) * Ta * Ta * Ta * va + 
        (-5.72983704e-07) * Ta * Ta * Ta * Ta * va + (-2.55090145e-09) * 
        Ta * Ta * Ta * Ta * Ta * va + (-0.751269505) * va * va + 
        (-0.00408350271) * Ta * va * va + (-5.21670675e-05) * 
        Ta * Ta * va * va + (1.94544667e-06) * Ta * Ta * Ta * 
        va * va + (1.14099531e-08) * Ta * Ta * Ta * Ta * va * 
        va + (0.158137256) * va * va * va + (-6.57263143e-05) * 
        Ta * va * va * va + (2.22697524e-07) * Ta * Ta * va * 
        va * va + (-4.16117031e-08) * Ta * Ta * Ta * va * va * 
        va + (-0.0127762753) * va * va * va * va + (9.66891875e-06) * 
        Ta * va * va * va * va + (2.52785852e-09) * Ta * Ta * 
        va * va * va * va + (0.000456306672) * va * va * va * 
        va * va + (-1.74202546e-07) * Ta * va * va * va * va * 
        va + (-5.91491269e-06) * va * va * va * va * va * va + 
        (0.398374029) * D_Tmrt + (0.000183945314) * Ta * D_Tmrt + 
        (-0.00017375451) * Ta * Ta * D_Tmrt + (-7.60781159e-07) * 
        Ta * Ta * Ta * D_Tmrt + (3.77830287e-08) * Ta * Ta * 
        Ta * Ta * D_Tmrt + (5.43079673e-10) * Ta * Ta * Ta * 
        Ta * Ta * D_Tmrt + (-0.0200518269) * va * D_Tmrt + (0.000892859837) * 
        Ta * va * D_Tmrt + (3.45433048e-06) * Ta * Ta * va * 
        D_Tmrt + (-3.77925774e-07) * Ta * Ta * Ta * va * D_Tmrt + 
        (-1.69699377e-09) * Ta * Ta * Ta * Ta * va * D_Tmrt + 
        (0.000169992415) * va * va * D_Tmrt + (-4.99204314e-05) * 
        Ta * va * va * D_Tmrt + (2.47417178e-07) * Ta * Ta * 
        va * va * D_Tmrt + (1.07596466e-08) * Ta * Ta * Ta * 
        va * va * D_Tmrt + (8.49242932e-05) * va * va * va * 
        D_Tmrt + (1.35191328e-06) * Ta * va * va * va * D_Tmrt + 
        (-6.21531254e-09) * Ta * Ta * va * va * va * D_Tmrt + 
        (-4.99410301e-06) * va * va * va * va * D_Tmrt + (-1.89489258e-08) * 
        Ta * va * va * va * va * D_Tmrt + (8.15300114e-08) * 
        va * va * va * va * va * D_Tmrt + (0.00075504309) * D_Tmrt * 
        D_Tmrt + (-5.65095215e-05) * Ta * D_Tmrt * D_Tmrt + (-4.52166564e-07) * 
        Ta * Ta * D_Tmrt * D_Tmrt + (2.46688878e-08) * Ta * Ta * 
        Ta * D_Tmrt * D_Tmrt + (2.42674348e-10) * Ta * Ta * Ta * 
        Ta * D_Tmrt * D_Tmrt + (0.00015454725) * va * D_Tmrt * 
        D_Tmrt + (5.2411097e-06) * Ta * va * D_Tmrt * D_Tmrt + 
        (-8.75874982e-08) * Ta * Ta * va * D_Tmrt * D_Tmrt + 
        (-1.50743064e-09) * Ta * Ta * Ta * va * D_Tmrt * D_Tmrt + 
        (-1.56236307e-05) * va * va * D_Tmrt * D_Tmrt + (-1.33895614e-07) * 
        Ta * va * va * D_Tmrt * D_Tmrt + (2.49709824e-09) * Ta * 
        Ta * va * va * D_Tmrt * D_Tmrt + (6.51711721e-07) * va * 
        va * va * D_Tmrt * D_Tmrt + (1.94960053e-09) * Ta * va * 
        va * va * D_Tmrt * D_Tmrt + (-1.00361113e-08) * va * 
        va * va * va * D_Tmrt * D_Tmrt + (-1.21206673e-05) * 
        D_Tmrt * D_Tmrt * D_Tmrt + (-2.1820366e-07) * Ta * D_Tmrt * 
        D_Tmrt * D_Tmrt + (7.51269482e-09) * Ta * Ta * D_Tmrt * 
        D_Tmrt * D_Tmrt + (9.79063848e-11) * Ta * Ta * Ta * D_Tmrt * 
        D_Tmrt * D_Tmrt + (1.25006734e-06) * va * D_Tmrt * D_Tmrt * 
        D_Tmrt + (-1.81584736e-09) * Ta * va * D_Tmrt * D_Tmrt * 
        D_Tmrt + (-3.52197671e-10) * Ta * Ta * va * D_Tmrt * 
        D_Tmrt * D_Tmrt + (-3.3651463e-08) * va * va * D_Tmrt * 
        D_Tmrt * D_Tmrt + (1.35908359e-10) * Ta * va * va * D_Tmrt * 
        D_Tmrt * D_Tmrt + (4.1703262e-10) * va * va * va * D_Tmrt * 
        D_Tmrt * D_Tmrt + (-1.30369025e-09) * D_Tmrt * D_Tmrt * 
        D_Tmrt * D_Tmrt + (4.13908461e-10) * Ta * D_Tmrt * D_Tmrt * 
        D_Tmrt * D_Tmrt + (9.22652254e-12) * Ta * Ta * D_Tmrt * 
        D_Tmrt * D_Tmrt * D_Tmrt + (-5.08220384e-09) * va * D_Tmrt * 
        D_Tmrt * D_Tmrt * D_Tmrt + (-2.24730961e-11) * Ta * va * 
        D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + (1.17139133e-10) * 
        va * va * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + (6.62154879e-10) * 
        D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + (4.0386326e-13) * 
        Ta * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + (1.95087203e-12) * 
        va * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + (-4.73602469e-12) * 
        D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + 
        (5.12733497) * Pa + (-0.312788561) * Ta * Pa + (-0.0196701861) * 
        Ta * Ta * Pa + (0.00099969087) * Ta * Ta * Ta * Pa + 
        (9.51738512e-06) * Ta * Ta * Ta * Ta * Pa + (-4.66426341e-07) * 
        Ta * Ta * Ta * Ta * Ta * Pa + (0.548050612) * va * Pa + 
        (-0.00330552823) * Ta * va * Pa + (-0.0016411944) * Ta * 
        Ta * va * Pa + (-5.16670694e-06) * Ta * Ta * Ta * va * 
        Pa + (9.52692432e-07) * Ta * Ta * Ta * Ta * va * Pa + 
        (-0.0429223622) * va * va * Pa + (0.00500845667) * Ta * 
        va * va * Pa + (1.00601257e-06) * Ta * Ta * va * va * 
        Pa + (-1.81748644e-06) * Ta * Ta * Ta * va * va * Pa + 
        (-0.00125813502) * va * va * va * Pa + (-0.000179330391) * 
        Ta * va * va * va * Pa + (2.34994441e-06) * Ta * Ta * 
        va * va * va * Pa + (0.000129735808) * va * va * va * 
        va * Pa + (1.2906487e-06) * Ta * va * va * va * va * 
        Pa + (-2.28558686e-06) * va * va * va * va * va * Pa + 
        (-0.0369476348) * D_Tmrt * Pa + (0.00162325322) * Ta * 
        D_Tmrt * Pa + (-3.1427968e-05) * Ta * Ta * D_Tmrt * Pa + 
        (2.59835559e-06) * Ta * Ta * Ta * D_Tmrt * Pa + (-4.77136523e-08) * 
        Ta * Ta * Ta * Ta * D_Tmrt * Pa + (0.0086420339) * va * 
        D_Tmrt * Pa + (-0.000687405181) * Ta * va * D_Tmrt * 
        Pa + (-9.13863872e-06) * Ta * Ta * va * D_Tmrt * Pa + 
        (5.15916806e-07) * Ta * Ta * Ta * va * D_Tmrt * Pa + 
        (-3.59217476e-05) * va * va * D_Tmrt * Pa + (3.28696511e-05) * 
        Ta * va * va * D_Tmrt * Pa + (-7.10542454e-07) * Ta * 
        Ta * va * va * D_Tmrt * Pa + (-1.243823e-05) * va * va * 
        va * D_Tmrt * Pa + (-7.385844e-09) * Ta * va * va * va * 
        D_Tmrt * Pa + (2.20609296e-07) * va * va * va * va * 
        D_Tmrt * Pa + (-0.00073246918) * D_Tmrt * D_Tmrt * Pa + 
        (-1.87381964e-05) * Ta * D_Tmrt * D_Tmrt * Pa + (4.80925239e-06) * 
        Ta * Ta * D_Tmrt * D_Tmrt * Pa + (-8.7549204e-08) * Ta * 
        Ta * Ta * D_Tmrt * D_Tmrt * Pa + (2.7786293e-05) * va * 
        D_Tmrt * D_Tmrt * Pa + (-5.06004592e-06) * Ta * va * 
        D_Tmrt * D_Tmrt * Pa + (1.14325367e-07) * Ta * Ta * va * 
        D_Tmrt * D_Tmrt * Pa + (2.53016723e-06) * va * va * D_Tmrt * 
        D_Tmrt * Pa + (-1.72857035e-08) * Ta * va * va * D_Tmrt * 
        D_Tmrt * Pa + (-3.95079398e-08) * va * va * va * D_Tmrt * 
        D_Tmrt * Pa + (-3.59413173e-07) * D_Tmrt * D_Tmrt * D_Tmrt * 
        Pa + (7.04388046e-07) * Ta * D_Tmrt * D_Tmrt * D_Tmrt * 
        Pa + (-1.89309167e-08) * Ta * Ta * D_Tmrt * D_Tmrt * 
        D_Tmrt * Pa + (-4.79768731e-07) * va * D_Tmrt * D_Tmrt * 
        D_Tmrt * Pa + (7.96079978e-09) * Ta * va * D_Tmrt * D_Tmrt * 
        D_Tmrt * Pa + (1.62897058e-09) * va * va * D_Tmrt * D_Tmrt * 
        D_Tmrt * Pa + (3.94367674e-08) * D_Tmrt * D_Tmrt * D_Tmrt * 
        D_Tmrt * Pa + (-1.18566247e-09) * Ta * D_Tmrt * D_Tmrt * 
        D_Tmrt * D_Tmrt * Pa + (3.34678041e-10) * va * D_Tmrt * 
        D_Tmrt * D_Tmrt * D_Tmrt * Pa + (-1.15606447e-10) * D_Tmrt * 
        D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * Pa + (-2.80626406) * 
        Pa * Pa + (0.548712484) * Ta * Pa * Pa + (-0.0039942841) * 
        Ta * Ta * Pa * Pa + (-0.000954009191) * Ta * Ta * Ta * 
        Pa * Pa + (1.93090978e-05) * Ta * Ta * Ta * Ta * Pa * 
        Pa + (-0.308806365) * va * Pa * Pa + (0.0116952364) * 
        Ta * va * Pa * Pa + (0.000495271903) * Ta * Ta * va * 
        Pa * Pa + (-1.90710882e-05) * Ta * Ta * Ta * va * Pa * 
        Pa + (0.00210787756) * va * va * Pa * Pa + (-0.000698445738) * 
        Ta * va * va * Pa * Pa + (2.30109073e-05) * Ta * Ta * 
        va * va * Pa * Pa + (0.00041785659) * va * va * va * 
        Pa * Pa + (-1.27043871e-05) * Ta * va * va * va * Pa * 
        Pa + (-3.04620472e-06) * va * va * va * va * Pa * Pa + 
        (0.0514507424) * D_Tmrt * Pa * Pa + (-0.00432510997) * 
        Ta * D_Tmrt * Pa * Pa + (8.99281156e-05) * Ta * Ta * 
        D_Tmrt * Pa * Pa + (-7.14663943e-07) * Ta * Ta * Ta * 
        D_Tmrt * Pa * Pa + (-0.000266016305) * va * D_Tmrt * 
        Pa * Pa + (0.000263789586) * Ta * va * D_Tmrt * Pa * 
        Pa + (-7.01199003e-06) * Ta * Ta * va * D_Tmrt * Pa * 
        Pa + (-0.000106823306) * va * va * D_Tmrt * Pa * Pa + 
        (3.61341136e-06) * Ta * va * va * D_Tmrt * Pa * Pa + 
        (2.29748967e-07) * va * va * va * D_Tmrt * Pa * Pa + 
        (0.000304788893) * D_Tmrt * D_Tmrt * Pa * Pa + (-6.42070836e-05) * 
        Ta * D_Tmrt * D_Tmrt * Pa * Pa + (1.16257971e-06) * Ta * 
        Ta * D_Tmrt * D_Tmrt * Pa * Pa + (7.68023384e-06) * va * 
        D_Tmrt * D_Tmrt * Pa * Pa + (-5.47446896e-07) * Ta * 
        va * D_Tmrt * D_Tmrt * Pa * Pa + (-3.5993791e-08) * va * 
        va * D_Tmrt * D_Tmrt * Pa * Pa + (-4.36497725e-06) * 
        D_Tmrt * D_Tmrt * D_Tmrt * Pa * Pa + (1.68737969e-07) * 
        Ta * D_Tmrt * D_Tmrt * D_Tmrt * Pa * Pa + (2.67489271e-08) * 
        va * D_Tmrt * D_Tmrt * D_Tmrt * Pa * Pa + (3.23926897e-09) * 
        D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * Pa * Pa + (-0.0353874123) * 
        Pa * Pa * Pa + (-0.22120119) * Ta * Pa * Pa * Pa + (0.0155126038) * 
        Ta * Ta * Pa * Pa * Pa + (-0.000263917279) * Ta * Ta * 
        Ta * Pa * Pa * Pa + (0.0453433455) * va * Pa * Pa * Pa + 
        (-0.00432943862) * Ta * va * Pa * Pa * Pa + (0.000145389826) * 
        Ta * Ta * va * Pa * Pa * Pa + (0.00021750861) * va * 
        va * Pa * Pa * Pa + (-6.66724702e-05) * Ta * va * va * 
        Pa * Pa * Pa + (3.3321714e-05) * va * va * va * Pa * 
        Pa * Pa + (-0.00226921615) * D_Tmrt * Pa * Pa * Pa + 
        (0.000380261982) * Ta * D_Tmrt * Pa * Pa * Pa + (-5.45314314e-09) * 
        Ta * Ta * D_Tmrt * Pa * Pa * Pa + (-0.000796355448) * 
        va * D_Tmrt * Pa * Pa * Pa + (2.53458034e-05) * Ta * 
        va * D_Tmrt * Pa * Pa * Pa + (-6.31223658e-06) * va * 
        va * D_Tmrt * Pa * Pa * Pa + (0.000302122035) * D_Tmrt * 
        D_Tmrt * Pa * Pa * Pa + (-4.77403547e-06) * Ta * D_Tmrt * 
        D_Tmrt * Pa * Pa * Pa + (1.73825715e-06) * va * D_Tmrt * 
        D_Tmrt * Pa * Pa * Pa + (-4.09087898e-07) * D_Tmrt * 
        D_Tmrt * D_Tmrt * Pa * Pa * Pa + (0.614155345) * Pa * 
        Pa * Pa * Pa + (-0.0616755931) * Ta * Pa * Pa * Pa * 
        Pa + (0.00133374846) * Ta * Ta * Pa * Pa * Pa * Pa + 
        (0.00355375387) * va * Pa * Pa * Pa * Pa + (-0.000513027851) * 
        Ta * va * Pa * Pa * Pa * Pa + (0.000102449757) * va * 
        va * Pa * Pa * Pa * Pa + (-0.00148526421) * D_Tmrt * 
        Pa * Pa * Pa * Pa + (-4.11469183e-05) * Ta * D_Tmrt * 
        Pa * Pa * Pa * Pa + (-6.80434415e-06) * va * D_Tmrt * 
        Pa * Pa * Pa * Pa + (-9.77675906e-06) * D_Tmrt * D_Tmrt * 
        Pa * Pa * Pa * Pa + (0.0882773108) * Pa * Pa * Pa * Pa * 
        Pa + (-0.00301859306) * Ta * Pa * Pa * Pa * Pa * Pa + 
        (0.00104452989) * va * Pa * Pa * Pa * Pa * Pa + (0.000247090539) * 
        D_Tmrt * Pa * Pa * Pa * Pa * Pa + (0.00148348065) * Pa * 
        Pa * Pa * Pa * Pa * Pa
    return(UTCI)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
