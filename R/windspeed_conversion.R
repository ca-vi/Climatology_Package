windspeed_conversion <- function (x, unit=c("m/s","km/h","B","kn"), into=c("m/s","km/h","B","kn"), num=FALSE) {
  
  unitlist <- c("m/s","km/h","B","kn")  
  error_unit <- "ERROR Define the unit in which the starting value is: unit='' as argument with 'm/s', 'km/h', 'B' or 'kn'"
  error_into <- "ERROR The function shall convert into which unit? Define: into='' as argument with 'm/s', 'km/h', 'B' or 'kn'"
  
  if (length(unit) > 1 | length(into) > 1){
    if (length(unit) > 1 & length(into) > 1){
      return (cat(error_unit,"\n", error_into))
    } else if (length(unit) > 1){
      return (cat(error_unit))
    } else if (length(into) > 1){
      return (cat(error_into))
    }
  } else if (!(unit %in% unitlist)){
    return (cat(error_unit))
  } else if (!(into %in% unitlist)){
    return (cat(error_into))
  } else {
    
    unit <- which(unitlist %in% unit)
    into <- which(unitlist %in% into)
    
    if (unit==1) {
      m <- x
      k <- x*3.6
      B <- (x/0.836)^0.67
      kn <- 1.9438*x
    }
    if(unit==2){
      m <- x/3.6
      k <- x
      B <- (x/3.6/0.836)^0.67
      kn <- 1.9438*x/3.6
    }
    if(unit==3){
      m <- 0.836*x^1.5
      k <- (0.836*x^1.5)*3.6
      B <- x
      kn <- 1.9438*0.836*x^1.5
    }
    if(unit==4){
      m <- x/1.9438
      k <- x/1.9438*3.6
      B <- (x/1.9438)^0.67/0.836
      kn <- x
    }
    if (num){
      return(round(c(m,k,B,kn)[into],2))
    } else {
      return(paste(round(c(m,k,B,kn)[into],2), unitlist[into]))
    }
  }
}

cart2polar <- function(u,v) {
  d <- complex(real = -v, imaginary = -u)
  data.frame(r = Mod(d),  theta = (Arg(d) %% (2*pi) * 360/(2*pi)))
}