humve_stationszuordnung_mittelwerte <- function (humvedata_meteo, humvedata_wind, humvedata_gill, logdata) {
  
  # new.data wird die Ausgabedatei:
  new.data <-as.data.frame(matrix(nrow=length(logdata$Station),ncol=18))
  names(new.data) <- c("TIMESTAMP","Station","Ta_150cm","RH_150cm","NETRAD","KWO","KWU","IRTS","ANGX","ANGY","WS","WD","SIGMA_WD","U","V","W","Tv","KT19")
  
  # damit alle die gleiche länge haben: (sonst probleme beim cbind)
  humvedata_meteo <-  humvedata_meteo [humvedata_meteo$TIMESTAMP  >= min(logdata$Beginn) & humvedata_meteo$TIMESTAMP  <= max(logdata$Ende),]
  humvedata_wind  <-  humvedata_wind  [humvedata_wind$TIMESTAMP   >= min(logdata$Beginn) & humvedata_wind$TIMESTAMP   <= max(logdata$Ende),]
  humvedata_gill  <-  humvedata_gill  [humvedata_gill$TIMESTAMP   >= min(logdata$Beginn) & humvedata_gill$TIMESTAMP   <= max(logdata$Ende),]
  
  

  for (i in seq_along(logdata$Station) )  {
    standzeitmittelung_daten <- sapply(sapply (cbind(humvedata_meteo[,(3:7)],humvedata_meteo[,(10:12)],humvedata_wind[,3:5],humvedata_gill[,3:4],humvedata_gill[,7:8]) [ humvedata_meteo$TIMESTAMP >= logdata$Beginn[i] & humvedata_meteo$TIMESTAMP <= logdata$Ende[i] ,  ] , mean), round, 2)

    new.data[i,(3:17)] <- standzeitmittelung_daten
    }
  new.data[,11:12] <- cart2polar(u = new.data[,14], v = new.data[,15])
  new.data$Station <- logdata$Station
  new.data$TIMESTAMP <- logdata$Ende
  new.data$KT19 <- logdata$KT.19
  print(str(new.data))
  invisible(new.data)
}

humve_stationszuordnung_ungemittelt <- function (humvedata_meteo, humvedata_wind, humvedata_gill, logdata) {
  
  # damit alle die gleiche länge haben (sonst probleme beim cbind)
  humvedata_meteo <-  humvedata_meteo [humvedata_meteo$TIMESTAMP  >= min(logdata$Beginn) & humvedata_meteo$TIMESTAMP  <= max(logdata$Ende),]
  humvedata_wind  <-  humvedata_wind  [humvedata_wind$TIMESTAMP   >= min(logdata$Beginn) & humvedata_wind$TIMESTAMP   <= max(logdata$Ende),]
  humvedata_gill  <-  humvedata_gill  [humvedata_gill$TIMESTAMP   >= min(logdata$Beginn) & humvedata_gill$TIMESTAMP   <= max(logdata$Ende),]
  

  for (i in seq_along(logdata$Station)) {
    meteo <- humvedata_meteo[humvedata_meteo$TIMESTAMP >= logdata$Beginn[i] & humvedata_meteo$TIMESTAMP <= logdata$Ende[i],]
    wind <- humvedata_wind[humvedata_wind$TIMESTAMP >= logdata$Beginn[i] & humvedata_wind$TIMESTAMP <= logdata$Ende[i],]
    gill <- humvedata_gill[humvedata_gill$TIMESTAMP >= logdata$Beginn[i] & humvedata_gill$TIMESTAMP <= logdata$Ende[i],]
    Station <- rep(logdata$Station[i],length.out=length(meteo[,1]))
    KT19 <- rep(logdata$KT.19[i],length.out=length(meteo[,1]))
  
    if (!exists("result")){
      result <- as.data.frame(cbind(meteo[,1],Station,meteo[,3:7],meteo[,10:12],wind[,3:5],gill[,3:4],gill[,7:8],KT19))
    } else {
      result <- rbind(result, cbind(meteo[,1],Station,meteo[,3:7],meteo[,10:12],wind[,3:5],gill[,3:4],gill[,7:8],KT19))
    }
  }
  names(result) <- c("TIMESTAMP","Station","Ta_150cm","RH_150cm","NETRAD","KWO","KWU","IRTS","ANGX","ANGY","WS","WD","SIGMA_WD","U","V","W","Tv","KT19")
  print(str(result))
  invisible(result)
}