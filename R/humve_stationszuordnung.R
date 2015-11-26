humve_stationszuordnung_mittelwerte <- function (humvedata_meteo, humvedata_wind, humvedata_gill, logdata) {
  
  # new.data wird die Ausgabedatei:
  new.data <-as.data.frame(matrix(nrow=length(logdata$station),ncol=18))
  names(new.data) <- c("TIMESTAMP","station","Ta_150cm","RH_150cm","NETRAD","KWO","KWU","IRTS","ANGX","ANGY","WS","WD","SIGMA_WD","U","V","W","Tv","KT19")
  
  # damit alle die gleiche länge haben: (sonst probleme beim cbind)
  humvedata_meteo <-  humvedata_meteo [humvedata_meteo$TIMESTAMP  >= min(logdata$beginn) & humvedata_meteo$TIMESTAMP  <= max(logdata$ende),]
  humvedata_wind  <-  humvedata_wind  [humvedata_wind$TIMESTAMP   >= min(logdata$beginn) & humvedata_wind$TIMESTAMP   <= max(logdata$ende),]
  humvedata_gill  <-  humvedata_gill  [humvedata_gill$TIMESTAMP   >= min(logdata$beginn) & humvedata_gill$TIMESTAMP   <= max(logdata$ende),]
  
  
  for (i in seq_along(logdata$station) )  {
    standzeitmittelung_daten <- sapply(sapply (cbind(humvedata_meteo[,(3:7)],humvedata_meteo[,(10:12)],humvedata_wind[,3:5],humvedata_gill[,3:4],humvedata_gill[,7:8]) [ humvedata_meteo$TIMESTAMP >= logdata$beginn[i] & humvedata_meteo$TIMESTAMP <= logdata$ende[i] ,  ] , mean), round, 2)
    new.data[i,(3:17)] <- standzeitmittelung_daten
    }
  new.data[,11:12] <- cart2polar(u = new.data[,14], v = new.data[,15])
  new.data$station <- logdata$station
  new.data$TIMESTAMP <- logdata$ende
  new.data$KT19 <- logdata$KT.19
  print(str(new.data))
  invisible(new.data)
}

humve_stationszuordnung_ungemittelt <- function (humvedata_meteo, humvedata_wind, humvedata_gill, logdata) {
  
  # damit alle die gleiche länge haben (sonst probleme beim cbind)
  humvedata_meteo <-  humvedata_meteo [humvedata_meteo$TIMESTAMP  >= min(logdata$beginn) & humvedata_meteo$TIMESTAMP  <= max(logdata$ende),]
  humvedata_wind  <-  humvedata_wind  [humvedata_wind$TIMESTAMP   >= min(logdata$beginn) & humvedata_wind$TIMESTAMP   <= max(logdata$ende),]
  humvedata_gill  <-  humvedata_gill  [humvedata_gill$TIMESTAMP   >= min(logdata$beginn) & humvedata_gill$TIMESTAMP   <= max(logdata$ende),]
  
  for (i in seq_along(logdata$station)) {
    meteo <- humvedata_meteo[humvedata_meteo$TIMESTAMP >= logdata$beginn[i] & humvedata_meteo$TIMESTAMP <= logdata$ende[i],]
    wind <- humvedata_wind[humvedata_wind$TIMESTAMP >= logdata$beginn[i] & humvedata_wind$TIMESTAMP <= logdata$ende[i],]
    gill <- humvedata_gill[humvedata_gill$TIMESTAMP >= logdata$beginn[i] & humvedata_gill$TIMESTAMP <= logdata$ende[i],]
    station <- rep(logdata$station[i],length.out=length(meteo[,1]))
    kt19 <- rep(logdata$KT.19[i],length.out=length(meteo[,1]))
  
    if (!exists("result")){
      result <- as.data.frame(cbind(meteo[,1],station,meteo[,3:7],meteo[,10:12],wind[,3:5],gill[,3:4],gill[,7:8],kt19))
    } else {
      result <- rbind(result, cbind(meteo[,1],station,meteo[,3:7],meteo[,10:12],wind[,3:5],gill[,3:4],gill[,7:8],kt19))
    }
  }
  names(result) <- c("TIMESTAMP","station","Ta_150cm","RH_150cm","NETRAD","KWO","KWU","IRTS","ANGX","ANGY","WS","WD","SIGMA_WD","U","V","W","Tv","KT19")
  print(str(result))
  invisible(result)
}