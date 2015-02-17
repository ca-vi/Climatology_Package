windrose <- function(windspeed, winddir, r=5, p=10, title = NULL, ...){  
  windmatrix<- as.data.frame(matrix(0, 4, 16))
  names(windmatrix) <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  dirup <- c(11.25,33.75,56.25,78.75,101.25,123.75,146.25,168.75,191.25,213.75,236.25,258.75,281.25,303.75,326.25,348.75) #festlegen der Gruppenobergrenzen
  dirlow <-c(348.75,11.25,33.75,56.25,78.75,101.25,123.75,146.25,168.75,191.25,213.75,236.25,258.75,281.25,303.75,326.25) #festlegen der Grupenuntergrenzen
  speedup <- c(0.5,1.0,1.5,2.0) #festlegen der Geschwindigkeitsobergrenzen
  speedlow <- (c(0.0,0.5,1.0,1.5)) #festlegen der Geschwindigkeitsuntergrenzen
  rownames(windmatrix)<- c(paste(toString(speedlow[1]),"-",toString(speedup[1])), paste(toString(speedlow[2]),"-",toString(speedup[2])), paste(toString(speedlow[3]),"-",toString(speedup[3])), paste(">",toString(speedup[3]))) # ändern der rownames
  # die folgende Schleife ordnet die Windgeschwindigkeiten in entsprechende Geschwindigkeitsgruppen
  for (a in seq_along(speedup)){
      speedclass_logi <- which(windspeed >=speedlow[a] & windspeed < speedup[a])
      # die jetzt innerhalb folgende Schleife ordnet die Windrichtung in eine Richtungsgruppe
       for (b in seq_along(dirlow)){
              if (b == 1) count  <- length(which(winddir[speedclass_logi] >= 348.75)) + length(which(winddir[speedclass_logi] < 11.25))
              if (b > 1) count  <- length(which(winddir[speedclass_logi] >=dirlow[b] & winddir[speedclass_logi] < dirup[b]))
              windmatrix[a,b]<-count
           }
       }
  plotWindrose(windmatrix, r, p, ang=-3*pi/16, main=title, ...)
}