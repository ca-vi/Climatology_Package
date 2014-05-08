###################################################################################################
#
## Kontext: Version 1.0 R - Skript
#
## Projekt: alle
#
## Zweck:   erstellen einer Funktion mit alle notwendigen Befehlen, zum Auslesen der Datensätze aus unseren Campbellloggern
#
## Aufbau:  einlesen der .dat Datei, umwandeln des Zeitstempels in ein ordentliches Date-Time-Format, anzeigen der finalen Struktur
#
## Eingabe: Der Pfad zur Datei. Bsp: > meine_daten <- readin_loggerdat("C:\\mydata\\CR800_20_Meteo.dat")
#           keine manuelle Vorprozessierung der Daten!
#           Anhand des Timestamp und der Recordnumber wird überprüft, ob und wo Inkonsistenzen auftauchen
#           ohne NAN-Values in den Daten haben die Spalten auch gleich den richtigen Vektortyp, ansonsten muss prozessiert werden
#
## Ausgabe: die Daten als data.frame mit den entsprechenden Spaltennamen.
#
## Benoetigte Pakete: ---
#
## Autor:   Carsten Vick (carsten.vick@campus.tu-berlin.de)
#
## Letzte Änderung: 27.01.2014
#
## TO DO :  Inkonsistente Bereiche werden rausgeschnitten und ein Hinweis angezeigt.
#
###################################################################################################

readin_loggerdat <- function (file, info=TRUE) {
  
  data <- read.csv(file, header = FALSE, skip = 4)
  names(data) <- names(read.csv(file, skip = 1))
  if (!is.numeric(data$RECORD[1])){
    cat("Daten bitte wie im Code beschrieben vorprozessieren.\n")
  } else {
  
  data$TIMESTAMP <- strptime(data$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")
  
  rn_fehler<-0
  ts_diff <- difftime(data$TIMESTAMP[2],data$TIMESTAMP[1],units="secs")
  ts_fehler<-0
  for (i in 1:(length(data$RECORD)-1)) {
    if (data$RECORD[i+1] - data$RECORD[i] != 1) rn_fehler <- c(rn_fehler,i)
    if (difftime(data$TIMESTAMP[i+1],data$TIMESTAMP[i]) != ts_diff){
      ts_fehler <- c(ts_fehler,i)
    } else {
    ts_diff <- difftime(data$TIMESTAMP[i+1],data$TIMESTAMP[i],units="secs")
    }
  }
  if (length(rn_fehler)>1 & info==TRUE) {
    cat("\n Fehler in Recordnumber in ZEILE:\n",rn_fehler,file="fehlerzeilen.txt")
    print(cat("!ACHTUNG!: Fehler in Recordnumber in ZEILE:",rn_fehler,"\n"))
  }
  if (length(ts_fehler)>1 & info==TRUE) {
    cat("\n Fehler in Timestamp in ZEILE:\n",ts_fehler,file="fehlerzeilen.txt", append=TRUE)
    print(cat("!ACHTUNG!: Fehler in Timestamp in ZEILE:",ts_fehler,"\n"))
  }
  
  if (info==TRUE) print(str(data))  
  return(data)
  }
}