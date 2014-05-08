###################################################################################################
#
## Kontext: Version 1.0 R - Skript Projekt "Prima Campusklima" 2013 
#         
#
## Zweck: Funktion zum read in des Humveprotokolls.
#
## Aufbau: 
#
## Eingabe: Die ausgefüllte Excelvorlage, in der die Daten einfach hintereinander gereiht sind. (Kopf unverändert, aber nach Station 11 kommt gleich Station 1)
#           Gespeichert als csv (MS-DOS) (heisst mit semicolon, wenn anders gespeichert muss read.csv2 geändert werden)
#       
## Ausgabe: Ein data.frame mit gleichem Aufbau und benutzbaren Spaltennamen
#
## Benoetigte Pakete: ---
#
#
## Autoren: Carsten Vick <carsten.vick@campus.tu-berlin.de>
#
## Letzte Änderung: 05.01.2014
# 
#
## TO DO :
#
###################################################################################################

readin_humveprotokoll <- function ( file, info=TRUE ) {
  data <- read.csv2 (file, skip=5)
  datum <- as.Date(read.csv2 (file)[2,6],format="%d.%m.%Y")
  data$beginn <- strptime(paste(datum, data$beginn),format="%Y-%m-%d %H:%M:%S")
  data$ende <- strptime(paste(datum, data$ende),format="%Y-%m-%d %H:%M:%S")
  data$bemerkungen <- as.character(data$bemerkungen)
  if (info==TRUE) print(str(data))
  data
}