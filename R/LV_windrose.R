windrose<-function(windspeed, winddir){
  # Stand: 03.12.2013
  #
  # windrose ist eine Funktion um Winddaten aus den Campbell Datenloggern in Windrosen zu plotten.
  # Voraussetzung ist entsprechende Vorprozessierung (keine NAN) und Installation des climatol-packages
  # Da das climatol package von der CRAN repository entfernt wurde (warum auch immer), muss es einmalig aus dem Archiv heruntergeladen werden.
  # <http://cran.r-project.org/src/contrib/Archive/climatol/> da die neuste Version herunterladen und mit
  #! install.packages(file, repos = NULL, type = "source") installieren (file: character vektor of directory/download path)
  # 
  # Autor des Skripts: Carsten Vick
  # Code im Skript: Britta Jaenicke
  #
  # windspeed: a numeric vector containing windspeed data
  # winddir: a numeric vector containing winddirection data
  #
  # Anmerkung des Autors: Ich habe den folgenden Code fast genauso von Britta übernommen und einfach eine Funktion daraus gemacht.
  # Die Variablennamen sind an einigen Stellen uneindeutig bezeichnet und sind oft Hilfsvariablen.
  # Falls es Fragen zum Code gibt, bitte an <britta.jaenicke@yahoo.de> wenden.
  
  library(climatol) # laden des packages
  data(windfr) # laden des data.frames aus dem Beispiel (ist bereits gut vordefiniert)
  windv_class<- windfr*0 # löschen der Daten aus dem Beispiel
  dirup <- c(11.25,33.75,56.25,78.75,101.25,123.75,146.25,168.75,191.25,213.75,236.25,258.75,281.25,303.75,326.25,348.75) #festlegen der Gruppenobergrenzen
  dirlow <-c(348.75,11.25,33.75,56.25,78.75,101.25,123.75,146.25,168.75,191.25,213.75,236.25,258.75,281.25,303.75,326.25) #festlegen der Grupenuntergrenzen
  speedup <- c(0.5,1.0,1.5,2.0) #festlegen der Geschwindigkeitsobergrenzen
  speedlow <- (c(0.0,0.5,1.0,1.5)) #festlegen der Geschwindigkeitsuntergrenzen
  rownames(windv_class)<- c(paste(toString(speedlow[1]),"-",toString(speedup[1])), paste(toString(speedlow[2]),"-",toString(speedup[2])), paste(toString(speedlow[3]),"-",toString(speedup[3])), paste(">",toString(speedup[3]))) # ändern der rownames des Beispiels
  # die folgende Schleife ordnet die Windgeschwindigkeiten in entsprechende Geschwindigkeitsgruppen
  for (a in seq_along(speedup)){
      idx <- which(windspeed >=speedlow[a] & windspeed < speedup[a])
      # die jetzt innerhalb folgende Schleife ordnet die Windrichtung in eine Richtungsgruppe
       for (b in seq_along(dirlow)){
               
              if (b == 1) cnt1  <- length(which(winddir[idx] >=dirlow[b]))
              if (b == 1) cnt2  <- length(which(winddir[idx] < dirup[b]))
              if (b == 1) cnt  <- cnt1+cnt2
              if (b > 1) cnt  <- length(which(winddir[idx] >=dirlow[b] & winddir[idx] < dirup[b]))
              windv_class[a,b]<-cnt   
         
           }
     
       }
  # die so entstandene windv_class hat genau die Dimensionen die von der rosavent-Funktion benötigt wird.
  rosavent(windv_class,5,10,ang=-3*pi/16,main="Windrose der Station")
}