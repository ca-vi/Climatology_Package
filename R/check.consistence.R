check.consistence <- function (data){

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


}