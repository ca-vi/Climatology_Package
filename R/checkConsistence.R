checkConsistence <- function (data, info=TRUE){
  if(names(data)[1]!="TIMESTAMP") stop("No Timestamp found! See ?read.logger for more information.")
  if(names(data)[2]!="RECORD") stop("No Recordnumber found! See ?read.logger for more information.")
  rn_fehler<-0
  ts_diff <- median(difftime(data$TIMESTAMP[-1],data$TIMESTAMP[-length(data$TIMESTAMP)],units="secs"))
  ts_fehler<-0
  for (i in 1:(length(data$RECORD)-1)) {
    if (is.na(data$RECORD[i]) | is.na(data$RECORD[i+1])) {rn_fehler <- c(rn_fehler,i)} else {
    if (data$RECORD[i+1] - data$RECORD[i] != 1) rn_fehler <- c(rn_fehler,i)}
    if (difftime(data$TIMESTAMP[i+1],data$TIMESTAMP[i]) != ts_diff) ts_fehler <- c(ts_fehler,i)
  }
  if (length(rn_fehler)>1 & info==TRUE) {
    cat("\n ERROR in Recordnumber in row:\n",rn_fehler,file="inconsistency.txt", append=TRUE)
    print(cat("!ATTENTION!: ERROR in Recordnumber in ROW:",rn_fehler,"\n"))
  }
  if (length(ts_fehler)>1 & info==TRUE) {
    cat("\n ERROR in Timestamp in row:\n",ts_fehler,file="inconsistency.txt", append=TRUE)
    print(cat("!ATTENTION!: ERROR in Timestamp in ROW:",ts_fehler,"\n"))
  }
  
  if(length(rn_fehler) == 1 & length(ts_fehler) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
    }
}