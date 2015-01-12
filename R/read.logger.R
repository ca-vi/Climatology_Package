read.logger <- function (file, info=FALSE) {
  
  data <- read.csv(file, header = FALSE, skip = 4, na.strings="NAN")
  names(data) <- names(read.csv(file, skip = 1))
  if (!is.numeric(data$RECORD[1])){
    cat("ERROR in data. Review the data!")
  } else {
  
  data$TIMESTAMP <- strptime(data$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")
  
  if (info==TRUE) print(str(data))  
  return(data)
  }
}