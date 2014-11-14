read.logger <- function (file, info=TRUE) {
  
  data <- read.csv(file, header = FALSE, skip = 4)
  names(data) <- names(read.csv(file, skip = 1))
  if (!is.numeric(data$RECORD[1])){
    cat("ERROR in data. Review the data!")
  } else {
  
  data$TIMESTAMP <- strptime(data$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")
  
  if (info==TRUE) print(str(data))  
  return(data)
  }
}