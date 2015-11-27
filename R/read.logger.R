read.logger <- function (file, info=FALSE, ...) {
  
  data <- read.csv(file, header = FALSE, skip = 4, na.strings="NAN", ...)
  names(data) <- names(read.csv(file, skip = 1))
  if (!exists("TIMESTAMP", where = data) | !exists("RECORD", where = data)){
    stop("Could not read the data properly. Please check the head of table in the file.")
  } else {
  
  data$TIMESTAMP <- as.POSIXct(data$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")
  
  if (info==TRUE) print(str(data))  
  invisible(data)
  }
}