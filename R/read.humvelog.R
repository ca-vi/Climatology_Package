read.humvelog <- function ( file, info=TRUE, ...) {
  if (!require(XLConnect)) install.packages("XLConnect")
  library(XLConnect)
  
  wb <- loadWorkbook(file)
  datum <- readWorksheet(wb, sheet = "Humve", region = "E4", header=FALSE)
  if (length(datum) == 0) stop("Es muss ein Datum angegeben werden")
  datum <- as.numeric(datum[1,1])
  
  data <- readWorksheet(wb, sheet = "Humve", startRow = 7)[,-c(4,5,6)]
  names(data) <- c(names(data)[1:3], "KT19", names(data[5]))
  
  data$Beginn <- data$Beginn + datum + 2209078800
  data$Ende <- data$Ende + datum + 2209078800
  data$Bemerkungen <- as.character(data$Bemerkungen)
  data$Bemerkungen[is.na(data$Bemerkungen)] <- ""
  data <- data[complete.cases(data),]
  
  if (info==TRUE) print(str(data))
  invisible(data)
}