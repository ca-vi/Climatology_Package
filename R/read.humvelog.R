read.humvelog <- function ( file, info=TRUE, ...) {
  data <- read.csv2 (file, skip=5, ...)
  if(!exists("beginn", where=data) | !exists("ende",where=data)) stop("Could not read file properly. Please check for wrong number of rows. read.csv2(skip=5)")
  datum <- as.Date(read.csv2 (file)[2,6],format="%d.%m.%Y")
  if(sum(is.na(data$beginn)) == length(data$beginn)) stop("Keine Beginn-Zeit lesbar.")
  if(sum(is.na(data$ende)) == length(data$ende)) stop("Keine Ende-Zeit lesbar.")
  data$beginn <- as.POSIXct(strptime(paste(datum, data$beginn),format="%Y-%m-%d %H:%M:%S"))
  data$ende <- as.POSIXct(strptime(paste(datum, data$ende),format="%Y-%m-%d %H:%M:%S"))
  data$bemerkungen <- as.character(data$bemerkungen)
  data$bemerkungen[is.na(data$bemerkungen)] <- ""
  data <- data[complete.cases(data),]
  if (info==TRUE) print(str(data))
  invisible(data)
}