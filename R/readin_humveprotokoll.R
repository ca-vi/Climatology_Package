readin_humveprotokoll <- function ( file, info=TRUE ) {
  data <- read.csv2 (file, skip=5)
  datum <- as.Date(read.csv2 (file)[2,6],format="%d.%m.%Y")
  data$beginn <- strptime(paste(datum, data$beginn),format="%Y-%m-%d %H:%M:%S")
  data$ende <- strptime(paste(datum, data$ende),format="%Y-%m-%d %H:%M:%S")
  data$bemerkungen <- as.character(data$bemerkungen)
  if (info==TRUE) print(str(data))
  data
}