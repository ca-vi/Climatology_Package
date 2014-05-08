############################################################
######### Rohdaten der BIOMET-Station einlesen #############
#################### und Aggregation #######################

# written by Alex Krug 2013

# Beta Version: read_in function for biomet station
# this function only needs file pathes of the three input files of the biomet-station

# last Changes
# 21.02.2014 -------------------

# rename Tmrt to MRT 
# define NA-Strings in read.table-function

read_BIOMET <- function(file_BM1, file_BM2, file_WM){ #description
  if(exists("file_BM1") & exists("file_BM2") & exists("file_WM")) message (':) Great, all needed Files were loaded!')
  else message (':(  Please Check Input Files')
  
  # Reading data from file
  data_BM1 <- read.table(file_BM1, sep =",", dec=".", skip=4, na.strings="NAN", col.names=scan(file_BM1, skip=1, nlines=1, sep=",", what=character()))
  data_BM2 <- read.table(file_BM2, sep =",", dec=".", skip=4, na.strings="NAN", col.names=scan(file_BM2, skip=1, nlines=1, sep=",", what=character()))
  data_WM <- read.table(file_WM, sep =",", dec=".", skip=4, na.strings="NAN", col.names=scan(file_WM, skip=1, nlines=1, sep=",", what=character()))
  
  #merge tables
  data1=merge(data_BM1,data_BM2, by="TIMESTAMP", all.x=T)
  data=merge(data1,data_WM, by="TIMESTAMP", all.x=T)
  
  # Calculate LW_west and add
  LW_west <- data$LW_west_meas + 5.67e-8 * (data$T_CNR4_SN121043 + 273.15)^4
  data <- cbind(data,LW_west)
  
  return(data)
  message( 'Done :-)' )
}
