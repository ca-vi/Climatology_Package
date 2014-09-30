############################################################
######### Rohdaten der BIOMET-Station einlesen #############
#################### und Aggregation #######################

# written by Alex Krug 2013

# Beta Version: read_in function for biomet station
# this function needs file pathes of the three input files of the biomet-station


read_BIOMET <- function (file_bm1, file_bm2, file_wm, all=F){ 
  
  if (any(file.exists(file_bm1) , file.exists(file_bm2) , file.exists(file_wm))==TRUE) {
      message('All Files were loaded') }
      else {stop('Please check your input files')}

  # Reading data from file
  data_BM1 <- read.table(file_bm1, sep =",", dec=".",na.strings=NA, skip=4, col.names=scan(file_bm1, skip=1, nlines=1, sep=",", what=character()))
  data_BM2 <- read.table(file_bm2, sep =",", dec=".",na.strings=NA, skip=4, col.names=scan(file_bm2, skip=1, nlines=1, sep=",", what=character()))
  data_WM <- read.table(file_wm, sep =",", dec=".",na.strings=NA, skip=4, col.names=scan(file_wm, skip=1, nlines=1, sep=",", what=character()))
    
  #merge tables
  data1=merge(data_BM1,data_BM2, by="TIMESTAMP", all.x=T,)
  data=merge(data1,data_WM, by="TIMESTAMP", all.x=T)
  
  # Calculate LW_west and add
  LW_west <- data$LW_west_meas + (5.67e-8 * ((data$T_CNR4_SN121043 + 273.15)^4)) 
  data <- cbind(data,LW_west)
  
  if (all==T) {
    return(data)} 
    else {    
      sel = c('TIMESTAMP', 'SW_downwelling', 'SW_upwelling','SW_east','SW_west','SW_south', 'SW_north', 
              'LW_downwelling', 'LW_upwelling', 'LW_east','LW_west','LW_south', 'LW_north', 'Ta', 'RH',
              'Wind_Speed', 'Wind_Direction', 'Tglobe', 'Tglobe_7011')
      data <- data[sel]
      return(data)
    }
    
cat('Done :-)')
}

