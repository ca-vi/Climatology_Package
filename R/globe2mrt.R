############################################################
######### Calculate MRT via Globe Thermometer ##############
######### according to THorsson et al. 22007 ###############
############################################################

# written by Alex Krug 2014

# Tg = the globe temperature (°C)
# Wind_Speed = the air velocity (ms−1)
# Ta = the air temperature (°C)
# D = the globe diameter (mm)
# eps = the globe emissivity


globe2mrt = function (x, Tg) {
  
  eps =   .97
  D   = 40.
  va  = x$Wind_Speed
  Ta  = x$Ta
  
  # Check first for data availablity 
  
  if ( 
        exists("Ta") &
        exists("va") &
        exists("Tg")    
      ) 
  
       message("Great, all important data are loaded")
  
  else message("Please check input table for missing data")
  
  
  buff = (Tg + 273.15)^4 + ((1.335*10^8)*(va^.71) / (eps*(D^.4))) * (Tg-Ta)
  
  mrt = (buff^.25)-273.15
  
  return(mrt)
  message("Done ;-)")
}