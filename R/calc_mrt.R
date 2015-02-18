############################################################
### Calculation of MRT according to Thorsson et al. 2007 ###
############################################################

# Written by Alex Krug 2013

# last Changes
# 15.02.2015 -------------------

# changing arguments CV
# wrinting error report CV
# rename Tmrt to MRT 
# round output to two digits
# bugfix

 calc_mrt <- function(data=NULL, SW_UP=NA, SW_DOWN=NA, SW_EAST=NA,
                      SW_WEST=NA, SW_NORTH=NA, SW_SOUTH=NA, LW_UP=NA,
                      LW_DOWN=NA, LW_EAST=NA, LW_WEST=NA, LW_NORTH=NA,
                      LW_SOUTH=NA){      # Angabe der Argumente

   F_i_h     =  .22        # E, W, N, S for a rotationally symmetric standing or walking person (Thorsson et al. 2007)
   F_i_v     =  .06        #  ; U, D for a rotationally symmetric standing or walking person (Thorsson et al. 2007)
   alpha     =  .7         #;the absorption coefficient for short-wave radiation
   epsylon   =  .97        #;emissivity of the human body
   sbc       = 5.67E-08    #;Stefan Boltzmann Constant [Wm-2K-4]
   if(is.null(data)){
     arg <- c(SW_UP, SW_DOWN, SW_EAST, SW_WEST, SW_NORTH, SW_SOUTH, LW_UP,
              LW_DOWN, LW_EAST, LW_WEST, LW_NORTH, LW_SOUTH)
     check.arg <- which(is.na(arg))
     if(length(check.arg) > 0) stop(paste(names(arg[which(is.na(arg))]),"are not defined."))
   } else {
    SW_UP     = data$SW_upwelling
    SW_DOWN   = data$SW_downwelling
    SW_EAST   = data$SW_east
    SW_WEST   = data$SW_west
    SW_NORTH  = data$SW_north
    SW_SOUTH  = data$SW_south
    LW_UP     = data$LW_upwelling
    LW_DOWN   = data$LW_downwelling
    LW_EAST   = data$LW_east
    LW_WEST   = data$LW_west
    LW_NORTH  = data$LW_north
    LW_SOUTH  = data$LW_south
   }
   
  S_str_SW =  alpha    *((F_i_v*(SW_UP + SW_DOWN)) + (F_i_h*(SW_EAST + SW_WEST + SW_NORTH + SW_SOUTH)))
  S_str_LW =  epsylon  *((F_i_v*(LW_UP + LW_DOWN)) + (F_i_h*(LW_EAST + LW_WEST + LW_NORTH + LW_SOUTH)))

  S_str =  S_str_SW + S_str_LW
                    

  mrt = round(((S_str/(epsylon*sbc))^(.25)) -273.15  ,2 )

  return(mrt)
}  

### END OF FUNCTION ###
