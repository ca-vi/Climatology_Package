############################################################
### Calculation of MRT according to Thorsson et al. 2007 ###
############################################################

# Written by Alex Krug 2013

# last Changes
# 21.02.2014 -------------------

# rename Tmrt to MRT 
# round output to two digits
# bugfix

 calc_mrt <- function(x){      # Angabe der Argumente

   F_i_h     =  .22        # E, W, N, S for a rotationally symmetric standing or walking person (Thorsson et al. 2007)
   F_i_v     =  .06        #  ; U, D for a rotationally symmetric standing or walking person (Thorsson et al. 2007)
   alpha     =  .7         #;the absorption coefficient for short-wave radiation
   epsylon   =  .97        #;emissivity of the human body
   sbc       = 5.67E-08    #;Stefan Boltzmann Constant [Wm-2K-4]
   
   SW_UP     =x$SW_upwelling
   SW_DOWN   =x$SW_downwelling
   SW_EAST   =x$SW_east
   SW_WEST   =x$SW_west
   SW_NORTH  =x$SW_north
   SW_SOUTH  =x$SW_south
   LW_UP     =x$LW_upwelling
   LW_DOWN   =x$LW_downwelling
   LW_EAST   =x$LW_east
   LW_WEST   =x$LW_west
   LW_NORTH  =x$LW_north
   LW_SOUTH  =x$LW_south

S_str_SW =  alpha    *((F_i_v*(SW_UP + SW_DOWN)) + (F_i_h*(SW_EAST + SW_WEST + SW_NORTH + SW_SOUTH)))
S_str_LW =  epsylon  *((F_i_v*(LW_UP + LW_DOWN)) + (F_i_h*(LW_EAST + LW_WEST + LW_NORTH + LW_SOUTH)))

S_str =  S_str_SW + S_str_LW
                    

mrt = round(((S_str/(epsylon*sbc))^(.25)) -273.15  ,2 )

return(mrt)
 }  

### END OF FUNCTION ###
