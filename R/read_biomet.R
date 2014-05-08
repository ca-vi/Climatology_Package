############################################################
######### Rohdaten der BIOMET-Station einlesen #############
#################### und Aggregation #######################

# written by Alex Krug 2013

# Beta Version: read_in function for biomet station
# this function needs file pathes of the three input files of the biomet-station


read_BIOMET <- function (file_BM1, file_BM2, file_WM){ #description
   
  # Reading data from file
  data_BM1 <- read.table(file_BM1, sep =",", dec=".", skip=4, col.names=scan(file_BM1, skip=1, nlines=1, sep=",", what=character()))
  data_BM2 <- read.table(file_BM2, sep =",", dec=".", skip=4, col.names=scan(file_BM2, skip=1, nlines=1, sep=",", what=character()))
  data_WM <- read.table(file_WM, sep =",", dec=".", skip=4, col.names=scan(file_WM, skip=1, nlines=1, sep=",", what=character()))
  
  #merge tables
  data1=merge(data_BM1,data_BM2, by="TIMESTAMP", all.x=T,)
  data=merge(data1,data_WM, by="TIMESTAMP", all.x=T)
  
  # Calculate LW_west and add
  LW_west <- data$LW_west_meas + (5.67e-8 * ((data$T_CNR4_SN121043 + 273.15)^4)) 
  data <- cbind(data,LW_west)
  
  return(data)
  message('Done :-)')
}


#############################################################
### Calculation of Tmrt according to Thorsson et al. 2007 ###
#############################################################

# Written by Alex Krug 2013

# Please use the "read_biomet_and_agg"-skript for correct notation

calc_tmrt <- function(x){      # Angabe der Argumente
  
  F_i_h     =  .22        # E, W, N, S for a rotationally symmetric standing or walking person (Thorsson et al. 2007)
  F_i_v     =  .06        #  ; U, D for a rotationally symmetric standing or walking person (Thorsson et al. 2007)
  alpha     =  .7         #;the absorbtion coefficient for short-wave radiation
  epsylon   =  .97        #;emissivity of the human body
  sbc       = 5.67E-08    #;Stefan Boltzmann Constant [Wm-2]
  
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
  LW_NORTH  =x$LW_south
  LW_SOUTH  =x$LW_north
  
  S_str_SW =  alpha    *((F_i_v*(SW_UP + SW_DOWN)) + (F_i_h*(SW_EAST + SW_WEST + SW_NORTH + SW_SOUTH)))
  S_Str_LW =  epsylon  *((F_i_v*(LW_UP + LW_DOWN)) + (F_i_h*(LW_EAST + LW_WEST + LW_NORTH + LW_SOUTH)))
  
  S_str =  S_str_SW + S_Str_LW
  
  
  Tmrt = ((S_str/(epsylon*sbc))^(.25)) -273.15   
  
  return(Tmrt)
}  


############################################################
### Calculation of PMV according to DIN EN ISO 7730 2006 ###
############################################################

# Written by Alex Krug 2013

# Please use the "read_biomet_and_agg"-skript for correct notation
# this calculation seems to be not correct due to wrong description in DIN EN ISO...


calc_pmv = function(x){
  tmrt  = calc_tmrt(x)
  
  pa    = x$RH * 10 * exp(16.6536 - 4030.183 / (x$Ta + 235))   # water vapur pressure [Pa]
  
  clo   = .6 # clothing factor für leichte Sommerbekleidung (siehe VDI oder DIN ISO 7730)
  met   = 1.9 # Wm^-2, Walking (3.2 ms-^1) on level ground Metabolic rate in relation to 1 m^2 of body surface (VDI 3787_2)
  
  #metaboloc rate
  m     = met*58.15   # metabolic rate in W/m2
  wme   = 0
  w     = wme*58.15   # external work in W/m2
  mw    = m-w         # internal heat production in the human body
  
  #clothing
  icl   = .155*clo    # thermal insulation of the clothing in M2K/W
  fcl   = if (icl <= .078)  {1+1.29*icl} else {1.05+0.645*icl} # clothing area factor
  
  #convection
  hcf   = 12.1*sqrt(x$Wind_Speed) # heat transf. coeff. by forced convection
  ta_k  = x$Ta+273.15 # air temperature in Kelvin
  tr_k  = tmrt+273.15 # mean radiant temperature in Kelvin
  
  #CALCULATE SURFACE TEMPERATURE OF CLOTHING BY ITERATION
  tcla  = ta_k + (35.5-x$Ta) / (3.5*(6.45*icl+.1))
  p1    = icl*fcl
  p2    = p1*3.96
  p3    = p1*100
  p4    = p1*ta_k
  p5    = 308.7-.028*mw+p2*(tr_k/100)^4
  
  xn = tcla/100
  xf = xn
  n = 0
  eps = .00015
  
  while(abs(xn[1]-xf[1])<eps) {
    xf=(xf+xn)/2
    hcn  =2.38*abs(100*xf-ta_k)^.25
    hc = ifelse(hcf>hcn,hcf,hcn)
    xn    =(p5+p4*hc-p2*xf^4) / (100+p3*hc)
    n = n+1
    if (n > 150) {pmv=-9999}
  }  
  tcl   =100*xn-273.15 # surface temperature of the clothing
  
  ############# HEAT LOSS COMPONENTS #################
  
  hl1   = 3.05*.001*(5733-6.99-mw-pa)               # heat loss diff. through skin
  if (mw > 58.15) {hl2=.42*(mw-58.15)} else {hl2=0} # heat loss by sweating (comfort)
  hl3   = 1.7 * .00001 * m * (5867-pa)              # latent respiration heat loss
  hl4   = 0.0014*m*(34-x$Ta)                        # dry respiration heat loss
  hl5   = 3.96*fcl*(xn^4-(tr_k/100)^4)              # heat loss by radiation
  hl6   = fcl * hc * (tcl-x$Ta)                     # heat loss by convection
  
  ts = 0.028+0.303*exp(-0.036*m)
  pmv = ts*(mw-hl1-hl2-hl3-hl4-hl5-hl6)
  
  return(pmv)
}

### END OF FUNCTION ###

#######################################################
### Calculation of UTCI according Peter Broede 2009 ###
#######################################################

# Written by Alex Krug 2013

# Please use the "read_biomet_and_agg"-skript for correct notation

# Input Variables:
# Ta      - air temperature in degC
# ehPa    - water vapour pressure in hPa
# Tmrt    - mean radiant temperature in degC
# va10m   - wind velocity (10 m) in m/s
# RH      - relative humidity in percent

calc_utci = function(x) {
  Tmrt = calc_tmrt(x)
  
  ##########################
  # calculates saturation vapour pressure over water in hPa for input air temperature (ta) in celsius according to:
  # Hardy, R.; ITS-90 Formulations for Vapor Pressure, Frostpoint Temperature, Dewpoint Temperature and Enhancement Factors in the Range -100 to 100 ∞C; 
  
  
  g=c( -2.8365744E3,
       -6.028076559E3,
       1.954263612E1,
       -2.737830188E-2,
       1.6261698E-5,
       7.0229056E-10,
       -1.8680009E-13,
       2.7150305)
  
  tk = x$Ta + 273.15   	# air temp in K
  es = g[8] * log(tk)
  for (i in 1:7) {
    es = g[i] * (tk^(i-3)) + es
  }
  
  es = exp(es)*0.01	      # *0.01: convert Pa to hPa
  ehPa = es * x$RH/100.00000000
  
  Pa = ehPa/10.0; #!~ use vapour pressure in kPa
  va = (log(10/0.01)/log(1.77/0.01))*x$Wind_Speed
  Ta = x$Ta
  D_Tmrt = Tmrt - Ta
  
  UTCI = Ta+
    ( 6.07562052E-01 )   + 
    ( -2.27712343E-02 ) * Ta + 
    ( 8.06470249E-04 ) * Ta*Ta + 
    ( -1.54271372E-04 ) * Ta*Ta*Ta + 
    ( -3.24651735E-06 ) * Ta*Ta*Ta*Ta + 
    ( 7.32602852E-08 ) * Ta*Ta*Ta*Ta*Ta + 
    ( 1.35959073E-09 ) * Ta*Ta*Ta*Ta*Ta*Ta + 
    ( -2.25836520E+00 ) * va + 
    ( 8.80326035E-02 ) * Ta*va + 
    ( 2.16844454E-03 ) * Ta*Ta*va + 
    ( -1.53347087E-05 ) * Ta*Ta*Ta*va + 
    ( -5.72983704E-07 ) * Ta*Ta*Ta*Ta*va + 
    ( -2.55090145E-09 ) * Ta*Ta*Ta*Ta*Ta*va + 
    ( -7.51269505E-01 ) * va*va + 
    ( -4.08350271E-03 ) * Ta*va*va + 
    ( -5.21670675E-05 ) * Ta*Ta*va*va + 
    ( 1.94544667E-06 ) * Ta*Ta*Ta*va*va + 
    ( 1.14099531E-08 ) * Ta*Ta*Ta*Ta*va*va + 
    ( 1.58137256E-01 ) * va*va*va + 
    ( -6.57263143E-05 ) * Ta*va*va*va + 
    ( 2.22697524E-07 ) * Ta*Ta*va*va*va + 
    ( -4.16117031E-08 ) * Ta*Ta*Ta*va*va*va + 
    ( -1.27762753E-02 ) * va*va*va*va + 
    ( 9.66891875E-06 ) * Ta*va*va*va*va + 
    ( 2.52785852E-09 ) * Ta*Ta*va*va*va*va + 
    ( 4.56306672E-04 ) * va*va*va*va*va + 
    ( -1.74202546E-07 ) * Ta*va*va*va*va*va + 
    ( -5.91491269E-06 ) * va*va*va*va*va*va + 
    ( 3.98374029E-01 ) * D_Tmrt + 
    ( 1.83945314E-04 ) * Ta*D_Tmrt + 
    ( -1.73754510E-04 ) * Ta*Ta*D_Tmrt + 
    ( -7.60781159E-07 ) * Ta*Ta*Ta*D_Tmrt + 
    ( 3.77830287E-08 ) * Ta*Ta*Ta*Ta*D_Tmrt + 
    ( 5.43079673E-10 ) * Ta*Ta*Ta*Ta*Ta*D_Tmrt + 
    ( -2.00518269E-02 ) * va*D_Tmrt + 
    ( 8.92859837E-04 ) * Ta*va*D_Tmrt + 
    ( 3.45433048E-06 ) * Ta*Ta*va*D_Tmrt + 
    ( -3.77925774E-07 ) * Ta*Ta*Ta*va*D_Tmrt + 
    ( -1.69699377E-09 ) * Ta*Ta*Ta*Ta*va*D_Tmrt + 
    ( 1.69992415E-04 ) * va*va*D_Tmrt + 
    ( -4.99204314E-05 ) * Ta*va*va*D_Tmrt + 
    ( 2.47417178E-07 ) * Ta*Ta*va*va*D_Tmrt + 
    ( 1.07596466E-08 ) * Ta*Ta*Ta*va*va*D_Tmrt + 
    ( 8.49242932E-05 ) * va*va*va*D_Tmrt + 
    ( 1.35191328E-06 ) * Ta*va*va*va*D_Tmrt + 
    ( -6.21531254E-09 ) * Ta*Ta*va*va*va*D_Tmrt + 
    ( -4.99410301E-06 ) * va*va*va*va*D_Tmrt + 
    ( -1.89489258E-08 ) * Ta*va*va*va*va*D_Tmrt + 
    ( 8.15300114E-08 ) * va*va*va*va*va*D_Tmrt + 
    ( 7.55043090E-04 ) * D_Tmrt*D_Tmrt + 
    ( -5.65095215E-05 ) * Ta*D_Tmrt*D_Tmrt + 
    ( -4.52166564E-07 ) * Ta*Ta*D_Tmrt*D_Tmrt + 
    ( 2.46688878E-08 ) * Ta*Ta*Ta*D_Tmrt*D_Tmrt + 
    ( 2.42674348E-10 ) * Ta*Ta*Ta*Ta*D_Tmrt*D_Tmrt + 
    ( 1.54547250E-04 ) * va*D_Tmrt*D_Tmrt + 
    ( 5.24110970E-06 ) * Ta*va*D_Tmrt*D_Tmrt + 
    ( -8.75874982E-08 ) * Ta*Ta*va*D_Tmrt*D_Tmrt + 
    ( -1.50743064E-09 ) * Ta*Ta*Ta*va*D_Tmrt*D_Tmrt + 
    ( -1.56236307E-05 ) * va*va*D_Tmrt*D_Tmrt + 
    ( -1.33895614E-07 ) * Ta*va*va*D_Tmrt*D_Tmrt + 
    ( 2.49709824E-09 ) * Ta*Ta*va*va*D_Tmrt*D_Tmrt + 
    ( 6.51711721E-07 ) * va*va*va*D_Tmrt*D_Tmrt + 
    ( 1.94960053E-09 ) * Ta*va*va*va*D_Tmrt*D_Tmrt + 
    ( -1.00361113E-08 ) * va*va*va*va*D_Tmrt*D_Tmrt + 
    ( -1.21206673E-05 ) * D_Tmrt*D_Tmrt*D_Tmrt + 
    ( -2.18203660E-07 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 7.51269482E-09 ) * Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 9.79063848E-11 ) * Ta*Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 1.25006734E-06 ) * va*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( -1.81584736E-09 ) * Ta*va*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( -3.52197671E-10 ) * Ta*Ta*va*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( -3.36514630E-08 ) * va*va*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 1.35908359E-10 ) * Ta*va*va*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 4.17032620E-10 ) * va*va*va*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( -1.30369025E-09 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 4.13908461E-10 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 9.22652254E-12 ) * Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( -5.08220384E-09 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( -2.24730961E-11 ) * Ta*va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 1.17139133E-10 ) * va*va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 6.62154879E-10 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 4.03863260E-13 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 1.95087203E-12 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( -4.73602469E-12 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + 
    ( 5.12733497E+00 ) * Pa + 
    ( -3.12788561E-01 ) * Ta*Pa + 
    ( -1.96701861E-02 ) * Ta*Ta*Pa + 
    ( 9.99690870E-04 ) * Ta*Ta*Ta*Pa + 
    ( 9.51738512E-06 ) * Ta*Ta*Ta*Ta*Pa + 
    ( -4.66426341E-07 ) * Ta*Ta*Ta*Ta*Ta*Pa + 
    ( 5.48050612E-01 ) * va*Pa + 
    ( -3.30552823E-03 ) * Ta*va*Pa + 
    ( -1.64119440E-03 ) * Ta*Ta*va*Pa + 
    ( -5.16670694E-06 ) * Ta*Ta*Ta*va*Pa + 
    ( 9.52692432E-07 ) * Ta*Ta*Ta*Ta*va*Pa + 
    ( -4.29223622E-02 ) * va*va*Pa + 
    ( 5.00845667E-03 ) * Ta*va*va*Pa + 
    ( 1.00601257E-06 ) * Ta*Ta*va*va*Pa + 
    ( -1.81748644E-06 ) * Ta*Ta*Ta*va*va*Pa + 
    ( -1.25813502E-03 ) * va*va*va*Pa + 
    ( -1.79330391E-04 ) * Ta*va*va*va*Pa + 
    ( 2.34994441E-06 ) * Ta*Ta*va*va*va*Pa + 
    ( 1.29735808E-04 ) * va*va*va*va*Pa + 
    ( 1.29064870E-06 ) * Ta*va*va*va*va*Pa + 
    ( -2.28558686E-06 ) * va*va*va*va*va*Pa + 
    ( -3.69476348E-02 ) * D_Tmrt*Pa + 
    ( 1.62325322E-03 ) * Ta*D_Tmrt*Pa + 
    ( -3.14279680E-05 ) * Ta*Ta*D_Tmrt*Pa + 
    ( 2.59835559E-06 ) * Ta*Ta*Ta*D_Tmrt*Pa + 
    ( -4.77136523E-08 ) * Ta*Ta*Ta*Ta*D_Tmrt*Pa + 
    ( 8.64203390E-03 ) * va*D_Tmrt*Pa + 
    ( -6.87405181E-04 ) * Ta*va*D_Tmrt*Pa + 
    ( -9.13863872E-06 ) * Ta*Ta*va*D_Tmrt*Pa + 
    ( 5.15916806E-07 ) * Ta*Ta*Ta*va*D_Tmrt*Pa + 
    ( -3.59217476E-05 ) * va*va*D_Tmrt*Pa + 
    ( 3.28696511E-05 ) * Ta*va*va*D_Tmrt*Pa + 
    ( -7.10542454E-07 ) * Ta*Ta*va*va*D_Tmrt*Pa + 
    ( -1.24382300E-05 ) * va*va*va*D_Tmrt*Pa + 
    ( -7.38584400E-09 ) * Ta*va*va*va*D_Tmrt*Pa + 
    ( 2.20609296E-07 ) * va*va*va*va*D_Tmrt*Pa + 
    ( -7.32469180E-04 ) * D_Tmrt*D_Tmrt*Pa + 
    ( -1.87381964E-05 ) * Ta*D_Tmrt*D_Tmrt*Pa + 
    ( 4.80925239E-06 ) * Ta*Ta*D_Tmrt*D_Tmrt*Pa + 
    ( -8.75492040E-08 ) * Ta*Ta*Ta*D_Tmrt*D_Tmrt*Pa + 
    ( 2.77862930E-05 ) * va*D_Tmrt*D_Tmrt*Pa + 
    ( -5.06004592E-06 ) * Ta*va*D_Tmrt*D_Tmrt*Pa + 
    ( 1.14325367E-07 ) * Ta*Ta*va*D_Tmrt*D_Tmrt*Pa + 
    ( 2.53016723E-06 ) * va*va*D_Tmrt*D_Tmrt*Pa + 
    ( -1.72857035E-08 ) * Ta*va*va*D_Tmrt*D_Tmrt*Pa + 
    ( -3.95079398E-08 ) * va*va*va*D_Tmrt*D_Tmrt*Pa + 
    ( -3.59413173E-07 ) * D_Tmrt*D_Tmrt*D_Tmrt*Pa + 
    ( 7.04388046E-07 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*Pa + 
    ( -1.89309167E-08 ) * Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt*Pa + 
    ( -4.79768731E-07 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*Pa + 
    ( 7.96079978E-09 ) * Ta*va*D_Tmrt*D_Tmrt*D_Tmrt*Pa + 
    ( 1.62897058E-09 ) * va*va*D_Tmrt*D_Tmrt*D_Tmrt*Pa + 
    ( 3.94367674E-08 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa + 
    ( -1.18566247E-09 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa + 
    ( 3.34678041E-10 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa + 
    ( -1.15606447E-10 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa + 
    ( -2.80626406E+00 ) * Pa*Pa + 
    ( 5.48712484E-01 ) * Ta*Pa*Pa + 
    ( -3.99428410E-03 ) * Ta*Ta*Pa*Pa + 
    ( -9.54009191E-04 ) * Ta*Ta*Ta*Pa*Pa + 
    ( 1.93090978E-05 ) * Ta*Ta*Ta*Ta*Pa*Pa + 
    ( -3.08806365E-01 ) * va*Pa*Pa + 
    ( 1.16952364E-02 ) * Ta*va*Pa*Pa + 
    ( 4.95271903E-04 ) * Ta*Ta*va*Pa*Pa + 
    ( -1.90710882E-05 ) * Ta*Ta*Ta*va*Pa*Pa + 
    ( 2.10787756E-03 ) * va*va*Pa*Pa + 
    ( -6.98445738E-04 ) * Ta*va*va*Pa*Pa + 
    ( 2.30109073E-05 ) * Ta*Ta*va*va*Pa*Pa + 
    ( 4.17856590E-04 ) * va*va*va*Pa*Pa + 
    ( -1.27043871E-05 ) * Ta*va*va*va*Pa*Pa + 
    ( -3.04620472E-06 ) * va*va*va*va*Pa*Pa + 
    ( 5.14507424E-02 ) * D_Tmrt*Pa*Pa + 
    ( -4.32510997E-03 ) * Ta*D_Tmrt*Pa*Pa + 
    ( 8.99281156E-05 ) * Ta*Ta*D_Tmrt*Pa*Pa + 
    ( -7.14663943E-07 ) * Ta*Ta*Ta*D_Tmrt*Pa*Pa + 
    ( -2.66016305E-04 ) * va*D_Tmrt*Pa*Pa + 
    ( 2.63789586E-04 ) * Ta*va*D_Tmrt*Pa*Pa + 
    ( -7.01199003E-06 ) * Ta*Ta*va*D_Tmrt*Pa*Pa + 
    ( -1.06823306E-04 ) * va*va*D_Tmrt*Pa*Pa + 
    ( 3.61341136E-06 ) * Ta*va*va*D_Tmrt*Pa*Pa + 
    ( 2.29748967E-07 ) * va*va*va*D_Tmrt*Pa*Pa + 
    ( 3.04788893E-04 ) * D_Tmrt*D_Tmrt*Pa*Pa + 
    ( -6.42070836E-05 ) * Ta*D_Tmrt*D_Tmrt*Pa*Pa + 
    ( 1.16257971E-06 ) * Ta*Ta*D_Tmrt*D_Tmrt*Pa*Pa + 
    ( 7.68023384E-06 ) * va*D_Tmrt*D_Tmrt*Pa*Pa + 
    ( -5.47446896E-07 ) * Ta*va*D_Tmrt*D_Tmrt*Pa*Pa + 
    ( -3.59937910E-08 ) * va*va*D_Tmrt*D_Tmrt*Pa*Pa + 
    ( -4.36497725E-06 ) * D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa + 
    ( 1.68737969E-07 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa + 
    ( 2.67489271E-08 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa + 
    ( 3.23926897E-09 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa + 
    ( -3.53874123E-02 ) * Pa*Pa*Pa + 
    ( -2.21201190E-01 ) * Ta*Pa*Pa*Pa + 
    ( 1.55126038E-02 ) * Ta*Ta*Pa*Pa*Pa + 
    ( -2.63917279E-04 ) * Ta*Ta*Ta*Pa*Pa*Pa + 
    ( 4.53433455E-02 ) * va*Pa*Pa*Pa + 
    ( -4.32943862E-03 ) * Ta*va*Pa*Pa*Pa + 
    ( 1.45389826E-04 ) * Ta*Ta*va*Pa*Pa*Pa + 
    ( 2.17508610E-04 ) * va*va*Pa*Pa*Pa + 
    ( -6.66724702E-05 ) * Ta*va*va*Pa*Pa*Pa + 
    ( 3.33217140E-05 ) * va*va*va*Pa*Pa*Pa + 
    ( -2.26921615E-03 ) * D_Tmrt*Pa*Pa*Pa + 
    ( 3.80261982E-04 ) * Ta*D_Tmrt*Pa*Pa*Pa + 
    ( -5.45314314E-09 ) * Ta*Ta*D_Tmrt*Pa*Pa*Pa + 
    ( -7.96355448E-04 ) * va*D_Tmrt*Pa*Pa*Pa + 
    ( 2.53458034E-05 ) * Ta*va*D_Tmrt*Pa*Pa*Pa + 
    ( -6.31223658E-06 ) * va*va*D_Tmrt*Pa*Pa*Pa + 
    ( 3.02122035E-04 ) * D_Tmrt*D_Tmrt*Pa*Pa*Pa + 
    ( -4.77403547E-06 ) * Ta*D_Tmrt*D_Tmrt*Pa*Pa*Pa + 
    ( 1.73825715E-06 ) * va*D_Tmrt*D_Tmrt*Pa*Pa*Pa + 
    ( -4.09087898E-07 ) * D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa*Pa + 
    ( 6.14155345E-01 ) * Pa*Pa*Pa*Pa + 
    ( -6.16755931E-02 ) * Ta*Pa*Pa*Pa*Pa + 
    ( 1.33374846E-03 ) * Ta*Ta*Pa*Pa*Pa*Pa + 
    ( 3.55375387E-03 ) * va*Pa*Pa*Pa*Pa + 
    ( -5.13027851E-04 ) * Ta*va*Pa*Pa*Pa*Pa + 
    ( 1.02449757E-04 ) * va*va*Pa*Pa*Pa*Pa + 
    ( -1.48526421E-03 ) * D_Tmrt*Pa*Pa*Pa*Pa + 
    ( -4.11469183E-05 ) * Ta*D_Tmrt*Pa*Pa*Pa*Pa + 
    ( -6.80434415E-06 ) * va*D_Tmrt*Pa*Pa*Pa*Pa + 
    ( -9.77675906E-06 ) * D_Tmrt*D_Tmrt*Pa*Pa*Pa*Pa + 
    ( 8.82773108E-02 ) * Pa*Pa*Pa*Pa*Pa + 
    ( -3.01859306E-03 ) * Ta*Pa*Pa*Pa*Pa*Pa + 
    ( 1.04452989E-03 ) * va*Pa*Pa*Pa*Pa*Pa + 
    ( 2.47090539E-04 ) * D_Tmrt*Pa*Pa*Pa*Pa*Pa + 
    ( 1.48348065E-03 ) * Pa*Pa*Pa*Pa*Pa*Pa 
  
  return(UTCI)
  
}

#######################################################
############ END of Calculation UTCI ##################
#######################################################
