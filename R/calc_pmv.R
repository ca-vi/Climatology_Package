
############################################################
### Calculation of PMV according to DIN EN ISO 7730 2006 ###
############################################################

# Written by Alex Krug 2013

# Please use the "read_biomet_and_agg"-skript for correct notation
# this calculation seems to be not correct due to wrong description in DIN EN ISO...

# last Changes
# 7.7.2014

# change ∞C to K calculation to +273
# validation of function with ISO 7730:2005(E)
  
calc_pmv = function(x, clo, met){

  if (length(x[is.na(x)==TRUE]) > 0 ) 
    stop("Input data contains missing values. PMV calculation is not possible. Please remove NA's and try again.")
  
  # hier evtl. NA-Ausgabe einbauen
  
mrt  = calc_mrt(x)

pa    = x$RH * 10 * exp(16.6536 - 4030.183 / (x$Ta + 235))   # water vapur pressure [Pa]

clo = if (missing(clo)) {clo=.6} else {clo=clo}
   # clo = .6 --> clothing factor f√ºr leichte Sommerbekleidung (siehe VDI oder DIN ISO 7730)
met = if (missing(met)) {met=2.} else {met=met}
  # Walking (on level surface) 0.9 m/s, 3.2 km/h, 2.0 mph
  # ASHARE 2004 Thermal Environmental Conditions for Human Occupancy Wm^-2

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
ta_k  = x$Ta+273 # air temperature in Kelvin
tr_k  = mrt+273 # mean radiant temperature in Kelvin

#CALCULATE SURFACE TEMPERATURE OF CLOTHING BY ITERATION
tcla  = ta_k + (35.5-x$Ta) / (3.5*icl+.1)

p1    = icl*fcl
p2    = p1*3.96
p3    = p1*100
p4    = p1*ta_k
p5    = 308.7-.028*mw+p2*(tr_k/100)^4

xn = tcla/100
xf = xn

n = 0
eps = .00015

hcn  =2.38*abs(100*xf-ta_k)^.25
hc = ifelse(hcf>hcn,hcf,hcn)
xn    =(p5+p4*hc-p2*xf^4) / (100+p3*hc)  

while(abs(xn[1]-xf[1])>eps) {
xf=(xf+xn)/2
hcn  =2.38*abs(100*xf-ta_k)^.25
hc = ifelse(hcf>hcn,hcf,hcn)
xn    =(p5+p4*hc-p2*xf^4) / (100+p3*hc)
n = n+1
if (n > 150) {pmv=-9999}
}  
tcl   =100*xn-273 # surface temperature of the clothing

############# HEAT LOSS COMPONENTS #################

hl1   = 3.05*.001*(5733-6.99*mw-pa)               # heat loss diff. through skin
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
