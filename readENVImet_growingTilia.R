# ---------------------------------------------------------------------------
# This Script is Simulation Specific for the UrbanTree-Simulation (Summer 2013)
# ---------------------------------------------------------------------------

source('~/ownCloud/RWorkbench/FG_Klima_pkg/R/fgclim_readENVImet.R')
# system.time(EMdata <- readENVImetData(metafile=metafile, datafile=datafile))

# Read the whole Simulation directory

dir <- "c:/Users/Krug/Documents/UrbanTreePaper/BaummodellTilia/output/cb_0ys_hb100/atmosphere/" 

metafile.list=list.files(dir,pattern = "EDI")
# sort by correct data order
metafile.list <- metafile.list[which(substr(metafile.list,22,31)=="21.06.2013")]
    
datafile.list=list.files(dir,pattern = "EDT")
# sort by correct data order
datafile.list <- datafile.list[which(substr(datafile.list,22,31)=="21.06.2013")]

meta<- readLines(metafile.list[1])
n_x <- 232
n_y <- 232
n_z <- 34
n_t <- length(datafile.list)


# Object list for ENVI-met v3.1!!!! 
# see the Documentation (www.envi-met.com) or metafile manually

index_obj = 2
OBJ = array(dim=c(n_x,n_y,n_z,n_t))

index_ws = 6
WS = array(dim=c(n_x,n_y,n_z,n_t))

index_mrt = 36
MRT = array(dim=c(n_x,n_y,n_z,n_t))

index_svfbv = 25
SVFbv = array(dim=c(n_x,n_y,n_z,n_t))

index_ta = 10
Ta = array(dim=c(n_x,n_y,n_z,n_t))

index_rh = 14
RH = array(dim=c(n_x,n_y,n_z,n_t))

index_rdir = 20
Rdir = array(dim=c(n_x,n_y,n_z,n_t))

for (t in 1:n_t) {
  metafile <- paste0(dir,metafile.list[t])
  datafile <- paste0(dir,datafile.list[t])

  EM_raw_data <- readENVImet3Data(metafile,datafile)
  
  OBJ[,,,t] = EM_raw_data[,,,index_obj]
  MRT[,,,t] = EM_raw_data[,,,index_mrt]
  WS[,,,t] = EM_raw_data[,,,index_ws]
  SVFbv[,,,t] = EM_raw_data[,,,index_svfbv]
  Ta[,,,t] = EM_raw_data[,,,index_ta]
  RH[,,,t] = EM_raw_data[,,,index_rh]
  Rdir[,,,t] = EM_raw_data[,,,index_rdir]
  
}
    