#################################################
######### read ENVI-met V3.1 files ##############
#################################################
  
readENVImet3Data <- function(metafile, datafile) {

meta=readLines(metafile)
cat(meta[1]) # V3.1

n_x_grids = strtoi(meta[2])
n_y_grids = strtoi(meta[3])
n_z_grids = strtoi(meta[4])
n_variables = strtoi(meta[5])

dim=c(n_x_grids,n_y_grids,n_z_grids,n_variables)

EMrawData = readBin(datafile, what="numeric", n=file.info(datafile)$size, size=4)
EMdata=array(EMrawData, dim=dim )


cat(" OK Variables read:",n_variables,"\n")
return(EMdata)  }

############################################################################################################
#END OF FUNCTION


# system.time(EMdata <- readENVImetData(metafile=metafile, datafile=datafile))

############################
# Read the whole directory #
############################

#dir <- "C:/ENVImet31/output/Bachelorarbeit/Tilia_75ys/atmosphere/"
# dir <- "c:/Users/Krug/Documents/UrbanTreePaper/BaummodellTilia/output/cb_0ys_hb100/atmosphere/" # ENVI-met V4 Data Example

metafile.list=list.files(dir,pattern = "EDI")
# sort by correct data order
metafile.list <- metafile.list[which(substr(metafile.list,22,31)=="21.06.2013")]
    
datafile.list=list.files(dir,pattern = "EDT")
# sort by correct data order
datafile.list <- datafile.list[which(substr(datafile.list,22,31)=="21.06.2013")]


n_x <- 232
n_y <- 232
n_z <- 34
n_t <- length(datafile.list)


#Object list for ENVI-met v3.1
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
    
###############################################
######### read ENVI-met V4 files ##############
###############################################

readENVImet4Data <- function(metafile, datafile) {
  
  meta=readLines(metafile)
 
  cat(meta[2]) # V4
  
  n_x_grids = strtoi(meta[3])
  n_y_grids = strtoi(meta[4])
  n_z_grids = strtoi(meta[5])
  n_variables = strtoi(meta[6])
  
  dim=c(n_x_grids,n_y_grids,n_z_grids,n_variables)
  
  EMrawData = readBin(datafile, what="numeric", n=file.info(datafile)$size, size=4)
  EMdata=array(EMrawData, dim=dim )
  
  
  cat(" OK Variables read:",n_variables,"\n")
  return(EMdata)  }


dir <- "c:/Users/Krug/Documents/LuckyFacades/Simulations/LF_ALB07/OUTPUT/atmosphere/" # ENVI-met V4 Data Example

metafile.list=list.files(dir,pattern = "EDI")
# sort by correct data order
metafile.list <- metafile.list[which(substr(metafile.list,22,31)=="21.06.2013")]

datafile.list=list.files(dir,pattern = "EDT")
# sort by correct data order
datafile.list <- datafile.list[which(substr(datafile.list,22,31)=="21.06.2013")]


n_x <- 168
n_y <- 168
n_z <- 30
n_t <- length(datafile.list)



#Object list for ENVI-met v4
index_obj = 1
OBJ = array(dim=c(n_x,n_y,n_z,n_t))

index_ws = 5
WS = array(dim=c(n_x,n_y,n_z,n_t))

index_mrt = 30
MRT = array(dim=c(n_x,n_y,n_z,n_t))

index_svfbv = 23
SVFbv = array(dim=c(n_x,n_y,n_z,n_t))

index_ta = 9
Ta = array(dim=c(n_x,n_y,n_z,n_t))

index_rh = 13
RH = array(dim=c(n_x,n_y,n_z,n_t))

index_rdir = 19
Rdir = array(dim=c(n_x,n_y,n_z,n_t))


for (t in 1:n_t) {
  metafile <- paste0(dir,metafile.list[t])
  datafile <- paste0(dir,datafile.list[t])
  
  EM_raw_data <- readENVImet4Data(metafile,datafile)
  
  OBJ[,,,t] = EM_raw_data[,,,index_obj]
  MRT[,,,t] = EM_raw_data[,,,index_mrt]
  WS[,,,t] = EM_raw_data[,,,index_ws]
  SVFbv[,,,t] = EM_raw_data[,,,index_svfbv]
  Ta[,,,t] = EM_raw_data[,,,index_ta]
  RH[,,,t] = EM_raw_data[,,,index_rh]
  Rdir[,,,t] = EM_raw_data[,,,index_rdir]
  
}