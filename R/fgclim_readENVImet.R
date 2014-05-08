###########################
# Name
#  
# readENVImetData
#
# Description
#  
# Reads the ENVI-met output and creates a datacontainer with the dimensions X, Y, Z, Variable.
# It checks if the ENVI-met-Output is in Version 3 or in Version 4
#
# Usage
# 
# output data <- readENVImetData(_path to metafile,_path to datafile_)
#  
# Example
#
# Input
#  
# path of .EDI-File (metafile) and path of .EDT-file (datafile)
#
# Output
#  
# initialize functions
#
# Author
#  
# AlK
#
# Date
#  
# 08.05.2014
#
# TODO
#   
# - Another function for reading the whole data-folder or simulation?
# - input-Vector for selected Variables?
# - ...
##################################

readENVImetData <- function(metafile, datafile) {
  
  meta=readLines(metafile)
  
  versioncontrol <-grepl("CreatedwithENVI-metV3",gsub(" ","",meta[length(meta)]))
  
  if (versioncontrol==TRUE) {
    
    cat("ENVI-met V3 ") 
    cat(meta[1]) # V3.1
    
    n_x_grids = strtoi(meta[2])
    n_y_grids = strtoi(meta[3])
    n_z_grids = strtoi(meta[4])
    n_variables = strtoi(meta[5])
  }
  else {
    
    cat("ENVI-met V4 ")
    cat(meta[2]) # V4
    
    n_x_grids = strtoi(meta[3])
    n_y_grids = strtoi(meta[4])
    n_z_grids = strtoi(meta[5])
    n_variables = strtoi(meta[6])
    
  }
  dim=c(n_x_grids,n_y_grids,n_z_grids,n_variables)
  
  EMrawData = readBin(datafile, what="numeric", n=file.info(datafile)$size, size=4)
  EMdata=array(EMrawData, dim=dim )
  
  
  cat(" OK Variables read:",n_variables,"\n")
  return(EMdata)  }
