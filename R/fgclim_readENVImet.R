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
