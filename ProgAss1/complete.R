complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files <- list.files( path = directory )[id]
  files <- files[!is.na(files)]
  
  nobs <- c()
  ids <- c()
  
#   for(f in 1:length(files)){
  for(f in files){
#     data <- read.csv( paste(directory, "/", files[f], sep="") )
    data <- read.csv( paste(directory, "/", f, sep="") )
    
    ids = c(ids,as.integer(substring(f,0,3)))
    nobs = c(nobs, nrow(na.omit(data)))

  }
  
  return( data.frame(id=id, nobs=nobs))
}
