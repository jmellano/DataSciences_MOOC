pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)


#   file <- c(directory)
#   print(file)
#   file<-directory+'/'+id+'.csv'
  
  listPollutant =0
  for(i in id){
    i<-formatC(i, width=3, flag="0")
    file<-paste(directory,as.character(i) , sep="/") 
    file<-paste(file,"csv" , sep=".") 
#     print(file)
#     print(str(i)+".csv")
    tmp<-read.csv(file, header = TRUE)
#   print(tmp[pollutant])
    listPollutant=rbind(listPollutant,tmp[pollutant])
#     print(lapply(tmp[pollutant],mean(na.rm=TRUE)))
#     print(mean(tmp[pollutant],na.rm=TRUE))
#     print(data.mean(pollutant))
  }
listPollutant=na.exclude(listPollutant)
retour=lapply(listPollutant,mean)

retour
}

