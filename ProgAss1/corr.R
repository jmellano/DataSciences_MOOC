source("C:\\Users\\Julien\\Documents\\MOOC\\data_assigment\\complete.R")
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  
#   files = list.files(path = directory, pattern = (".csv"))
#   print(files)
  # On récupère l'ensemble des mesures et on filtre uniquement sur ceux >= au nombre demandé
  completeFile = complete(directory,1:332)
  selectedFile=completeFile[completeFile[,"noobs"]>=threshold,]  
#   print(selectedFile)
  selectedFileID = selectedFile[,"id"]
  count=1
  cors <- numeric()
  for(i in selectedFileID){
    i<-formatC(i, width=3, flag="0")
    file<-paste(directory,as.character(i) , sep="/") 
    file<-paste(file,"csv" , sep=".") 
    tmp<-read.csv(file, header = TRUE)
    
    nitrate=tmp['nitrate']
    sulfate=tmp['sulfate']
    nitrateSansNA = nitrate[!is.na(nitrate)]
    SulfateSansNA = sulfate[!is.na(sulfate)]
#     print(nitrateSansNA)
#     print(SulfateSansNA)
    cors[count] = cor(nitrate,sulfate,use = "na.or.complete")
#     print(cors)
#     nbValueNitrate = length(nitrate[!is.na(nitrate)])
#     nbValueSulfate = length(sulfate[!is.na(sulfate)])
    
#     if(count == 0){
#       mNitrate =  c(as.numeric(i),as.numeric(nbValueNitrate))
#       mSulfate =  c(as.numeric(i),as.numeric(nbValue))
#     }else{
#       mNitrate=rbind(mNitrate,c(as.numeric(i),as.numeric(nbValueNitrate)))
#       mSulfate=rbind(mSulfate,c(as.numeric(i),as.numeric(nbValue)))
#     }
#     
    count = count +1
  }
#   
#   dimnames(m) = list(NULL,c('id','noobs'))
#   m
  cors
}
