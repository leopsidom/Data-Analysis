pollutantmean <- function(directory, pollutant, id = 1:332) 
  {
  N<-NULL;
  for (i in id) {
    if (i<10){
    temp<-read.csv(paste(directory,"/",as.character(0),as.character(0),as.character(i),".csv",sep=""));
    M<-temp[!is.na(temp[pollutant]),pollutant];
    N<-c(N,M);}
    else 
    
    if(i>=10 & i<100){
    temp<-read.csv(paste(directory,"/",as.character(0),as.character(i),".csv",sep=""));
    M<-temp[!is.na(temp[pollutant]),pollutant];
    N<-c(N,M);}
   
    
    else 
      if(i>=100){
    temp<-read.csv(paste(directory,"/",as.character(i),".csv",sep=""));
    M<-temp[!is.na(temp[pollutant]),pollutant];
    N<-c(N,M);}
      
  }
  mean(N)
  
}