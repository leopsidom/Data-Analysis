complete <- function(directory, id = 1:332) 
  {
  N<-NULL;name<-NULL;
  for (i in id) {
    if (i<10){
      temp<-read.csv(paste(directory,"/",as.character(0),as.character(0),as.character(i),".csv",sep=""));
      M<-sum(!is.na(temp["sulfate"])&!is.na(temp["nitrate"]));
      N<-c(N,M);
      
    }
    else 
      
      if(i>=10 & i<100){
        x<-paste(directory,"/",as.character(0),as.character(i),".csv",sep="");
        temp<-read.csv(x);
        M<-sum(!is.na(temp["sulfate"])&!is.na(temp["nitrate"]));
        N<-c(N,M);
        
      }
    
    
    else 
      if(i>=100){
        x<-paste(directory,"/",as.character(i),".csv",sep="")
        temp<-read.csv(x);
        M<-sum(!is.na(temp["sulfate"])&!is.na(temp["nitrate"]));
        N<-c(N,M);
           
      }}
    data.frame(id=id,nobs=N)
  }
    