corr <- function(directory, threshold = 0) {
  co<-NULL;
  for (i in 1:332) {
    if (i<10){
      temp<-read.csv(paste(directory,"/",as.character(0),as.character(0),as.character(i),".csv",sep=""));
      M<-sum(!is.na(temp["sulfate"])&!is.na(temp["nitrate"]));
      if(M>threshold)
      {cot<-cor(temp[!is.na(temp["sulfate"])&!is.na(temp["nitrate"]),"sulfate"],temp[!is.na(temp["sulfate"])&!is.na(temp["nitrate"]),"nitrate"]);
       co<-c(co,cot);
      }
      
    }
    else 
      
      if(i>=10 & i<100){
        x<-paste(directory,"/",as.character(0),as.character(i),".csv",sep="");
        temp<-read.csv(x);
        M<-sum(!is.na(temp["sulfate"])&!is.na(temp["nitrate"]));
        if(M>threshold)
        {
        cot<-cor(temp[!is.na(temp["sulfate"])&!is.na(temp["nitrate"]),"sulfate"],temp[!is.na(temp["sulfate"])&!is.na(temp["nitrate"]),"nitrate"]);
         co<-c(co,cot);
        }
        
      }
    
    
    else 
      if(i>=100){
        x<-paste(directory,"/",as.character(i),".csv",sep="")
        temp<-read.csv(x);
        M<-sum(!is.na(temp["sulfate"])&!is.na(temp["nitrate"]));
        N<-c(N,M);
        if(M>threshold)
        {cot<-cor(temp[!is.na(temp["sulfate"])&!is.na(temp["nitrate"]),"sulfate"],temp[!is.na(temp["sulfate"])&!is.na(temp["nitrate"]),"nitrate"]);
         co<-c(co,cot);
        }
      }}
  co
}