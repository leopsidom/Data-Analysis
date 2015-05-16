best <- function(state, outcome)
{
  dat<-read.csv("outcome-of-care-measures.csv")
  temp<-split(dat,dat["State"])
  
  if(sum(state==dat["State"])!=0)
  {
    b<-temp[state]
    
    if (outcome=="heart attack")
    { x<-as.numeric(levels(b[[1]][,11])[b[[1]][,11]])
      y<-min(x,na.rm=TRUE)==x
      y[is.na(y)]<-FALSE
      as.vector(b[[1]][y,"Hospital.Name"])}
      else 
      {if (outcome=="heart failure")
         {  x<-as.numeric(levels(b[[1]][,17])[b[[1]][,17]])
            y<-min(x,na.rm=TRUE)==x
            y[is.na(y)]<-FALSE
            as.vector(b[[1]][y,"Hospital.Name"])}
            else {
                 if (outcome=="pneumonia")
                    { x<-as.numeric(levels(b[[1]][,23])[b[[1]][,23]])
                      y<-min(x,na.rm=TRUE)==x
                      y[is.na(y)]<-FALSE
                      as.vector(b[[1]][y,"Hospital.Name"])}
                      else stop("invalid outcome")}}
  }
  else stop("invalid state")
}