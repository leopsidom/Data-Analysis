rankhospital <- function(state, outcome, num = "best")
{
  dat<-read.csv("outcome-of-care-measures.csv")
  temp<-split(dat,dat["State"])
  
  if(sum(state==dat["State"])!=0)
  {
    b<-temp[state]
    
    if (outcome=="heart attack")
    { x<-as.numeric(levels(b[[1]][,11])[b[[1]][,11]])
      y<-b[[1]][order(x,b[[1]][,2],na.last=NA),]
      if(num=="best")
         {as.vector(y[1,2])}
         else if(num=="worst")
                {as.vector(tail(y[,2],n=1L))}
                else {as.vector(y[num,2])}
    }
    else 
    {if (outcome=="heart failure")
    {  x<-as.numeric(levels(b[[1]][,17])[b[[1]][,17]])
       y<-b[[1]][order(x,b[[1]][,2],na.last=NA),]
       if(num=="best")
         {as.vector(y[1,2])}
         else if(num=="worst")
                {as.vector(tail(y[,2],n=1L))}
                else {as.vector(y[num,2])}
    }
    else {
      if (outcome=="pneumonia")
      { x<-as.numeric(levels(b[[1]][,23])[b[[1]][,23]])
        y<-b[[1]][order(x,b[[1]][,2],na.last=NA),]
        if(num=="best")
          {as.vector(y[1,2])}
          else if(num=="worst")
              {as.vector(tail(y[,2],n=1L))}
              else {as.vector(y[num,2])}}
      else stop("invalid outcome")}}
  }
  else stop("invalid state")
}