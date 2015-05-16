rankall <- function(outcome, num = "best")
{

dat<-read.csv("outcome-of-care-measures.csv")
temp<-split(dat,dat["State"])
y<-NULL
z<-NULL

if (outcome=="heart attack")
  { 
  for (i in 1:54)
  {
    state<-unique(dat["State"])
    b<-temp[as.character(state[i,])]
    x<-as.numeric(levels(b[[1]][,11])[b[[1]][,11]])
    xo<-b[[1]][order(x,b[[1]][,2],na.last=NA),]
    if(num=="best")
    {z<-rbind(z,c(as.character(xo[1,2]),as.character(state[i,])))}
    else
    {
      if(num=="worst")
      {
        z<-rbind(z,c(tail(as.character(xo[,2]),1),as.character(state[i,])))
      }
      else
      {z<-rbind(z,c(as.character(xo[num,2]),as.character(state[i,])))}
    }
  }
  colnames(z)<-c("hospital","state")
  z<-as.data.frame(z)
  z[order(z["state"]),]
  }
  else
  {
  
  if (outcome=="heart failure")
  { 
    for (i in 1:54)
  {
    state<-unique(dat["State"])
    b<-temp[as.character(state[i,])]
    x<-as.numeric(levels(b[[1]][,17])[b[[1]][,17]])
    xo<-b[[1]][order(x,b[[1]][,2],na.last=NA),]
    
    if(num=="best")
    {z<-rbind(z,c(as.character(xo[1,2]),as.character(state[i,])))}
    else
    {
      if(num=="worst")
      {
        z<-rbind(z,c(tail(as.character(xo[,2]),1),as.character(state[i,])))
      }
      else
      {z<-rbind(z,c(as.character(xo[num,2]),as.character(state[i,])))}
    }
  }
  colnames(z)<-c("hospital","state")
  z<-as.data.frame(z)
  z[order(z["state"]),]
}

  else
  {
  if (outcome=="pneumonia")
  { for (i in 1:54)
  {
    state<-unique(dat["State"])
    b<-temp[as.character(state[i,])]
    x<-as.numeric(levels(b[[1]][,23])[b[[1]][,23]])
    xo<-b[[1]][order(x,b[[1]][,2],na.last=NA),]
    
    if(num=="best")
    {z<-rbind(z,c(as.character(xo[1,2]),as.character(state[i,])))}
    else
    {
      if(num=="worst")
      {
        z<-rbind(z,c(tail(as.character(xo[,2]),1),as.character(state[i,])))
      }
      else
      {z<-rbind(z,c(as.character(xo[num,2]),as.character(state[i,])))}
    }
  }
  colnames(z)<-c("hospital","state")
  z<-as.data.frame(z)
  z
  }
  colnames(z)<-c("hospital","state")
  z<-as.data.frame(z)
  z[order(z["state"]),]
  }
  }

}
