rankhospital<-function(state,outcome,num="best") 
{
## Read outcome data
df<-read.csv("outcome-of-care-measures.csv",header=T,stringsAsFactors=FALSE)
df<-df[,c(2,7,11,17,23)]
df[,3]<-as.numeric(df[,3])
df[,4]<-as.numeric(df[,4])
df[,5]<-as.numeric(df[,5])
states<-names(table(df[,2]))
outcomes<-c("heart attack","heart failure","pneumonia")
 
## Check that state and outcome are valid
if (is.na(match(state,states)))
	stop("invalid state")
if (is.na(match(outcome,outcomes))) 
      stop("invalid outcome")

## ...

index<-match(outcome,outcomes) 
df1<-df[df[,2]==state,]
df2<-df1[complete.cases(df1),]

df2<-df2[order(df2[,2+index],df2[,1]),]

if (num=="best") return(df2[1,1])
if (num=="worst") return(df2[dim(df2)[1],1])
    else return(df2[num,1])
}

rankhospital("TX","heart failure",4)
rankhospital("MD","heart attack","worst")
rankhospital("MN","heart attack",5000)