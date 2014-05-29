best<-function(state,outcome) 
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

## Return hospital names

index<-match(outcome,outcomes) 
df1<-df[df[,2]==state,]
df2<-df1[complete.cases(df1),]

result=vector()
result<-switch(index,
df2[df2[,3]==min(df2[,3]),1],
df2[df2[,4]==min(df2[,4]),1],
df2[df2[,5]==min(df2[,5]),1])
return(result[1])
}
