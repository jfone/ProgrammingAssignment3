rankall<-function(outcome,num="best") 
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
if (is.na(match(outcome,outcomes))) 
      stop("invalid outcome")

index<-match(outcome,outcomes) 
df1<-df[,c(1,2,2+index)]
df1<-df1[complete.cases(df1),]
df2<-df1[order(df1[,2],df1[,3],df1[,1]),]
head(df2)
df3<-split(df2,df2[,2])
len<-as.vector(sapply(df3,dim)[1,])

result<-data.frame(stringsAsFactors=FALSE)

if (num=="best")
	for (i in states) 
		result<-rbind(result,cbind(hospital=df3[i][[1]][,1][1],state=i)) else 

if (num=="worst")
	for (i in states) 
		result<-rbind(result,cbind(hospital=df3[i][[1]][,1][len[match(i,states)]],state=i)) else 

for (i in states) 
	result<-rbind(result,cbind(hospital=df3[i][[1]][,1][num],state=i))

rownames(result)<-states
return(result)
}


head(rankall("heart attack",20),10)
tail(rankall("pneumonia","worst"),3)
tail(rankall("heart failure"),10)

outcome="pneumonia"
num="worst"
