library(readxl)
data=read_excel(file.choose())
data1=data[1:43,]
data2=data[-(1:43),]
table(data1$outcome)
table(data2$outcome)
table(data1$COMPLICATION)
table(data2$COMPLICATION)
prop.test(c(41,32),c(43,39))
prop.test(c(2,7),c(43,39))
prop.test(c(2,11),c(43,39))
t.test(data1$Tsb,data2$Tsb)
t.test(data1$DB,data2$DB)
t.test(data1$sgot,data2$sgot)
t.test(data1$sgpt,data2$sgpt)
t.test(data1$PYMSTH,data2$PYMSTH)
t.test(data1$PYDIA,data2$PYDIA)
par(mfrow=c(2,1))
hist(data1$b.wt,breaks=c(0,1,1.25,1.5),main="Distribution of Weight Groups for the Case Population",xlab="Birth Weight")
hist(data2$b.wt,breaks=c(0,1,1.25,1.5),main="Distribution of Weight Groups for the Control Population",xlab="Birth Weight")
par(mfrow=c(2,1))
hist(data1$ges.Age,breaks=c(0,30,34,38),main="Distribution of Gestational Age Groups for the Case Population",xlab="Age")
hist(data2$ges.Age,breaks=c(0,30,34,38),main="Distribution of Gestational Age Groups for the Control Population",xlab="Age")
#Primary findings
prop.test(c(length(which(data1$b.wt<1)),length(which(data2$b.wt<1))),c(length(data1$b.wt),length(data2$b.wt)))
prop.test(c(length(which((data1$b.wt>=1)&(data1$b.wt<1.25))),length(which((data2$b.wt>=1)&(data2$b.wt<1.25)))),c(length(data1$b.wt),length(data2$b.wt)))
prop.test(c(length(which((data1$b.wt>=1.25)&(data1$b.wt<1.5))),length(which((data2$b.wt>=1.25)&(data2$b.wt<1.5)))),c(length(data1$b.wt),length(data2$b.wt)))
prop.test(c(length(which(data1$ges.Age<30)),length(which(data2$ges.Age<30))),c(length(data1$ges.Age),length(data2$ges.Age)))
prop.test(c(length(which((data1$ges.Age>=30)&(data1$ges.Age<34))),length(which((data2$ges.Age>=30)&(data2$ges.Age<34)))),c(length(data1$ges.Age),length(data2$ges.Age)))
prop.test(c(length(which((data1$ges.Age>=34)&(data1$ges.Age<=38))),length(which((data2$ges.Age>=34)&(data2$ges.Age<=38)))),c(length(data1$ges.Age),length(data2$ges.Age)))
colnames(data[,8:11])=c("1/4Feed","1/2Feed","3/4Feed","FullFeed")
colnames(data1[,8:11])=c("1/4Feed","1/2Feed","3/4Feed","FullFeed")
colnames(data2[,8:11])=c("1/4Feed","1/2Feed","3/4Feed","FullFeed")
#Discharged
t.test(na.omit(data1$wt[which(data1$ges.Age<30)]),na.omit(data2$wt[which(data2$ges.Age<30)]))
t.test(na.omit(data1$wt[which((data1$ges.Age>=30)&(data1$ges.Age<34))]),na.omit(data2$wt[which((data1$ges.Age>=30)&(data1$ges.Age<34))]))
t.test(na.omit(data1$wt[which((data1$ges.Age>=34)&(data1$ges.Age<=38))]),na.omit(data2$wt[which((data1$ges.Age>=34)&(data1$ges.Age<=38))]))
#1/4 Feed
t.test(na.omit(data1$feed1[which(data1$ges.Age<30)]),na.omit(data2$feed1[which(data2$ges.Age<30)]))
t.test(na.omit(data1$feed1[which((data1$ges.Age>=30)&(data1$ges.Age<34))]),na.omit(data2$feed1[which((data1$ges.Age>=30)&(data1$ges.Age<34))]))
t.test(na.omit(data1$feed1[which((data1$ges.Age>=34)&(data1$ges.Age<=38))]),na.omit(data2$feed1[which((data1$ges.Age>=34)&(data1$ges.Age<=38))]))
#1/2 Feed
t.test(na.omit(data1$feed2[which(data1$ges.Age<30)]),na.omit(data2$feed2[which(data2$ges.Age<30)]))
t.test(na.omit(data1$feed2[which((data1$ges.Age>=30)&(data1$ges.Age<34))]),na.omit(data2$feed2[which((data1$ges.Age>=30)&(data1$ges.Age<34))]))
t.test(na.omit(data1$feed2[which((data1$ges.Age>=34)&(data1$ges.Age<=38))]),na.omit(data2$feed2[which((data1$ges.Age>=34)&(data1$ges.Age<=38))]))
#3/4 Feed
t.test(na.omit(data1$feed3[which(data1$ges.Age<30)]),na.omit(data2$feed3[which(data2$ges.Age<30)]))
t.test(na.omit(data1$feed3[which((data1$ges.Age>=30)&(data1$ges.Age<34))]),na.omit(data2$feed3[which((data1$ges.Age>=30)&(data1$ges.Age<34))]))
t.test(na.omit(data1$feed3[which((data1$ges.Age>=34)&(data1$ges.Age<=38))]),na.omit(data2$feed3[which((data1$ges.Age>=34)&(data1$ges.Age<=38))]))
#Full Feed
t.test(na.omit(data1$fullfeed[which(data1$ges.Age<30)]),na.omit(data2$fullfeed[which(data2$ges.Age<30)]))
t.test(na.omit(data1$fullfeed[which((data1$ges.Age>=30)&(data1$ges.Age<34))]),na.omit(data2$fullfeed[which((data1$ges.Age>=30)&(data1$ges.Age<34))]))
t.test(na.omit(data1$fullfeed[which((data1$ges.Age>=34)&(data1$ges.Age<=38))]),na.omit(data2$fullfeed[which((data1$ges.Age>=34)&(data1$ges.Age<=38))]))




