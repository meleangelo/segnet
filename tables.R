## TABLES PAPER April 2019


################################################################
# TABLE ESTIMATES, FULL MODEL

rm(list=ls())

file <- ""


#x<-read.table("parbayes_exch_shares_parall_june2011_4thrun.txt")
x<-read.table("parbayes_exch_shares_parall_june2011_5thrun.txt")

library(prettyR)

q5<-function(x) quantile(x,probs=0.05)  # 5 pctile function
q95<-function(x) quantile(x,probs=0.95) # 95 pctile function

meanx<-apply(x,2,mean)
sdx<-apply(x,2,sd)
med<-apply(x,2,median)
mode<-apply(x,2,Mode)
p5<-apply(x,2,q5)
p95<-apply(x,2,q95)

##### mcmc se batch means
batch<-5000
meanse<-array(0,c(dim(x)[2],dim(x)[1]/batch))
meanse[,1]<-apply(x[1:batch,],2,mean)
for (i in 2:(dim(x)[1]/batch) ) {
  meanse[,i]<-apply(x[seq((i-1)*batch+1,i*batch,1),],2,mean)
}
#mcse<-apply((meanse[,5:dim(meanse)[2]]-meanx)^2,1,mean) 
mcse<-apply((meanse-meanx)^2,1,mean) 

mcse
cbind(meanx,med,sdx,mcse,p5,p95)
cbind(meanx,med,sdx,mcse)

mm<-matrix(round(cbind(meanx,med,sdx,p5,p95),4),c(dim(x)[2],5))
rownames(mm) <- c("CONSTANT","SAME GENDER","SAME GRADE","WHITE-WHITE","BLACK-BLACK","HISP-HISP","BEAUTY i","BEAUTY j","PERSONALITY i","PERSONALITY j","Income i - Income j","Income i + Income j","FRACTION WHITES","FRACTION BLACKS","FRACTION HISP","WHITE-WHITE * FRACTION WHITE","BLACK-BLACK * FRACTION BLACKS","HISP-HISP * FRACTION HISP","SCHOOL 1","SCHOOL 2","SCHOOL 3","SCHOOL 4","SCHOOL 5","SCHOOL 6","SCHOOL 7","SCHOOL 8","SCHOOL 9","SCHOOL 10","SCHOOL 11","SCHOOL 12","SCHOOL 13","CONSTANTm","SAME GENDERm","SAME GRADEm","WHITE-WHITEm","BLACK-BLACKm","HISP-HISPm","BEAUTY i=j YESm","BEAUTY i=j NOm",
                  "PERSONALITY i=j YESm","PERSONALITY i=j NOm","CONSTANTv","SAME GENDERv","SAME GRADEv","WHITE-WHITEv","BLACK-BLACKv","HISP-HISPv")

colnames(mm) <- c("mean", "median", "std. dev.", "5 pctile", "95 pctile")
library(xtable)
print(xtable(mm,digits=4),type="latex")


