## TABLES PAPER April 2019


rm(list=ls())

# Table full model (old specification)

resultsfolder <- "D:/segnet/results/estimation/"
estimatesfile <- "Rmpi_estimation_run3.RData"
file <- paste(resultsfolder, estimatesfile, sep="")
load(file)

x <- Theta
library(prettyR)

q5<-function(x) quantile(x,probs=0.05)  # 5 pctile function
q95<-function(x) quantile(x,probs=0.95) # 95 pctile function

meanx<-apply(x,2,mean)
sdx<-apply(x,2,sd)
med<-apply(x,2,median)
mode<-apply(x,2,Mode)
p5<-apply(x,2,q5)
p95<-apply(x,2,q95)


mm<-matrix(round(cbind(meanx,med,sdx,p5,p95),4),c(dim(x)[2],5))
rownames(mm) <- c("CONSTANT","SAME GENDER","SAME GRADE","WHITE-WHITE","BLACK-BLACK","HISP-HISP","BEAUTY i","BEAUTY j","PERSONALITY i","PERSONALITY j","Income i - Income j","Income i + Income j","FRACTION WHITES","FRACTION BLACKS","FRACTION HISP","WHITE-WHITE * FRACTION WHITE","BLACK-BLACK * FRACTION BLACKS","HISP-HISP * FRACTION HISP","SCHOOL 1","SCHOOL 2","SCHOOL 3","SCHOOL 4","SCHOOL 5","SCHOOL 6","SCHOOL 7","SCHOOL 8","SCHOOL 9","SCHOOL 10","SCHOOL 11","SCHOOL 12","SCHOOL 13","CONSTANTm","SAME GENDERm","SAME GRADEm","WHITE-WHITEm","BLACK-BLACKm","HISP-HISPm",
                  "CONSTANTv","SAME GENDERv","SAME GRADEv","WHITE-WHITEv","BLACK-BLACKv","HISP-HISPv")

colnames(mm) <- c("mean", "median", "std. dev.", "5 pctile", "95 pctile")
library(xtable)
print(xtable(mm,digits=4),type="latex")



# Table full model, only direct utility (old specification)

resultsfolder <- "D:/segnet/results/estimation/"
estimatesfile <- "Rmpi_estimation_dirutil_run2.RData"
file <- paste(resultsfolder, estimatesfile, sep="")
load(file)

x <- Theta
library(prettyR)

q5<-function(x) quantile(x,probs=0.05)  # 5 pctile function
q95<-function(x) quantile(x,probs=0.95) # 95 pctile function

meanx<-apply(x,2,mean)
sdx<-apply(x,2,sd)
med<-apply(x,2,median)
mode<-apply(x,2,Mode)
p5<-apply(x,2,q5)
p95<-apply(x,2,q95)


mm<-matrix(round(cbind(meanx,med,sdx,p5,p95),4),c(dim(x)[2],5))
rownames(mm) <- c("CONSTANT","SAME GENDER","SAME GRADE","WHITE-WHITE","BLACK-BLACK","HISP-HISP","BEAUTY i","BEAUTY j","PERSONALITY i","PERSONALITY j","Income i - Income j","Income i + Income j","FRACTION WHITES","FRACTION BLACKS","FRACTION HISP","WHITE-WHITE * FRACTION WHITE","BLACK-BLACK * FRACTION BLACKS","HISP-HISP * FRACTION HISP","SCHOOL 1","SCHOOL 2","SCHOOL 3","SCHOOL 4","SCHOOL 5","SCHOOL 6","SCHOOL 7","SCHOOL 8","SCHOOL 9","SCHOOL 10","SCHOOL 11","SCHOOL 12","SCHOOL 13")

colnames(mm) <- c("mean", "median", "std. dev.", "5 pctile", "95 pctile")
library(xtable)
print(xtable(mm,digits=4),type="latex")



# Table full model (new specification)

resultsfolder <- "D:/segnet/results/estimation/"
estimatesfile <- "Rmpi_estimation_newspec_run3.RData"
file <- paste(resultsfolder, estimatesfile, sep="")
load(file)

x <- Theta

q5<-function(x) quantile(x,probs=0.05)  # 5 pctile function
q95<-function(x) quantile(x,probs=0.95) # 95 pctile function

meanx<-apply(x,2,mean)
sdx<-apply(x,2,sd)
med<-apply(x,2,median)
p5<-apply(x,2,q5)
p95<-apply(x,2,q95)


mm<-matrix(round(cbind(meanx,med,sdx,p5,p95),4),c(dim(x)[2],5))
rownames(mm) <- c("CONSTANT", "MALE", "WHITE", "BLACK", "HISP", "INCOME", "SAME GENDER","SAME GRADE","WHITE-WHITE","BLACK-BLACK","HISP-HISP","BEAUTY i","BEAUTY j","PERSONALITY i","PERSONALITY j","Income i - Income j","Income i + Income j","FRACTION WHITES","FRACTION BLACKS","FRACTION HISP","WHITE-WHITE * FRACTION WHITE","BLACK-BLACK * FRACTION BLACKS","HISP-HISP * FRACTION HISP","SCHOOL 1","SCHOOL 2","SCHOOL 3","SCHOOL 4","SCHOOL 5","SCHOOL 6","SCHOOL 7","SCHOOL 8","SCHOOL 9","SCHOOL 10","SCHOOL 11","SCHOOL 12","SCHOOL 13","CONSTANTm","SAME GENDERm","SAME GRADEm","WHITE-WHITEm","BLACK-BLACKm","HISP-HISPm",
                  "CONSTANTv","SAME GENDERv","SAME GRADEv","WHITE-WHITEv","BLACK-BLACKv","HISP-HISPv")

colnames(mm) <- c("mean", "median", "std. dev.", "5 pctile", "95 pctile")
library(xtable)
print(xtable(mm,digits=4),type="latex")



# Table full model (new specification)

resultsfolder <- "D:/segnet/results/estimation/"
estimatesfile <- "Rmpi_estimation_newspec_dirutil_run2.RData"
file <- paste(resultsfolder, estimatesfile, sep="")
load(file)

x <- Theta

q5<-function(x) quantile(x,probs=0.05)  # 5 pctile function
q95<-function(x) quantile(x,probs=0.95) # 95 pctile function

meanx<-apply(x,2,mean)
sdx<-apply(x,2,sd)
med<-apply(x,2,median)
p5<-apply(x,2,q5)
p95<-apply(x,2,q95)


mm<-matrix(round(cbind(meanx,med,sdx,p5,p95),4),c(dim(x)[2],5))
rownames(mm) <- c("CONSTANT", "MALE", "WHITE", "BLACK", "HISP", "INCOME", "SAME GENDER","SAME GRADE","WHITE-WHITE","BLACK-BLACK","HISP-HISP","BEAUTY i","BEAUTY j","PERSONALITY i","PERSONALITY j","Income i - Income j","Income i + Income j","FRACTION WHITES","FRACTION BLACKS","FRACTION HISP","WHITE-WHITE * FRACTION WHITE","BLACK-BLACK * FRACTION BLACKS","HISP-HISP * FRACTION HISP","SCHOOL 1","SCHOOL 2","SCHOOL 3","SCHOOL 4","SCHOOL 5","SCHOOL 6","SCHOOL 7","SCHOOL 8","SCHOOL 9","SCHOOL 10","SCHOOL 11","SCHOOL 12","SCHOOL 13")

colnames(mm) <- c("mean", "median", "std. dev.", "5 pctile", "95 pctile")
library(xtable)
print(xtable(mm,digits=4),type="latex")



# Table full model (new specification) ALL SATURATED SCHOOLS

resultsfolder <- "D:/segnet/results/estimation/"
estimatesfile <- "Rmpi_estimation_newspec_allsat_run3.RData"
file <- paste(resultsfolder, estimatesfile, sep="")
load(file)

x <- Theta

q5<-function(x) quantile(x,probs=0.05)  # 5 pctile function
q95<-function(x) quantile(x,probs=0.95) # 95 pctile function

meanx<-apply(x,2,mean)
sdx<-apply(x,2,sd)
med<-apply(x,2,median)
p5<-apply(x,2,q5)
p95<-apply(x,2,q95)


mm<-matrix(round(cbind(meanx,med,sdx,p5,p95),4),c(dim(x)[2],5))
rownames(mm) <- c("CONSTANT", "MALE", "WHITE", "BLACK", "HISP", "INCOME", "SAME GENDER","SAME GRADE","WHITE-WHITE","BLACK-BLACK","HISP-HISP","BEAUTY i","BEAUTY j","PERSONALITY i","PERSONALITY j","Income i - Income j","Income i + Income j","FRACTION WHITES","FRACTION BLACKS","FRACTION HISP","WHITE-WHITE * FRACTION WHITE","BLACK-BLACK * FRACTION BLACKS","HISP-HISP * FRACTION HISP","SCHOOL 1","SCHOOL 2","SCHOOL 3","SCHOOL 4","SCHOOL 5","SCHOOL 6","SCHOOL 7","SCHOOL 8","SCHOOL 9","SCHOOL 10","SCHOOL 11","SCHOOL 12","SCHOOL 13", "SCHOOL 14", "SCHOOL 15","CONSTANTm","SAME GENDERm","SAME GRADEm","WHITE-WHITEm","BLACK-BLACKm","HISP-HISPm",
                  "CONSTANTv","SAME GENDERv","SAME GRADEv","WHITE-WHITEv","BLACK-BLACKv","HISP-HISPv")

colnames(mm) <- c("mean", "median", "std. dev.", "5 pctile", "95 pctile")
library(xtable)
print(xtable(mm,digits=4),type="latex")



# Table full model (new specification), ALL SATURATED SCHOOLS

resultsfolder <- "D:/segnet/results/estimation/"
estimatesfile <- "Rmpi_estimation_newspec_allsat_dirutil_run3.RData"
file <- paste(resultsfolder, estimatesfile, sep="")
load(file)

x <- Theta

q5<-function(x) quantile(x,probs=0.05)  # 5 pctile function
q95<-function(x) quantile(x,probs=0.95) # 95 pctile function

meanx<-apply(x,2,mean)
sdx<-apply(x,2,sd)
med<-apply(x,2,median)
p5<-apply(x,2,q5)
p95<-apply(x,2,q95)


mm<-matrix(round(cbind(meanx,med,sdx,p5,p95),4),c(dim(x)[2],5))
rownames(mm) <- c("CONSTANT", "MALE", "WHITE", "BLACK", "HISP", "INCOME", "SAME GENDER","SAME GRADE","WHITE-WHITE","BLACK-BLACK","HISP-HISP","BEAUTY i","BEAUTY j","PERSONALITY i","PERSONALITY j","Income i - Income j","Income i + Income j","FRACTION WHITES","FRACTION BLACKS","FRACTION HISP","WHITE-WHITE * FRACTION WHITE","BLACK-BLACK * FRACTION BLACKS","HISP-HISP * FRACTION HISP","SCHOOL 1","SCHOOL 2","SCHOOL 3","SCHOOL 4","SCHOOL 5","SCHOOL 6","SCHOOL 7","SCHOOL 8","SCHOOL 9","SCHOOL 10","SCHOOL 11","SCHOOL 12","SCHOOL 13", "SCHOOL 14", "SCHOOL 15")

colnames(mm) <- c("mean", "median", "std. dev.", "5 pctile", "95 pctile")
library(xtable)
print(xtable(mm,digits=4),type="latex")


#### SEGREGATION AND OTHER INDICATORS FOR TABLE 1

rm(list=ls())
setwd("/Users/Angelo/Dropbox/segnetworks/results/")
load("table1_seg.Rdata")


library(xtable)

keep_rows <- c(1,2,6,7,9,10,14,15,18,22,23,25,27)
table1add <- tables[keep_rows,]
rownames(table1add) <- c("FSI gender", "FSI race", "FSI income 90", "FSI income 50", 
                         "SSI gender", "SSI race", "SSI income 90", "SSI income 50",
                         "CHI females", "CHI income 90", "CHI income 50", 
                         "Clustering", "Density")
xtable(table1add, digits=3)
