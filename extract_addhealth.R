###########################################
# program: extract_addhealth
# author: Angelo Mele
# date created: 07/20/2009
# last modified: 01/20/2019
# description: this file extracts the AddHealth files
#              from the SAS XPORT format into R. 
#              the files are then saved in csv format
#              in a folder of intermediate datasets
#
############################################


# clean memory
rm(list = ls())

# load library for sas files
library(foreign)


########################################
#  STEP 1: FRIEND FILES
#######################################

# clean memory
rm(list = ls())

# load library for sas files
library(foreign)

# set folder where friend files are contained as working folder
setwd("E:/backup_phoenix/addhealth/data/ICPSR_27022/DS0001/")

# extract friend data for Wave 1
xx<-read.xport("DA27022P1.XPT")

# save data in csv format in intermediate folder
write.csv(xx, file="E:/backup_phoenix/addhealth/intermediate/friendraw.csv")

# clean memory
rm(list=ls())



############################################################
#  STEP 2: INDIVIDUAL ATTRIBUTES FROM IN-HOME INTERVIEWS
############################################################

# clean memory
rm(list = ls())

# load library for sas files
library(foreign)

# set folder where friend files are contained as working folder
setwd("/hpcapp1/home/amele1/addhealth/data/ICPSR_27021/DS0001/")


# extract data for Wave 1
xx<-read.xport("DA27021P1.XPT")

# create variable for ID of individuals
id<-array(0,20745)
id<-as.character(xx[,1])
id<-as.numeric(id)

# attach dataset to call variables by name
attach(xx)

# extract only the following variables
data<-cbind(AID,id,SCID,SSCID,COMMID,H1GI6A,H1GI6B,H1GI6C,H1GI6D,H1GI6E,H1GI7A,H1GI7B,H1GI7C,H1GI7D,H1GI7E,H1GI7F,H1GI7G,H1GI8,H1GI9,H1GH59A,H1GH59B,H1GH60,H1RE1,H1GI1M,H1GI1Y,H1GI4,H1GI19,H1GI20,BIO_SEX)

# remove big dataset to save RAM
rm(xx)

# save data in csv format in intermediate folder
write.csv(data, file="/hpcapp1/home/amele1/addhealth/intermediate/attributesraw.csv")

# clean memory
rm(list = ls())







############################################################
#  STEP 6: INDIVIDUAL ATTRIBUTES FOR SATURATED SAMPLE IN-HOME INTERVIEWS
#          NEW VARIABLES AS OF 6/16/2011
############################################################

# clean memory
rm(list = ls())

# load library for sas files
library(foreign)

# set folder where friend files are contained as working folder
setwd("/hpcapp1/home/amele1/addhealth/data/ICPSR_27021/DS0001/")

# extract data for Wave 1
xx<-read.xport("DA27021P1.XPT")


# create variable for ID of individuals
id<-array(0,20745)
id<-as.character(xx[,1])
id<-as.numeric(id)

# attach dataset to call variables by name
attach(xx)

# extract only the following variables
SCID<-as.character(SCID)
SCID<-as.numeric(SCID)




#### check for each school the number of missing and nonresponse for income
#### and replace it with a random draw from the unconditional distribution (log normal)

pa55imp<-array(NA,length(PA55))
saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
table<-matrix(nrow=16,ncol=5)
for (ss in 1:16) {
  
  a<-length(PA55[SCID==saturated[ss]])
  b<-sum(na.omit(PA55[SCID==saturated[ss]])==9996)
  c<-sum(is.na(PA55[SCID==saturated[ss]]))
  table[ss,1]<-a
  table[ss,2]<-b
  table[ss,3]<-c
  table[ss,4]<-b/a
  table[ss,5]<-c/a
  
  inc<-array(na.omit(PA55[PA55!=9996 & SCID==saturated[ss]]))
  linc<-log(inc+.000000001)
  e<-mean(linc)
  f<-sd(linc)
  x<-rnorm(a+b,e,f)
  y<-exp(x)
  pa<-PA55[SCID==saturated[ss]]
  k<-1
  for (i in 1:length(pa)) {
    if (is.na(pa[i])) {
      pa[i]<-y[k]
      k<-k+1
    }
    if (pa[i]==9996) {
      pa[i]<-y[k]
      k<-k+1
    }
    if (k>a+b) break
    
  }
  pa55imp[SCID==saturated[ss]]<-pa
}



data<-cbind(AID,id,SCID,SSCID,COMMID,H1GI6A,H1GI6B,H1GI6C,H1GI6D,H1GI6E,H1GI7A,H1GI7B,H1GI7C,H1GI7D,H1GI7E,H1GI7F,H1GI7G,H1GI8,H1GI9,H1GH59A,H1GH59B,H1GH60,H1RE1,H1GI1M,H1GI1Y,H1GI4,H1GI19,H1GI20,BIO_SEX,PA55,pa55imp,PA12,PB8,H1RM1,H1RF1,H1IR1,H1IR2,H1IR5)

# remove big dataset to save RAM
rm(xx)

# save data in csv format in intermediate folder
write.csv(data, file="/hpcapp1/home/amele1/addhealth/intermediate/attributesraw_sat2.csv")

# clean memory
rm(list = ls())






############################################################
#  STEP 6: CHECKS FOR MISSING VALUES IN PARENTAL VARIABLES, SATURATED SAMPLE
############################################################

# clean memory
rm(list = ls())

# load library for sas files
library(foreign)

# set folder where friend files are contained as working folder
setwd("/hpcapp1/home/amele1/addhealth/data/ICPSR_27021/DS0001/")

# extract data for Wave 1
xx<-read.xport("DA27021P1.XPT")

# create variable for ID of individuals
id<-array(0,20745)
id<-as.character(xx[,1])
id<-as.numeric(id)

# attach dataset to call variables by name
attach(xx)


# extract only the following variables
SCID<-as.character(SCID)
SCID<-as.numeric(SCID)


# check for each school
for (ss in 1:16) {
  
  saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
  cbind(H1GI6A[SCID==saturated[ss]],BIO_SEX[SCID==saturated[ss]],PA55[SCID==saturated[ss]],PA12[SCID==saturated[ss]],PB8[SCID==saturated[ss]],H1RM1[SCID==saturated[ss]],H1RF1[SCID==saturated[ss]])
  
}



#### check for each school the number of missing and nonresponse for income
#### and replace it with a random draw from the unconditional distribution (log normal)

pa55imp<-array(NA,length(PA55))
saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
table<-matrix(nrow=16,ncol=5)
for (ss in 1:16) {
  
  a<-length(PA55[SCID==saturated[ss]])
  b<-sum(na.omit(PA55[SCID==saturated[ss]])==9996)
  c<-sum(is.na(PA55[SCID==saturated[ss]]))
  table[ss,1]<-a
  table[ss,2]<-b
  table[ss,3]<-c
  table[ss,4]<-b/a
  table[ss,5]<-c/a
  
  inc<-array(na.omit(PA55[PA55!=9996 & SCID==saturated[ss]]))
  linc<-log(inc+.000000001)
  e<-mean(linc)
  f<-sd(linc)
  x<-rnorm(a+b,e,f)
  y<-exp(x)
  pa<-PA55[SCID==saturated[ss]]
  k<-1
  for (i in 1:length(pa)) {
    if (is.na(pa[i])) {
      pa[i]<-y[k]
      k<-k+1
    }
    if (pa[i]==9996) {
      pa[i]<-y[k]
      k<-k+1
    }
    if (k>a+b) break
    
  }
  pa55imp[SCID==saturated[ss]]<-pa
}



#### check for each school the number of missing and nonresponse for edu

saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
table<-matrix(nrow=16,ncol=7)
for (ss in 1:16) {
  
  a<-length(PA12[SCID==saturated[ss]])
  b<-sum(na.omit(PA12[SCID==saturated[ss]])==96)
  c<-sum(is.na(PA12[SCID==saturated[ss]]))
  d<-sum(na.omit(PA12[SCID==saturated[ss]])==97)
  
  table[ss,1]<-a
  table[ss,2]<-b
  table[ss,3]<-d
  table[ss,4]<-c
  table[ss,5]<-b/a
  table[ss,6]<-d/a
  table[ss,7]<-c/a
  
  
}




#### check for each school the number of missing and nonresponse for edu

saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
table<-matrix(nrow=16,ncol=5)
for (ss in 1:16) {
  
  a<-length(PB8[SCID==saturated[ss]])
  b<-sum(na.omit(PB8[SCID==saturated[ss]])==97)
  c<-sum(is.na(PB8[SCID==saturated[ss]]))
  table[ss,1]<-a
  table[ss,2]<-b
  table[ss,3]<-c
  table[ss,4]<-b/a
  table[ss,5]<-c/a
  
}

table





#### check for each school the number of missing and nonresponse for edu

saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
table<-matrix(nrow=16,ncol=7)
for (ss in 1:16) {
  
  a<-length(H1RM1[SCID==saturated[ss]])
  b<-sum(na.omit(H1RM1[SCID==saturated[ss]])==96)
  c<-sum(is.na(H1RM1[SCID==saturated[ss]]))
  d<-sum(na.omit(H1RM1[SCID==saturated[ss]])==97)
  
  table[ss,1]<-a
  table[ss,2]<-b
  table[ss,3]<-d
  table[ss,4]<-c
  table[ss,5]<-b/a
  table[ss,6]<-d/a
  table[ss,7]<-c/a
  
  
}

