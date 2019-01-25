###########################################
# program: tables
# file: tables.txt
# author: Angelo Mele
# date created: 07/27/2009
# last modified: 01/21/2019
# description: this file creates some of the 
#              descriptive stats tables
#
############################################


# clean memory
rm(list = ls())


######################################################
#  TABLE 1: DESCRIPTIVE STATS OF INDIVIDUAL SCHOOLS
######################################################


# library
library(statnet)


# initialize all variables

TotStudents<-array(NA,146)
TotLinks<-array(NA,146)
AvgDegree<-array(NA,146)
Density<-array(NA,146)
Whites<-array(NA,146)	
Blacks<-array(NA,146)	
Asians<-array(NA,146)		
Hispanics<-array(NA,146)		
Others<-array(NA,146)		

FragmRace<-array(NA,146)	

Females<-array(NA,146)		

Seven<-array(NA,146)	
Eight<-array(NA,146)	
Nine<-array(NA,146)	
Ten<-array(NA,146)	
Eleven<-array(NA,146)	
Twelve<-array(NA,146)		



for (ss in 1:145) {
  #for (ss in 25:25) {
  #for (ss in 48:48) {
  
  ###  there is something wrong with school 86 and 146
  ###  therefore i get rid of those. also schools 13,100,137,142 have no links
  if (ss != 86 & ss != 13 & ss!=100 & ss!=137 & ss!=142) {
    
    
    
    urls<-paste("E:/backup_phoenix/addhealth/schools/x",ss,".txt",sep="")
    nodeinfo<-read.table(urls)
    names(nodeinfo)<-c("race","gender","grade")
    urls<-paste("E:/backup_phoenix/addhealth/schools/g",ss,".txt",sep="")
    edgelist<-read.table(urls)
    
    if (length(edgelist[,1])>1 & length(nodeinfo$race)>30) {
      
      
      #### networks attributes: race
      TotStudents[ss]<-length(nodeinfo$race)
      TotLinks[ss]<-length(edgelist[,1])
      AvgDegree[ss]<-TotLinks[ss]/TotStudents[ss]
      Density[ss]<-TotLinks[ss]/(TotStudents[ss]^2)
      Whites[ss]<-sum(nodeinfo$race==1)/TotStudents[ss]	
      Blacks[ss]<-sum(nodeinfo$race==2)/TotStudents[ss]	
      Asians[ss]<-sum(nodeinfo$race==3)/TotStudents[ss]	
      Hispanics[ss]<-sum(nodeinfo$race==4)/TotStudents[ss]	
      Others[ss]<-sum(nodeinfo$race==5)/TotStudents[ss]	
      
      #### networks attributes: gender
      Females[ss]<-sum(nodeinfo$gender==1)/TotStudents[ss]	
      
      #### networks attributes: grade
      Seven[ss]<-sum(nodeinfo$grade==7)/TotStudents[ss]
      Eight[ss]<-sum(nodeinfo$grade==8)/TotStudents[ss]
      Nine[ss]<-sum(nodeinfo$grade==9)/TotStudents[ss]
      Ten[ss]<-sum(nodeinfo$grade==10)/TotStudents[ss]
      Eleven[ss]<-sum(nodeinfo$grade==11)/TotStudents[ss]
      Twelve[ss]<-sum(nodeinfo$grade==12)/TotStudents[ss]	
      
      #### fragmentation
      FragmRace[ss]<-1-Whites[ss]^2-Blacks[ss]^2-Asians[ss]^2-Hispanics[ss]^2-Others[ss]^2
      
      
    }
  }
  
}

# school id
school<-1:146


#### Graphs 

#pdf("/home/econ/amele2/socnet/results/FragVsLinks.pdf")
plot(FragmRace,TotLinks)
#dev.off()

#### TABLE 1 : Students, Racial composition and gender composition (EACH SCHOOL)

table1<-cbind(school,TotStudents,TotLinks, AvgDegree, Density,Females,Whites,Blacks,Asians,Hispanics,Others,FragmRace)



#### TABLE 2: Grade composition (EACH SCHOOL)

table2<-cbind(school,TotStudents,TotLinks, AvgDegree, Density,Seven,Eight,Nine,Ten,Eleven,Twelve)



#### TABLE 1A: summary stats for racial and gender composition

TotStudents<-summary(TotStudents)
TotLinks<-summary(TotLinks)
AvgDegree<- summary(AvgDegree)
Density<- summary(Density)
Whites<-summary(Whites)
Blacks<-summary(Blacks)
Asians<-summary(Asians)
Hispanics<-summary(Hispanics)
Others<-summary(Others)
Females<-summary(Females)
FragmRace<-summary(FragmRace)

table1a<-rbind(TotStudents,TotLinks,AvgDegree,Density,Whites,Blacks,Asians,Hispanics,Others,Females,FragmRace)


#### TABLE 2A: summary stats for grade composition

Seven<-summary(Seven)
Eight<-summary(Eight)
Nine<-summary(Nine)
Ten<-summary(Ten)
Eleven<-summary(Eleven)
Twelve<-summary(Twelve)

table2a<-rbind(Seven,Eight,Nine,Ten,Eleven,Twelve)



#### Tables in tab format

write.table(round(table1,5),file="D:/segnet/results/descriptive/Table1.txt",row.names=FALSE)
write.table(round(table1a,5),file="D:/segnet/results/descriptive/Table1a.txt",row.names=TRUE)
write.table(round(table2,5),file="D:/segnet/results/descriptive/Table2.txt",row.names=FALSE)
write.table(round(table2a,5),file="D:/segnet/results/descriptive/Table2a.txt",row.names=TRUE)



#### Tables in TeX format (copy in LaTeX file)

write.table(round(table1,5),file="D:/segnet/results/descriptive/Table1TeX.txt",row.names=FALSE, sep=" & ")
write.table(round(table1a,5),file="D:/segnet/results/descriptive/Table1aTeX.txt",row.names=TRUE, sep=" & ")
write.table(round(table2,5),file="D:/segnet/results/descriptive/Table2TeX.txt",row.names=FALSE, sep=" & ")
write.table(round(table2a,5),file="D:/segnet/results/descriptive/Table2aTeX.txt",row.names=TRUE, sep=" & ")


# clean memory
rm(list=ls()) 






# clean memory
rm(list = ls())


######################################################
#  TABLE 1: DESCRIPTIVE STATS OF SATURATED SAMPLE SCHOOLS
######################################################


# library
library(statnet)


# initialize all variables

TotStudents<-array(NA,16)
TotLinks<-array(NA,16)
Whites<-array(NA,16)	
Blacks<-array(NA,16)	
Asians<-array(NA,16)		
Hispanics<-array(NA,16)		
Others<-array(NA,16)		

FragmRace<-array(NA,16)	

Females<-array(NA,16)		

Seven<-array(NA,16)	
Eight<-array(NA,16)	
Nine<-array(NA,16)	
Ten<-array(NA,16)	
Eleven<-array(NA,16)	
Twelve<-array(NA,16)		







for (ss in 1:16) {
  
  saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
  
  
  urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/x",saturated[ss],".txt",sep="")
  nodeinfo<-read.table(urls)
  names(nodeinfo)<-c("race","gender","grade")
  urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/g",saturated[ss],".txt",sep="")
  edgelist<-read.table(urls)
  
  
  
  #### networks attributes: race
  TotStudents[ss]<-length(nodeinfo$race)
  TotLinks[ss]<-length(edgelist[,1])
  Whites[ss]<-sum(nodeinfo$race==1)/TotStudents[ss]	
  Blacks[ss]<-sum(nodeinfo$race==2)/TotStudents[ss]	
  Asians[ss]<-sum(nodeinfo$race==3)/TotStudents[ss]	
  Hispanics[ss]<-sum(nodeinfo$race==4)/TotStudents[ss]	
  Others[ss]<-sum(nodeinfo$race==5)/TotStudents[ss]	
  
  #### networks attributes: gender
  Females[ss]<-sum(nodeinfo$gender==1)/TotStudents[ss]	
  
  #### networks attributes: grade
  Seven[ss]<-sum(nodeinfo$grade==7)/TotStudents[ss]
  Eight[ss]<-sum(nodeinfo$grade==8)/TotStudents[ss]
  Nine[ss]<-sum(nodeinfo$grade==9)/TotStudents[ss]
  Ten[ss]<-sum(nodeinfo$grade==10)/TotStudents[ss]
  Eleven[ss]<-sum(nodeinfo$grade==11)/TotStudents[ss]
  Twelve[ss]<-sum(nodeinfo$grade==12)/TotStudents[ss]	
  
  #### fragmentation
  FragmRace[ss]<-1-Whites[ss]^2-Blacks[ss]^2-Asians[ss]^2-Hispanics[ss]^2-Others[ss]^2
  
  
  
  
}

# school id
school<-saturated

#### TABLE 1 : Students, Racial composition and gender composition (EACH SCHOOL)

table_sat1<-rbind(school,TotStudents,TotLinks,Females,Whites,Blacks,Asians,Hispanics,Others,FragmRace,Seven,Eight,Nine,Ten,Eleven,Twelve)

#### TABLE 1 LaTeX
write.table(round(table_sat1,3),file="D:/segnet/results/descriptive/Table_sat1TeX.txt",col.names=FALSE, sep=" & ")

