###########################################
# program: datasets_addhealth
# file: datasets.txt
# author: Angelo Mele
# date created: 07/22/2009
# last modified: 07/22/2009
# description: this file constructs the datasets to be
#              used in estimation
#              
############################################



########################################
#  STEP 1: INDIVIDUAL SCHOOL FILES
#######################################

# clean memory
rm(list=ls())

for (ss in 1:145) {
  
  ###  there is something wrong with school 86 and 146
  ###  therefore i get rid of those
  if (ss != 86) {
    
    
    # import attribute files
    attr<-read.csv("/hpcapp1/home/amele1/addhealth/intermediate/indivattributes.csv")
    
    # keep only data for the school
    attrsch<-attr[attr$SCID==ss,]
    
    # keep only if there are no missing values
    for (i in 1:dim(attrsch)[2]) {
      attrsch<-attrsch[!is.na(attrsch[,i]),]
    }
    
    # gen numerical id
    numid<-1:length(attrsch[,1])
    
    # consider only networks with more than 30 students
    if ( length(attrsch[,1]>30) ) {
      
      # attach to id
      attach(attrsch)
      ids<-cbind(numid,id)
      
      # create dataset with individual attributes
      x<-cbind(race,gender,grade)
      urls<-paste("/hpcapp1/home/amele1/addhealth/schools/x",ss,".txt",sep="")
      write.table(x,file=urls,row.names=FALSE, col.names=FALSE)
      
      # get data of links
      gg<-read.csv(file="/hpcapp1/home/amele1/addhealth/intermediate/friendlist.csv")
      names(gg)[2:3]<-c("idresp","idnom")
      
      # merge with ids
      m1<-merge(gg,ids,by.x="idresp",by.y="id")
      names(m1)[4]<-"numidresp"
      m2<-merge(m1,ids,by.x="idnom",by.y="id")
      names(m2)[5]<-"numidnom"
      
      # create list of links
      attach(m2)
      glist<-cbind(numidresp,numidnom)
      #####names(glist)<-c("i","j")
      
      # dataset with list of links
      urls<-paste("/hpcapp1/home/amele1/addhealth/schools/g",ss,".txt",sep="")
      write.table(glist,file=urls,row.names=FALSE, col.names=FALSE)
      
    } # if ( length(attrsch[,1]>30) )
    
  } # if (ss!=86)
  
  
  # clean memory
  rm(list=ls())
  
}







########################################
#  STEP 2: INDIVIDUAL COMMUNITY FILES
#######################################

# clean memory
rm(list=ls())

for (ss in 1:90) {
  
  
  # import attribute files
  attr<-read.csv("/hpcapp1/home/amele1/addhealth/intermediate/indivattributes.csv")
  
  # keep only data for the school
  attrsch<-attr[attr$COMMID==ss,]
  
  # keep only if there are no missing values
  for (i in 1:dim(attrsch)[2]) {
    attrsch<-attrsch[!is.na(attrsch[,i]),]
  }
  
  # gen numerical id
  numid<-1:length(attrsch[,1])
  
  # consider only networks with more than 30 students
  if ( length(attrsch[,1]>30) ) {
    
    # attach to id
    attach(attrsch)
    ids<-cbind(numid,id)
    
    # create dataset with individual attributes
    x<-cbind(race,gender,grade)
    urls<-paste("/hpcapp1/home/amele1/addhealth/communities/xc",ss,".txt",sep="")
    write.table(x,file=urls,row.names=FALSE, col.names=FALSE)
    
    # get data of links
    gg<-read.csv(file="/hpcapp1/home/amele1/addhealth/intermediate/friendlist.csv")
    names(gg)[2:3]<-c("idresp","idnom")
    
    # merge with ids
    m1<-merge(gg,ids,by.x="idresp",by.y="id")
    names(m1)[4]<-"numidresp"
    m2<-merge(m1,ids,by.x="idnom",by.y="id")
    names(m2)[5]<-"numidnom"
    
    # create list of links
    attach(m2)
    glist<-cbind(numidresp,numidnom)
    #####names(glist)<-c("i","j")
    
    # dataset with list of links
    urls<-paste("/hpcapp1/home/amele1/addhealth/communities/gc",ss,".txt",sep="")
    write.table(glist,file=urls,row.names=FALSE, col.names=FALSE)
    
  } # if ( length(attrsch[,1]>30) )
  
  
  # clean memory
  rm(list=ls())
  
}









########################################
#  STEP 3: INDIVIDUAL SCHOOL FILES, SATURATED SCHOOLS
#######################################

# clean memory
rm(list=ls())

for (ss in 1:16) {
  
  saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
  
  
  # import attribute files
  attr<-read.csv("/hpcapp1/home/amele1/addhealth/intermediate/indivattributes_sat.csv")
  
  # keep only data for the school
  attrsch<-attr[attr$SCID==saturated[ss],]
  
  # keep only if there are no missing values
  for (i in 1:dim(attrsch)[2]) {
    attrsch<-attrsch[!is.na(attrsch[,i]),]
  }
  
  # gen numerical id
  numid<-1:length(attrsch[,1])
  
  # attach to id
  attach(attrsch)
  ids<-cbind(numid,id)
  
  # create dataset with individual attributes
  x<-cbind(race,gender,grade)
  urls<-paste("/hpcapp1/home/amele1/addhealth/schools/saturated/x",saturated[ss],".txt",sep="")
  write.table(x,file=urls,row.names=FALSE, col.names=FALSE)
  
  # get data of links
  gg<-read.csv(file="/hpcapp1/home/amele1/addhealth/intermediate/friendlist.csv")
  names(gg)[2:3]<-c("idresp","idnom")
  
  # merge with ids
  m1<-merge(gg,ids,by.x="idresp",by.y="id")
  names(m1)[4]<-"numidresp"
  m2<-merge(m1,ids,by.x="idnom",by.y="id")
  names(m2)[5]<-"numidnom"
  
  # create list of links
  attach(m2)
  glist<-cbind(numidresp,numidnom)
  #####names(glist)<-c("i","j")
  
  # dataset with list of links
  urls<-paste("/hpcapp1/home/amele1/addhealth/schools/saturated/g",saturated[ss],".txt",sep="")
  write.table(glist,file=urls,row.names=FALSE, col.names=FALSE)
  
  
  
  
  # clean memory
  rm(list=ls())
  
}












########################################
#  STEP 4: INDIVIDUAL SCHOOL FILES, SATURATED SCHOOLS
#          VARIABLES AT 6/16/2011
#######################################

# clean memory
rm(list=ls())

for (ss in 1:16) {
  
  saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
  
  
  # import attribute files
  attr<-read.csv("/hpcapp1/home/amele1/addhealth/intermediate/indivattributes_sat2.csv")
  
  # keep only data for the school
  attrsch<-attr[attr$SCID==saturated[ss],]
  
  # keep only if there are no missing values
  for (i in 1:dim(attrsch)[2]) {
    attrsch<-attrsch[!is.na(attrsch[,i]),]
  }
  
  # gen numerical id
  numid<-1:length(attrsch[,1])
  
  # attach to id
  attach(attrsch)
  ids<-cbind(numid,id)
  
  # create dataset with individual attributes
  x<-cbind(race,gender,grade,income,beauty,personality,school1,school2,school3,school4,school5,school6,school7,school8,school9,school10,school11,school12,school13,school14,school15,school16)
  
  
  urls<-paste("/hpcapp1/home/amele1/addhealth/schools/saturated/x",saturated[ss],".txt",sep="")
  write.table(x,file=urls,row.names=FALSE, col.names=FALSE)
  
  # get data of links
  gg<-read.csv(file="/hpcapp1/home/amele1/addhealth/intermediate/friendlist.csv")
  names(gg)[2:3]<-c("idresp","idnom")
  
  # merge with ids
  m1<-merge(gg,ids,by.x="idresp",by.y="id")
  names(m1)[4]<-"numidresp"
  m2<-merge(m1,ids,by.x="idnom",by.y="id")
  names(m2)[5]<-"numidnom"
  
  # create list of links
  attach(m2)
  glist<-cbind(numidresp,numidnom)
  #####names(glist)<-c("i","j")
  
  # dataset with list of links
  urls<-paste("/hpcapp1/home/amele1/addhealth/schools/saturated/g",saturated[ss],".txt",sep="")
  write.table(glist,file=urls,row.names=FALSE, col.names=FALSE)
  
  
  
  
  # clean memory
  rm(list=ls())
  
}



