# create policy datasets
# this file creates several network datasets based on Add Health schools
# that are used in the policy experiments.

rm(list=ls())
library(statnet)

satschool <- 77 # largest diverse network
urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/net",satschool,".Rdata",sep="")
load(urls)

whites <- which(get.vertex.attribute(net, "white")==1)
blacks <- which(get.vertex.attribute(net, "black")==1)
asians <- which(get.vertex.attribute(net, "asian")==1)
hisps  <- which(get.vertex.attribute(net, "hisp")==1)
others <- which(get.vertex.attribute(net, "other")==1)

#by income
income <- get.vertex.attribute(net, "loginc")

quintiles <- quantile(income, probs = c(.2,.4,.6,.8))

inc1 <- which(income <= quintiles[1])
inc2 <- which(income > quintiles[1] & income <= quintiles[2])
inc3 <- which(income > quintiles[2] & income <= quintiles[3])
inc4 <- which(income > quintiles[3] & income <= quintiles[4])
inc5 <- which(income > quintiles[4])


  

netw <-network( net[whites, whites])
attrnames <- list.vertex.attributes(net)
for (i in 1:length(attrnames)) {
  attrvalue <- get.vertex.attribute(net, attrnames[i])
  attrvalue <- attrvalue[whites] 
  set.vertex.attribute(netw, attrnames[i], attrvalue)
}
netw





##################################
# SCHOOL  88 and 106: swap students of different racial groups
###################################

rm(list=ls())
library(statnet)

satschool <- 88 # largest diverse network
urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/net",satschool,".Rdata",sep="")
load(urls)

# create dataset with all attributes of network 88
whites88 <- which(get.vertex.attribute(net, "white")==1) # white students ids
blacks88 <- which(get.vertex.attribute(net, "black")==1) 
asians88 <- which(get.vertex.attribute(net, "asian")==1)
hisps88  <- which(get.vertex.attribute(net, "hisp")==1)
others88 <- which(get.vertex.attribute(net, "other")==1)


attrnames <- list.vertex.attributes(net)
attr88 <- data.frame(matrix(NA, nrow = 90, ncol = length(attrnames)))
names(attr88) <- attrnames
for (i in 1:length(attrnames)) {
  attrvalue <- get.vertex.attribute(net, attrnames[i])
  attr88[,i] <- attrvalue 
}
attr88$vertex.names <- NULL



# create datasset for school 106
satschool <- 106 
urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/net",satschool,".Rdata",sep="")
load(urls)

# create dataset with all attributes of network 106
whites106 <- which(get.vertex.attribute(net, "white")==1) # white students ids
blacks106 <- which(get.vertex.attribute(net, "black")==1) 
asians106 <- which(get.vertex.attribute(net, "asian")==1)
hisps106  <- which(get.vertex.attribute(net, "hisp")==1)
others106 <- which(get.vertex.attribute(net, "other")==1)


attrnames <- list.vertex.attributes(net)
attr106 <- data.frame(matrix(NA, nrow = 81, ncol = length(attrnames)))
names(attr106) <- attrnames
for (i in 1:length(attrnames)) {
  attrvalue <- get.vertex.attribute(net, attrnames[i])
  attr106[,i] <- attrvalue 
}
attr106$vertex.names <- NULL



# policy allocations
attr88_1 <- attr88 # initial value
attr106_1 <- attr106 # initial value
attr88_2 <- rbind(attr88[whites88[1:84],], 
                  attr106[blacks106[1:5],], 
                  others88)
attr106_2 <- rbind(attr106[whites88[85:length(whites88)],], 
                   attr106[blacks106[6:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_3 <- rbind(attr88[whites88[1:79],], 
                  attr106[blacks106[1:10],], 
                  others88)
attr106_3 <- rbind(attr88[whites88[80:length(whites88)],], 
                   attr106[blacks106[11:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_4 <- rbind(attr88[whites88[1:74],], 
                  attr106[blacks106[1:15],], 
                  others88)
attr106_4 <- rbind(attr88[whites88[75:length(whites88)],], 
                   attr106[blacks106[16:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_5 <- rbind(attr88[whites88[1:69],], 
                  attr106[blacks106[1:20],], 
                  others88)
attr106_5 <- rbind(attr88[whites88[70:length(whites88)],], 
                   attr106[blacks106[21:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_6 <- rbind(attr88[whites88[1:64],], 
                  attr106[blacks106[1:25],], 
                  others88)
attr106_6 <- rbind(attr88[whites88[65:length(whites88)],], 
                   attr106[blacks106[26:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_7 <- rbind(attr88[whites88[1:59],], 
                  attr106[blacks106[1:30],], 
                  others88)
attr106_7 <- rbind(attr88[whites88[60:length(whites88)],], 
                   attr106[blacks106[31:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_8 <- rbind(attr88[whites88[1:54],], 
                  attr106[blacks106[1:35],], 
                  others88)
attr106_8 <- rbind(attr88[whites88[55:length(whites88)],], 
                   attr106[blacks106[36:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_9 <- rbind(attr88[whites88[1:49],], 
                  attr106[blacks106[1:40],], 
                  others88)
attr106_9 <- rbind(attr88[whites88[50:length(whites88)],], 
                   attr106[blacks106[41:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_10 <- rbind(attr88[whites88[1:44],], 
                  attr106[blacks106[1:45],], 
                  others88)
attr106_10 <- rbind(attr88[whites88[45:length(whites88)],], 
                   attr106[blacks106[46:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_11 <- rbind(attr88[whites88[1:39],], 
                  attr106[blacks106[1:50],], 
                  others88)
attr106_11 <- rbind(attr88[whites88[40:length(whites88)],], 
                   attr106[blacks106[51:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_12 <- rbind(attr88[whites88[1:34],], 
                  attr106[blacks106[1:55],], 
                  others88)
attr106_12 <- rbind(attr88[whites88[35:length(whites88)],], 
                   attr106[blacks106[56:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_13 <- rbind(attr88[whites88[1:29],], 
                  attr106[blacks106[1:60],], 
                  others88)
attr106_13 <- rbind(attr88[whites88[30:length(whites88)],], 
                   attr106[blacks106[61:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_14 <- rbind(attr88[whites88[1:24],], 
                  attr106[blacks106[1:65],], 
                  others88)
attr106_14 <- rbind(attr88[whites88[25:length(whites88)],], 
                   attr106[blacks106[66:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_15 <- rbind(attr88[whites88[1:19],], 
                  attr106[blacks106[1:70],], 
                  others88)
attr106_15 <- rbind(attr88[whites88[20:length(whites88)],], 
                   attr106[blacks106[71:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])
attr88_16 <- rbind(attr88[whites88[1:14],], 
                  attr106[blacks106[1:75],], 
                  others88)
attr106_16 <- rbind(attr88[whites88[15:length(whites88)],], 
                   attr106[blacks106[76:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[others106,])


