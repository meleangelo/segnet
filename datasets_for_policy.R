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

net88 <- net

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


net106 <- net

# create dataset with all attributes of network 106
whites106 <- which(get.vertex.attribute(net, "white")==1) # white students ids
blacks106 <- which(get.vertex.attribute(net, "black")==1) 
asians106 <- which(get.vertex.attribute(net, "asian")==1)
hisps106  <- which(get.vertex.attribute(net, "hisp")==1)
others106 <- which(get.vertex.attribute(net, "other")==1)
unknown106 <- which(get.vertex.attribute(net, "race")==0)


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
                  attr88[others88,])
attr106_2 <- rbind(attr88[whites88[85:length(whites88)],], 
                   attr106[blacks106[6:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_3 <- rbind(attr88[whites88[1:79],], 
                  attr106[blacks106[1:10],], 
                  attr88[others88,])
attr106_3 <- rbind(attr88[whites88[80:length(whites88)],], 
                   attr106[blacks106[11:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_4 <- rbind(attr88[whites88[1:74],], 
                  attr106[blacks106[1:15],], 
                  attr88[others88,])
attr106_4 <- rbind(attr88[whites88[75:length(whites88)],], 
                   attr106[blacks106[16:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_5 <- rbind(attr88[whites88[1:69],], 
                  attr106[blacks106[1:20],], 
                  attr88[others88,])
attr106_5 <- rbind(attr88[whites88[70:length(whites88)],], 
                   attr106[blacks106[21:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_6 <- rbind(attr88[whites88[1:64],], 
                  attr106[blacks106[1:25],], 
                  attr88[others88,])
attr106_6 <- rbind(attr88[whites88[65:length(whites88)],], 
                   attr106[blacks106[26:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_7 <- rbind(attr88[whites88[1:59],], 
                  attr106[blacks106[1:30],], 
                  attr88[others88,])
attr106_7 <- rbind(attr88[whites88[60:length(whites88)],], 
                   attr106[blacks106[31:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_8 <- rbind(attr88[whites88[1:54],], 
                  attr106[blacks106[1:35],], 
                  attr88[others88,])
attr106_8 <- rbind(attr88[whites88[55:length(whites88)],], 
                   attr106[blacks106[36:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_9 <- rbind(attr88[whites88[1:49],], 
                  attr106[blacks106[1:40],], 
                  attr88[others88,])
attr106_9 <- rbind(attr88[whites88[50:length(whites88)],], 
                   attr106[blacks106[41:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_10 <- rbind(attr88[whites88[1:44],], 
                  attr106[blacks106[1:45],], 
                  attr88[others88,])
attr106_10 <- rbind(attr88[whites88[45:length(whites88)],], 
                   attr106[blacks106[46:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_11 <- rbind(attr88[whites88[1:39],], 
                  attr106[blacks106[1:50],], 
                  attr88[others88,])
attr106_11 <- rbind(attr88[whites88[40:length(whites88)],], 
                   attr106[blacks106[51:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_12 <- rbind(attr88[whites88[1:34],], 
                  attr106[blacks106[1:55],], 
                  attr88[others88,])
attr106_12 <- rbind(attr88[whites88[35:length(whites88)],], 
                   attr106[blacks106[56:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_13 <- rbind(attr88[whites88[1:29],], 
                  attr106[blacks106[1:60],], 
                  attr88[others88,])
attr106_13 <- rbind(attr88[whites88[30:length(whites88)],], 
                   attr106[blacks106[61:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_14 <- rbind(attr88[whites88[1:24],], 
                  attr106[blacks106[1:65],], 
                  attr88[others88,])
attr106_14 <- rbind(attr88[whites88[25:length(whites88)],], 
                   attr106[blacks106[66:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_15 <- rbind(attr88[whites88[1:19],], 
                  attr106[blacks106[1:70],], 
                  attr88[others88,])
attr106_15 <- rbind(attr88[whites88[20:length(whites88)],], 
                   attr106[blacks106[71:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])
attr88_16 <- rbind(attr88[whites88[1:14],], 
                  attr106[blacks106[1:75],], 
                  attr88[others88,])
attr106_16 <- rbind(attr88[whites88[15:length(whites88)],], 
                   attr106[blacks106[76:length(blacks106)],], 
                   attr106[hisps106,], 
                   attr106[unknown106,])




dflist106 <- list(attr106_1,attr106_2, attr106_3,attr106_4,attr106_5,attr106_6,attr106_7,
                  attr106_8,attr106_9,attr106_10,attr106_11,attr106_12,attr106_13,
                  attr106_14,attr106_15,attr106_16)
dflist88 <- list(attr88_1,attr88_2, attr88_3,attr88_4,attr88_5,attr88_6,attr88_7,
                  attr88_8,attr88_9,attr88_10,attr88_11,attr88_12,attr88_13,
                  attr88_14,attr88_15,attr88_16)
policy_num <- 1
for (i in dflist106) {
  n <- dim(i)[1]
 net <- network(net106, vertex.attr = i, vertex.attrnames = names(i))
 # generate variable with race fractions
 i$fwhite <- sum(i$race==1)/n
 i$fblack <- sum(i$race==2)/n
 i$fasian <- sum(i$race==3)/n
 i$fhisp <- sum(i$race==4)/n
 i$fother <- sum(i$race==5)/n
 
 ww_fwhite <- (1*(i$race==1) %*% t(1*(i$race==1)) )*i$fwhite[1]
 bb_fblack <- (1*(i$race==2) %*% t(1*(i$race==2)) )*i$fblack[1]
 aa_fasian <- (1*(i$race==3) %*% t(1*(i$race==3)) )*i$fasian[1]
 hh_fhisp <- (1*(i$race==4) %*% t(1*(i$race==4)) )*i$fhisp[1]
 oo_fother <- (1*(i$race==5) %*% t(1*(i$race==5)) )*i$fother[1]
 
 set.vertex.attribute(net, "fwhite", i$fwhite)
 set.vertex.attribute(net, "fblack", i$fblack)
 set.vertex.attribute(net, "fasian", i$fasian)
 set.vertex.attribute(net, "fhisp", i$fhisp)
 set.vertex.attribute(net, "fother", i$fother)

 set.edge.value(net, "ww_fwhite", ww_fwhite)
 set.edge.value(net, "bb_fblack", bb_fblack)
 set.edge.value(net, "aa_fasian", aa_fasian)
 set.edge.value(net, "hh_fhisp", hh_fhisp)
 set.edge.value(net, "oo_fother", oo_fother)
 
urls <- paste("E:/backup_phoenix/addhealth/schools/saturated/netpolicy106_",policy_num,".Rdata",sep="")

 save(net, file = urls)
 policy_num <- policy_num+1
}


policy_num <- 1
for (i in dflist88) {
  n <- dim(i)[1]
  net <- network(net88, vertex.attr = i, vertex.attrnames = names(i))
  # generate variable with race fractions
  i$fwhite <- sum(i$race==1)/n
  i$fblack <- sum(i$race==2)/n
  i$fasian <- sum(i$race==3)/n
  i$fhisp <- sum(i$race==4)/n
  i$fother <- sum(i$race==5)/n
  
  ww_fwhite <- (1*(i$race==1) %*% t(1*(i$race==1)) )*i$fwhite[1]
  bb_fblack <- (1*(i$race==2) %*% t(1*(i$race==2)) )*i$fblack[1]
  aa_fasian <- (1*(i$race==3) %*% t(1*(i$race==3)) )*i$fasian[1]
  hh_fhisp <- (1*(i$race==4) %*% t(1*(i$race==4)) )*i$fhisp[1]
  oo_fother <- (1*(i$race==5) %*% t(1*(i$race==5)) )*i$fother[1]
  
  set.vertex.attribute(net, "fwhite", i$fwhite)
  set.vertex.attribute(net, "fblack", i$fblack)
  set.vertex.attribute(net, "fasian", i$fasian)
  set.vertex.attribute(net, "fhisp", i$fhisp)
  set.vertex.attribute(net, "fother", i$fother)
  
  set.edge.value(net, "ww_fwhite", ww_fwhite)
  set.edge.value(net, "bb_fblack", bb_fblack)
  set.edge.value(net, "aa_fasian", aa_fasian)
  set.edge.value(net, "hh_fhisp", hh_fhisp)
  set.edge.value(net, "oo_fother", oo_fother)
  
  urls <- paste("E:/backup_phoenix/addhealth/schools/saturated/netpolicy88_",policy_num,".Rdata",sep="")
  
  save(net, file = urls)
  policy_num <- policy_num+1
}







#########################################################
#######################################
############################
###################
###########
#####
###
###
##
#
##
#







##################################
# SCHOOL  88 and 106: swap students of different income levels
###################################


# use median to separate rich and poor kids

rm(list=ls())
library(statnet)

satschool <- 88 
urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/net",satschool,".Rdata",sep="")
load(urls)

net88 <- net

# create dataset with all attributes of network 88
whites88 <- which(get.vertex.attribute(net, "white")==1) # white students ids
blacks88 <- which(get.vertex.attribute(net, "black")==1) 
asians88 <- which(get.vertex.attribute(net, "asian")==1)
hisps88  <- which(get.vertex.attribute(net, "hisp")==1)
others88 <- which(get.vertex.attribute(net, "other")==1)

loginc88 <- get.vertex.attribute(net, "loginc")
medinc88 <- median(loginc88)
rich88 <- which(loginc88 > medinc88)
nonrich88 <- which(loginc88 <= medinc88)


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


net106 <- net

# create dataset with all attributes of network 106
whites106 <- which(get.vertex.attribute(net, "white")==1) # white students ids
blacks106 <- which(get.vertex.attribute(net, "black")==1) 
asians106 <- which(get.vertex.attribute(net, "asian")==1)
hisps106  <- which(get.vertex.attribute(net, "hisp")==1)
others106 <- which(get.vertex.attribute(net, "other")==1)
unknown106 <- which(get.vertex.attribute(net, "race")==0)

loginc106 <- get.vertex.attribute(net, "loginc")
medinc106 <- median(loginc106)
rich106 <- which(loginc106 > medinc106)
nonrich106 <- which(loginc106 <= medinc106)


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

attr88_2 <- rbind(attr88[rich88[1:40],], 
                  attr106[nonrich106[1:5],],
                  attr88[nonrich88,])
attr106_2 <- rbind(attr88[rich88[41:45],], 
                   attr106[rich106,], 
                   attr106[nonrich106[6:41],])

attr88_3 <- rbind(attr88[rich88[1:35],], 
                  attr106[nonrich106[1:10],],
                  attr88[nonrich88,])
attr106_3 <- rbind(attr88[rich88[36:45],], 
                   attr106[rich106,], 
                   attr106[nonrich106[11:41],])

attr88_4 <- rbind(attr88[rich88[1:30],], 
                  attr106[nonrich106[1:15],],
                  attr88[nonrich88,])
attr106_4 <- rbind(attr88[rich88[31:45],], 
                   attr106[rich106,], 
                   attr106[nonrich106[16:41],])

attr88_5 <- rbind(attr88[rich88[1:25],], 
                  attr106[nonrich106[1:20],],
                  attr88[nonrich88,])
attr106_5 <- rbind(attr88[rich88[26:45],], 
                   attr106[rich106,], 
                   attr106[nonrich106[21:41],])

attr88_6 <- rbind(attr88[rich88[1:20],], 
                  attr106[nonrich106[1:25],],
                  attr88[nonrich88,])
attr106_6 <- rbind(attr88[rich88[21:45],], 
                   attr106[rich106,], 
                   attr106[nonrich106[26:41],])

attr88_7 <- rbind(attr88[rich88[1:15],], 
                  attr106[nonrich106[1:30],],
                  attr88[nonrich88,])
attr106_7 <- rbind(attr88[rich88[16:45],], 
                   attr106[rich106,], 
                   attr106[nonrich106[31:41],])

attr88_8 <- rbind(attr88[rich88[1:10],], 
                  attr106[nonrich106[1:35],],
                  attr88[nonrich88,])
attr106_8 <- rbind(attr88[rich88[11:45],], 
                   attr106[rich106,], 
                   attr106[nonrich106[36:41],])

dflist106 <- list(attr106_1,attr106_2, attr106_3,attr106_4,attr106_5,attr106_6,attr106_7,
                  attr106_8)
dflist88 <- list(attr88_1,attr88_2, attr88_3,attr88_4,attr88_5,attr88_6,attr88_7,
                 attr88_8)
policy_num <- 1
for (i in dflist106) {
  n <- dim(i)[1]
  net <- network(net106, vertex.attr = i, vertex.attrnames = names(i))
  # generate variable with race fractions
  i$fwhite <- sum(i$race==1)/n
  i$fblack <- sum(i$race==2)/n
  i$fasian <- sum(i$race==3)/n
  i$fhisp <- sum(i$race==4)/n
  i$fother <- sum(i$race==5)/n
  
  ww_fwhite <- (1*(i$race==1) %*% t(1*(i$race==1)) )*i$fwhite[1]
  bb_fblack <- (1*(i$race==2) %*% t(1*(i$race==2)) )*i$fblack[1]
  aa_fasian <- (1*(i$race==3) %*% t(1*(i$race==3)) )*i$fasian[1]
  hh_fhisp <- (1*(i$race==4) %*% t(1*(i$race==4)) )*i$fhisp[1]
  oo_fother <- (1*(i$race==5) %*% t(1*(i$race==5)) )*i$fother[1]
  
  set.vertex.attribute(net, "fwhite", i$fwhite)
  set.vertex.attribute(net, "fblack", i$fblack)
  set.vertex.attribute(net, "fasian", i$fasian)
  set.vertex.attribute(net, "fhisp", i$fhisp)
  set.vertex.attribute(net, "fother", i$fother)
  
  set.edge.value(net, "ww_fwhite", ww_fwhite)
  set.edge.value(net, "bb_fblack", bb_fblack)
  set.edge.value(net, "aa_fasian", aa_fasian)
  set.edge.value(net, "hh_fhisp", hh_fhisp)
  set.edge.value(net, "oo_fother", oo_fother)
  
  urls <- paste("E:/backup_phoenix/addhealth/schools/saturated/netpolicy_rich106_",policy_num,".Rdata",sep="")
  
  save(net, file = urls)
  policy_num <- policy_num+1
}


policy_num <- 1
for (i in dflist88) {
  n <- dim(i)[1]
  net <- network(net88, vertex.attr = i, vertex.attrnames = names(i))
  # generate variable with race fractions
  i$fwhite <- sum(i$race==1)/n
  i$fblack <- sum(i$race==2)/n
  i$fasian <- sum(i$race==3)/n
  i$fhisp <- sum(i$race==4)/n
  i$fother <- sum(i$race==5)/n
  
  ww_fwhite <- (1*(i$race==1) %*% t(1*(i$race==1)) )*i$fwhite[1]
  bb_fblack <- (1*(i$race==2) %*% t(1*(i$race==2)) )*i$fblack[1]
  aa_fasian <- (1*(i$race==3) %*% t(1*(i$race==3)) )*i$fasian[1]
  hh_fhisp <- (1*(i$race==4) %*% t(1*(i$race==4)) )*i$fhisp[1]
  oo_fother <- (1*(i$race==5) %*% t(1*(i$race==5)) )*i$fother[1]
  
  set.vertex.attribute(net, "fwhite", i$fwhite)
  set.vertex.attribute(net, "fblack", i$fblack)
  set.vertex.attribute(net, "fasian", i$fasian)
  set.vertex.attribute(net, "fhisp", i$fhisp)
  set.vertex.attribute(net, "fother", i$fother)
  
  set.edge.value(net, "ww_fwhite", ww_fwhite)
  set.edge.value(net, "bb_fblack", bb_fblack)
  set.edge.value(net, "aa_fasian", aa_fasian)
  set.edge.value(net, "hh_fhisp", hh_fhisp)
  set.edge.value(net, "oo_fother", oo_fother)
  
  urls <- paste("E:/backup_phoenix/addhealth/schools/saturated/netpolicy_rich88_",policy_num,".Rdata",sep="")
  
  save(net, file = urls)
  policy_num <- policy_num+1
}


