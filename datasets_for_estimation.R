# this code loads the datasets and creates additional variables that will
# be used in estimation

rm(list=ls())


#for (ss in 1:16) {
for (ss in 1:16) {
  # list of saturated schools  
  saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
  
  # file with node attributes
  urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/x",saturated[ss],".txt",sep="")
  nodeinfo<-read.table(urls)
  names(nodeinfo)<-c("race","gender","grade", "loginc", 
                     "phys_attr", "pers_attr", 
                     "sch1","sch2", "sch3", "sch4", "sch5", "sch6", "sch7", 
                     "sch8", "sch9", "sch10", "sch11", "sch12", 
                     "sch13", "sch14", "sch15", "sch16")
  # file with edgelist
  urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/g",saturated[ss],".txt",sep="")
  edgelist<-read.table(urls)
  
  # size of network (number of nodes)
  n <- dim(nodeinfo)[1]
  # number of links
  totlinks <- dim(edgelist)[1]
  # create adjacency matrix empty
  adj <- matrix(0, nrow =n, ncol = n)
  # put 1s in adjacency matrix
  for (l in 1:totlinks){
    adj[edgelist[l,1],edgelist[l,2]] <- 1
  }
  
  # generate variable with race of individual nodes dummies
  nodeinfo$white <- 1*(nodeinfo$race==1)
  nodeinfo$black <- 1*(nodeinfo$race==2)
  nodeinfo$asian <- 1*(nodeinfo$race==3)
  nodeinfo$hisp <- 1*(nodeinfo$race==4)
  nodeinfo$other <- 1*(nodeinfo$race==5)

  # generate variable with race fractions
  nodeinfo$fwhite <- sum(nodeinfo$race==1)/n
  nodeinfo$fblack <- sum(nodeinfo$race==2)/n
  nodeinfo$fasian <- sum(nodeinfo$race==3)/n
  nodeinfo$fhisp <- sum(nodeinfo$race==4)/n
  nodeinfo$fother <- sum(nodeinfo$race==5)/n
  
  ww_fwhite <- (1*(nodeinfo$race==1) %*% t(1*(nodeinfo$race==1)) )*nodeinfo$fwhite[1]
  bb_fblack <- (1*(nodeinfo$race==2) %*% t(1*(nodeinfo$race==2)) )*nodeinfo$fblack[1]
  aa_fasian <- (1*(nodeinfo$race==3) %*% t(1*(nodeinfo$race==3)) )*nodeinfo$fasian[1]
  hh_fhisp <- (1*(nodeinfo$race==4) %*% t(1*(nodeinfo$race==4)) )*nodeinfo$fhisp[1]
  oo_fother <- (1*(nodeinfo$race==5) %*% t(1*(nodeinfo$race==5)) )*nodeinfo$fother[1]
  
  net <- network(adj, vertex.attr = nodeinfo, directed = TRUE )
  set.edge.value(net, "ww_fwhite", ww_fwhite)
  set.edge.value(net, "bb_fblack", bb_fblack)
  set.edge.value(net, "aa_fasian", aa_fasian)
  set.edge.value(net, "hh_fhisp", hh_fhisp)
  set.edge.value(net, "oo_fother", oo_fother)
  
  

  
  urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/net",saturated[ss],".Rdata",sep="")
  rm(list=setdiff(ls(), list("net", "urls")))
  save.image(file = urls)
}
