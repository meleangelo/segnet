### SCript for policy simulation summaries

rm(list = ls())
detach("package:isnar", unload=TRUE)
detach("package:igraph", unload=TRUE)

# data from network school 106
urls <- "D:/segnet/results/policy/netpolicy106_1.RData"
load(urls)
library(network)

# income cutoffs to generate rich and poor indicators
loginc <- get.vertex.attribute(net, "loginc")
loginc106_50 <- median(loginc)
loginc106_90 <- quantile(loginc, 0.9)
loginc106_10 <- median(loginc, 0.1)

# data from network school 88
urls <- "D:/segnet/results/policy/netpolicy88_1.RData"
load(urls)
library(network)

# income cutoffs to generate rich and poor indicators
loginc <- get.vertex.attribute(net, "loginc")
loginc88_50 <- median(loginc)
loginc88_90 <- quantile(loginc, 0.9)
loginc88_10 <- median(loginc, 0.1)

tables <- array(NA, c(16,1000,36))
  
  
for (policy in 2:16){ #16){
  print(paste("policy = ", policy))
  # policy data
  urls <- paste("D:/segnet/results/policy/netpolicy106_",policy, ".RData", sep = "")
  load(urls)
  library(network)
  # get attributes that are relevant
  gender <- get.vertex.attribute(net, "gender")
  race <- get.vertex.attribute(net, "race")
  loginc <- get.vertex.attribute(net, "loginc")
  
  # for each network compute the indices
  for (network in 1:1000){ #1000) {
    print(paste("network = ", network))
    
    if("isnar" %in% (.packages())){
      detach("package:isnar", unload=TRUE) 
    }
    if("igraph" %in% (.packages())){
      detach("package:igraph", unload=TRUE) 
    }
    library(network)
    # create adj matrix for igraph
    gg <- as.sociomatrix(g[[network]])
    
    # libraries
#    detach("package:statnet", unload=TRUE) 
#    detach("package:sna", unload=TRUE) 
#    detach("package:ergm.userterms", unload=TRUE) 
#    detach("package:tsna", unload=TRUE) 
#    detach("package:ergm.count", unload = TRUE)
#    detach("package:tergm", unload = TRUE)
#    detach("package:ergm", unload = TRUE)
#    detach("package:networkDynamic", unload = TRUE)
    if("network" %in% (.packages())){
      detach("package:network", unload=TRUE) 
    }
    #detach("package:network", unload=TRUE) 
    
    library(isnar)
    library(igraph)
    
    # generate igraph object
    ggg <- graph.adjacency(gg, mode = "directed")
    ggu <- graph.adjacency(gg, mode = "undirected")
    
    # attach attributes to igraph object
    V(ggg)$race <- race
    V(ggg)$gender <- gender
    V(ggg)$loginc <- loginc
    V(ggg)$whites <- 1*(race == 1)
    V(ggg)$blacks <- 1*(race == 2)
    V(ggg)$hisps <- 1*(race == 4)
    
    V(ggg)$rich <- 1*(loginc >= loginc106_90)
    V(ggg)$rich2 <- 1*(loginc >= loginc106_50)
    V(ggg)$poor <- 1*(loginc <= loginc106_10)
    
    V(ggu)$race <- race
    V(ggu)$gender <- gender
    V(ggu)$loginc <- loginc
    V(ggu)$whites <- 1*(race == 1)
    V(ggu)$blacks <- 1*(race == 2)
    V(ggu)$hisps <- 1*(race == 4)
    
    V(ggu)$rich <- 1*(loginc >= loginc106_90)
    V(ggu)$rich2 <- 1*(loginc >= loginc106_50)
    V(ggu)$poor <- 1*(loginc <= loginc106_10)
    
    # compute freeman seg
    tables[policy, network,1] <- freeman(ggu, "gender")
    tables[policy, network,2] <- freeman(ggu, "race")
    tables[policy, network,3] <- freeman(ggu, "whites")
    tables[policy, network,4] <- freeman(ggu, "blacks")
    tables[policy, network,5] <- freeman(ggu, "hisps")
    tables[policy, network,6] <- freeman(ggu, "rich")
    tables[policy, network,7] <- freeman(ggu, "rich2")
    tables[policy, network,8] <- freeman(ggu, "poor")
    
    # compute spectral segregation index
    tables[policy, network,9] <- mean(ssi(ggu, "gender"))
    tables[policy, network,10] <- mean(ssi(ggu, "race"))
    tables[policy, network,11] <- mean(ssi(ggu, "whites"))
    tables[policy, network,12] <- mean(ssi(ggu, "blacks"))
    tables[policy, network,13] <- mean(ssi(ggu, "hisps"))
    tables[policy, network,14] <- mean(ssi(ggu, "rich"))
    tables[policy, network,15] <- mean(ssi(ggu, "rich2"))
    tables[policy, network,16] <- mean(ssi(ggu, "poor"))
    
    
    # compute coleman homophily
    tables[policy, network,17] <- coleman(ggg, "gender")[1] #male
    tables[policy, network,18] <- coleman(ggg, "gender")[2] #female
    tables[policy, network,19] <- coleman(ggg, "whites")[2]
    tables[policy, network,20] <- coleman(ggg, "blacks")[2]
    tables[policy, network,21] <- coleman(ggg, "hisps")[2]
    tables[policy, network,22] <- coleman(ggg, "rich")[2]
    tables[policy, network,23] <- coleman(ggg, "rich2")[2]
    tables[policy, network,24] <- coleman(ggg, "poor")[2]
    
    # clustering
    tables[policy, network,25] <- transitivity(ggg)
    tables[policy, network,26] <- mean(transitivity(ggg), na.rm=T)
    
    # density
    tables[policy, network,27] <- ecount(ggg)/(vcount(ggg)*(vcount(ggg)-1))
    
    # most central players, in-degree
    deg <- degree(ggg, mode = "in")
    nn <- which(deg == max(deg))
    tables[policy, network,28] <- gender[nn[1]] # gender of most central 
    tables[policy, network,29] <- race[nn[1]] # race of most central 
    tables[policy, network,30] <- loginc[nn[1]] # log income of most central 
    # most central eigenvector
    ev <- evcent(ggg)$vector
    nn <- which(ev == max(ev))
    tables[policy, network,31] <- gender[nn[1]] # gender of most central 
    tables[policy, network,32] <- race[nn[1]] # race of most central 
    tables[policy, network,33] <- loginc[nn[1]] # log income of most central 
    # most central players, out-degree
    deg <- degree(ggg, mode = "out")
    nn <- which(deg == max(deg))
    tables[policy, network,34] <- gender[nn[1]] # gender of most central 
    tables[policy, network,35] <- race[nn[1]] # race of most central 
    tables[policy, network,36] <- loginc[nn[1]] # log income of most central 
    
    
    
    
    
    #table(race)/dim(gg)[1]
    #pr <- page.rank(ggg)$vector
    #nn <-which(pr == max(pr))
    #pr[nn]
    #ggg
    
    #race[nn]
    #gender[nn]
 }
}

tables106 <- tables

tables <- array(NA, c(16,1000,36))

for (policy in 2:16){ #16){
  print(paste("policy = ", policy))
  # policy data
  urls <- paste("D:/segnet/results/policy/netpolicy88_",policy, ".RData", sep = "")
  load(urls)
  library(network)
  # get attributes that are relevant
  gender <- get.vertex.attribute(net, "gender")
  race <- get.vertex.attribute(net, "race")
  loginc <- get.vertex.attribute(net, "loginc")
  
  # for each network compute the indices
  for (network in 1:1000){ #1000) {
    print(paste("network = ", network))
    
    if("isnar" %in% (.packages())){
      detach("package:isnar", unload=TRUE) 
    }
    if("igraph" %in% (.packages())){
      detach("package:igraph", unload=TRUE) 
    }
    library(network)
    # create adj matrix for igraph
    gg <- as.sociomatrix(g[[network]])
    
    # libraries
    #    detach("package:statnet", unload=TRUE) 
    #    detach("package:sna", unload=TRUE) 
    #    detach("package:ergm.userterms", unload=TRUE) 
    #    detach("package:tsna", unload=TRUE) 
    #    detach("package:ergm.count", unload = TRUE)
    #    detach("package:tergm", unload = TRUE)
    #    detach("package:ergm", unload = TRUE)
    #    detach("package:networkDynamic", unload = TRUE)
    if("network" %in% (.packages())){
      detach("package:network", unload=TRUE) 
    }
    #detach("package:network", unload=TRUE) 
    
    library(isnar)
    library(igraph)
    
    # generate igraph object
    ggg <- graph.adjacency(gg, mode = "directed")
    ggu <- graph.adjacency(gg, mode = "undirected")
    
    # attach attributes to igraph object
    V(ggg)$race <- race
    V(ggg)$gender <- gender
    V(ggg)$loginc <- loginc
    V(ggg)$whites <- 1*(race == 1)
    V(ggg)$blacks <- 1*(race == 2)
    V(ggg)$hisps <- 1*(race == 4)
    
    V(ggg)$rich <- 1*(loginc >= loginc88_90)
    V(ggg)$rich2 <- 1*(loginc >= loginc88_50)
    V(ggg)$poor <- 1*(loginc <= loginc88_10)
    
    V(ggu)$race <- race
    V(ggu)$gender <- gender
    V(ggu)$loginc <- loginc
    V(ggu)$whites <- 1*(race == 1)
    V(ggu)$blacks <- 1*(race == 2)
    V(ggu)$hisps <- 1*(race == 4)
    
    V(ggu)$rich <- 1*(loginc >= loginc88_90)
    V(ggu)$rich2 <- 1*(loginc >= loginc88_50)
    V(ggu)$poor <- 1*(loginc <= loginc88_10)
    
    # compute freeman seg
    tables[policy, network,1] <- freeman(ggu, "gender")
    tables[policy, network,2] <- freeman(ggu, "race")
    tables[policy, network,3] <- freeman(ggu, "whites")
    tables[policy, network,4] <- freeman(ggu, "blacks")
    #tables[policy, network,5] <- freeman(ggu, "hisps")
    tables[policy, network,6] <- freeman(ggu, "rich")
    tables[policy, network,7] <- freeman(ggu, "rich2")
    tables[policy, network,8] <- freeman(ggu, "poor")
    
    # compute spectral segregation index
    tables[policy, network,9] <- mean(ssi(ggu, "gender"))
    tables[policy, network,10] <- mean(ssi(ggu, "race"))
    tables[policy, network,11] <- mean(ssi(ggu, "whites"))
    tables[policy, network,12] <- mean(ssi(ggu, "blacks"))
    #tables[policy, network,13] <- mean(ssi(ggu, "hisps"))
    tables[policy, network,14] <- mean(ssi(ggu, "rich"))
    tables[policy, network,15] <- mean(ssi(ggu, "rich2"))
    tables[policy, network,16] <- mean(ssi(ggu, "poor"))
    
    
    # compute coleman homophily
    tables[policy, network,17] <- coleman(ggg, "gender")[1] #male
    tables[policy, network,18] <- coleman(ggg, "gender")[2] #female
    tables[policy, network,19] <- coleman(ggg, "whites")[2]
    tables[policy, network,20] <- coleman(ggg, "blacks")[2]
    #tables[policy, network,21] <- coleman(ggg, "hisps")[2]
    tables[policy, network,22] <- coleman(ggg, "rich")[2]
    tables[policy, network,23] <- coleman(ggg, "rich2")[2]
    tables[policy, network,24] <- coleman(ggg, "poor")[2]
    
    # clustering
    tables[policy, network,25] <- transitivity(ggg)
    tables[policy, network,26] <- mean(transitivity(ggg), na.rm=T)
    
    # density
    tables[policy, network,27] <- ecount(ggg)/(vcount(ggg)*(vcount(ggg)-1))
    
    # most central players, in-degree
    deg <- degree(ggg, mode = "in")
    nn <- which(deg == max(deg))
    tables[policy, network,28] <- gender[nn[1]] # gender of most central 
    tables[policy, network,29] <- race[nn[1]] # race of most central 
    tables[policy, network,30] <- loginc[nn[1]] # log income of most central 
    # most central eigenvector
    ev <- evcent(ggg)$vector
    nn <- which(ev == max(ev))
    tables[policy, network,31] <- gender[nn[1]] # gender of most central 
    tables[policy, network,32] <- race[nn[1]] # race of most central 
    tables[policy, network,33] <- loginc[nn[1]] # log income of most central 
    # most central players, out-degree
    deg <- degree(ggg, mode = "out")
    nn <- which(deg == max(deg))
    tables[policy, network,34] <- gender[nn[1]] # gender of most central 
    tables[policy, network,35] <- race[nn[1]] # race of most central 
    tables[policy, network,36] <- loginc[nn[1]] # log income of most central 
    
    
    
    
    
    #table(race)/dim(gg)[1]
    #pr <- page.rank(ggg)$vector
    #nn <-which(pr == max(pr))
    #pr[nn]
    #ggg
    
    #race[nn]
    #gender[nn]
  }
}
tables88 <- tables

urls <- "D:/segnet/results/policy/policy_raceswap_88_106.RData"
save.image(file = urls)


# create plot for school 88 and 106 to compare
x88 <- c()
x106 <- c()
for (policy in 2:16) {
  print(policy)
  urls <- paste("D:/segnet/results/policy/netpolicy88_",policy, ".RData", sep = "")
  load(urls)
  library(network)
  # get attributes that are relevant
  gender <- get.vertex.attribute(net, "gender")
  race <- get.vertex.attribute(net, "race")
  loginc <- get.vertex.attribute(net, "loginc")
  
  x88 <- c(x88, sum(race==2)/length(race))
  
  urls <- paste("D:/segnet/results/policy/netpolicy106_",policy, ".RData", sep = "")
  load(urls)
  library(network)
  # get attributes that are relevant
  gender <- get.vertex.attribute(net, "gender")
  race <- get.vertex.attribute(net, "race")
  loginc <- get.vertex.attribute(net, "loginc")

  x106 <- c(x106, sum(race==2)/length(race))
  
}


# plot of gender segregation (freeman)
x <- x88
my <- c()
y <- c()
y025 <- c()
y975 <- c()
y05 <- c()
y95 <- c()
vbl <- 1
for (policy in 2:16) {
  y <- c(y, mean(tables88[policy, ,vbl], na.rm = T))
  my <- c(y, median(tables88[policy, ,vbl], na.rm = T))
  y025 <- c(y025, quantile(tables88[policy, ,vbl], probs = 0.025, na.rm = T))
  y975 <- c(y975, quantile(tables88[policy, ,vbl], probs = 0.975, na.rm = T))
  y05 <- c(y05, quantile(tables88[policy, ,vbl], probs = 0.05, na.rm = T))
  y95 <- c(y95, quantile(tables88[policy, ,vbl], probs = 0.95, na.rm = T))
#  x <- c(x, policy)
}
plot(x,y, type = "b", col = "red", lwd = 2, ylim= c(0,1))
lines(y025, lwd = 2, lty = 2)
lines(y975, lwd = 2, lty = 2)
lines(y05, lwd = 2, lty = 2, col = "blue")
lines(y95, lwd = 2, lty = 2, col = "blue")




x <- x106
my <- c()
y <- c()
y025 <- c()
y975 <- c()
y05 <- c()
y95 <- c()
vbl <- 1
for (policy in 2:16) {
  y <- c(y, mean(tables106[policy, ,vbl], na.rm = T))
  my <- c(y, median(tables106[policy, ,vbl], na.rm = T))
  y025 <- c(y025, quantile(tables106[policy, ,vbl], probs = 0.025, na.rm = T))
  y975 <- c(y975, quantile(tables106[policy, ,vbl], probs = 0.975, na.rm = T))
  y05 <- c(y05, quantile(tables106[policy, ,vbl], probs = 0.05, na.rm = T))
  y95 <- c(y95, quantile(tables106[policy, ,vbl], probs = 0.95, na.rm = T))
#  x <- c(x, policy)
}
plot(x,y, type = "b", col = "red", lwd = 2, ylim= c(0,1))
lines(y025, lwd = 2, lty = 2)
lines(y975, lwd = 2, lty = 2)
lines(y05, lwd = 2, lty = 2, col = "blue")
lines(y95, lwd = 2, lty = 2, col = "blue")





# plot of mean gender segregation (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables88[policy, ,1], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables88[policy, ,9], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables88[policy, ,17], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average gender segregation in school 88",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables106[policy, ,1], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables106[policy, ,9], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables106[policy, ,17], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average gender segregation in school 106",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")



# plot of mean income segregation (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables88[policy, ,6], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables88[policy, ,14], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables88[policy, ,22], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average income segregation in school 88",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables106[policy, ,6], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables106[policy, ,14], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables106[policy, ,22], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average income segregation in school 106",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")






# plot of mean income segregation (median) (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables88[policy, ,7], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables88[policy, ,15], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables88[policy, ,23], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average income segregation in school 88",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables106[policy, ,7], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables106[policy, ,15], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables106[policy, ,23], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average income segregation in school 106",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")






# plot of mean racial segregation, all races (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables88[policy, ,2], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables88[policy, ,10], na.rm = T))
  #mean_coleman <- c(mean_coleman, mean(tables88[policy, ,23], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average racial segregation in school 88",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
#lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables106[policy, ,2], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables106[policy, ,10], na.rm = T))
  #mean_coleman <- c(mean_coleman, mean(tables106[policy, ,23], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average racial segregation in school 106",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
#lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")



# plot of mean racial segregation, black (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables88[policy, ,4], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables88[policy, ,12], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables88[policy, ,20], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average black segregation in school 88",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
#lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables106[policy, ,4], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables106[policy, ,12], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables106[policy, ,20], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average black segregation in school 106",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
#lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")



# plot of mean racial segregation, whites (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables88[policy, ,3], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables88[policy, ,11], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables88[policy, ,19], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average white segregation in school 88",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
#lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables106[policy, ,3], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables106[policy, ,11], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables106[policy, ,19], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average white segregation in school 106",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
#lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")




# plot of mean racial segregation, hispanics (all)
# only school 106

x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:16) {
  mean_freeman <- c(mean_freeman, mean(tables106[policy, ,5], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables106[policy, ,13], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables106[policy, ,21], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average hispanic segregation in school 106",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
legend("topleft", c("freeman", "ssi"), 
       text.col = c("red", "blue"), 
       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))
#lines(x,mean_coleman, lwd = 2, lty = 3, col = "black")






############################################################
#
###############################################################
########################################################
###############################################
########################################
#################################
############################
######################
###################
#################
################
#########
#######



### SCript for policy simulation summaries

rm(list = ls())
detach("package:isnar", unload=TRUE)
detach("package:igraph", unload=TRUE)

# data from network school 106
urls <- "D:/segnet/results/policy/netpolicy_rich106_1.RData"
load(urls)
library(network)

# income cutoffs to generate rich and poor indicators
loginc <- get.vertex.attribute(net, "loginc")
loginc106_50 <- median(loginc)
loginc106_90 <- quantile(loginc, 0.9)
loginc106_10 <- median(loginc, 0.1)

# data from network school 88
urls <- "D:/segnet/results/policy/netpolicy_rich88_1.RData"
load(urls)
library(network)

# income cutoffs to generate rich and poor indicators
loginc <- get.vertex.attribute(net, "loginc")
loginc88_50 <- median(loginc)
loginc88_90 <- quantile(loginc, 0.9)
loginc88_10 <- median(loginc, 0.1)

tables <- array(NA, c(8,1000,36))


for (policy in 2:8){ 
  print(paste("policy = ", policy))
  # policy data
  urls <- paste("D:/segnet/results/policy/netpolicy_rich106_",policy, ".RData", sep = "")
  load(urls)
  library(network)
  # get attributes that are relevant
  gender <- get.vertex.attribute(net, "gender")
  race <- get.vertex.attribute(net, "race")
  loginc <- get.vertex.attribute(net, "loginc")
  
  # for each network compute the indices
  for (network in 1:1000){ #1000) {
    print(paste("network = ", network))
    
    if("isnar" %in% (.packages())){
      detach("package:isnar", unload=TRUE) 
    }
    if("igraph" %in% (.packages())){
      detach("package:igraph", unload=TRUE) 
    }
    library(network)
    # create adj matrix for igraph
    gg <- as.sociomatrix(g[[network]])
    
    # libraries
    #    detach("package:statnet", unload=TRUE) 
    #    detach("package:sna", unload=TRUE) 
    #    detach("package:ergm.userterms", unload=TRUE) 
    #    detach("package:tsna", unload=TRUE) 
    #    detach("package:ergm.count", unload = TRUE)
    #    detach("package:tergm", unload = TRUE)
    #    detach("package:ergm", unload = TRUE)
    #    detach("package:networkDynamic", unload = TRUE)
    if("network" %in% (.packages())){
      detach("package:network", unload=TRUE) 
    }
    #detach("package:network", unload=TRUE) 
    
    library(isnar)
    library(igraph)
    
    # generate igraph object
    ggg <- graph.adjacency(gg, mode = "directed")
    ggu <- graph.adjacency(gg, mode = "undirected")
    
    # attach attributes to igraph object
    V(ggg)$race <- race
    V(ggg)$gender <- gender
    V(ggg)$loginc <- loginc
    V(ggg)$whites <- 1*(race == 1)
    V(ggg)$blacks <- 1*(race == 2)
    V(ggg)$hisps <- 1*(race == 4)
    
    V(ggg)$rich <- 1*(loginc >= loginc106_90)
    V(ggg)$rich2 <- 1*(loginc >= loginc106_50)
    V(ggg)$poor <- 1*(loginc <= loginc106_10)
    
    V(ggu)$race <- race
    V(ggu)$gender <- gender
    V(ggu)$loginc <- loginc
    V(ggu)$whites <- 1*(race == 1)
    V(ggu)$blacks <- 1*(race == 2)
    V(ggu)$hisps <- 1*(race == 4)
    
    V(ggu)$rich <- 1*(loginc >= loginc106_90)
    V(ggu)$rich2 <- 1*(loginc >= loginc106_50)
    V(ggu)$poor <- 1*(loginc <= loginc106_10)
    
    # compute freeman seg
    tables[policy, network,1] <- freeman(ggu, "gender")
    tables[policy, network,2] <- freeman(ggu, "race")
    tables[policy, network,3] <- freeman(ggu, "whites")
    tables[policy, network,4] <- freeman(ggu, "blacks")
    tables[policy, network,5] <- freeman(ggu, "hisps")
    tables[policy, network,6] <- freeman(ggu, "rich")
    tables[policy, network,7] <- freeman(ggu, "rich2")
    tables[policy, network,8] <- freeman(ggu, "poor")
    
    # compute spectral segregation index
    tables[policy, network,9] <- mean(ssi(ggu, "gender"))
    tables[policy, network,10] <- mean(ssi(ggu, "race"))
    tables[policy, network,11] <- mean(ssi(ggu, "whites"))
    tables[policy, network,12] <- mean(ssi(ggu, "blacks"))
    tables[policy, network,13] <- mean(ssi(ggu, "hisps"))
    tables[policy, network,14] <- mean(ssi(ggu, "rich"))
    tables[policy, network,15] <- mean(ssi(ggu, "rich2"))
    tables[policy, network,16] <- mean(ssi(ggu, "poor"))
    
    
    # compute coleman homophily
    tables[policy, network,17] <- coleman(ggg, "gender")[1] #male
    tables[policy, network,18] <- coleman(ggg, "gender")[2] #female
    tables[policy, network,19] <- coleman(ggg, "whites")[2]
    tables[policy, network,20] <- coleman(ggg, "blacks")[2]
    tables[policy, network,21] <- coleman(ggg, "hisps")[2]
    tables[policy, network,22] <- coleman(ggg, "rich")[2]
    tables[policy, network,23] <- coleman(ggg, "rich2")[2]
    tables[policy, network,24] <- coleman(ggg, "poor")[2]
    
    # clustering
    tables[policy, network,25] <- transitivity(ggg)
    tables[policy, network,26] <- mean(transitivity(ggg), na.rm=T)
    
    # density
    tables[policy, network,27] <- ecount(ggg)/(vcount(ggg)*(vcount(ggg)-1))
    
    # most central players, in-degree
    deg <- degree(ggg, mode = "in")
    nn <- which(deg == max(deg))
    tables[policy, network,28] <- gender[nn[1]] # gender of most central 
    tables[policy, network,29] <- race[nn[1]] # race of most central 
    tables[policy, network,30] <- loginc[nn[1]] # log income of most central 
    # most central eigenvector
    ev <- evcent(ggg)$vector
    nn <- which(ev == max(ev))
    tables[policy, network,31] <- gender[nn[1]] # gender of most central 
    tables[policy, network,32] <- race[nn[1]] # race of most central 
    tables[policy, network,33] <- loginc[nn[1]] # log income of most central 
    # most central players, out-degree
    deg <- degree(ggg, mode = "out")
    nn <- which(deg == max(deg))
    tables[policy, network,34] <- gender[nn[1]] # gender of most central 
    tables[policy, network,35] <- race[nn[1]] # race of most central 
    tables[policy, network,36] <- loginc[nn[1]] # log income of most central 
    
    
    
    
    
    #table(race)/dim(gg)[1]
    #pr <- page.rank(ggg)$vector
    #nn <-which(pr == max(pr))
    #pr[nn]
    #ggg
    
    #race[nn]
    #gender[nn]
  }
}

tables_rich106 <- tables

tables <- array(NA, c(8,1000,36))

for (policy in 2:8){
  print(paste("policy = ", policy))
  # policy data
  urls <- paste("D:/segnet/results/policy/netpolicy_rich88_",policy, ".RData", sep = "")
  load(urls)
  library(network)
  # get attributes that are relevant
  gender <- get.vertex.attribute(net, "gender")
  race <- get.vertex.attribute(net, "race")
  loginc <- get.vertex.attribute(net, "loginc")
  
  # for each network compute the indices
  for (network in 1:1000){ #1000) {
    print(paste("network = ", network))
    
    if("isnar" %in% (.packages())){
      detach("package:isnar", unload=TRUE) 
    }
    if("igraph" %in% (.packages())){
      detach("package:igraph", unload=TRUE) 
    }
    library(network)
    # create adj matrix for igraph
    gg <- as.sociomatrix(g[[network]])
    
    # libraries
    #    detach("package:statnet", unload=TRUE) 
    #    detach("package:sna", unload=TRUE) 
    #    detach("package:ergm.userterms", unload=TRUE) 
    #    detach("package:tsna", unload=TRUE) 
    #    detach("package:ergm.count", unload = TRUE)
    #    detach("package:tergm", unload = TRUE)
    #    detach("package:ergm", unload = TRUE)
    #    detach("package:networkDynamic", unload = TRUE)
    if("network" %in% (.packages())){
      detach("package:network", unload=TRUE) 
    }
    #detach("package:network", unload=TRUE) 
    
    library(isnar)
    library(igraph)
    
    # generate igraph object
    ggg <- graph.adjacency(gg, mode = "directed")
    ggu <- graph.adjacency(gg, mode = "undirected")
    
    # attach attributes to igraph object
    V(ggg)$race <- race
    V(ggg)$gender <- gender
    V(ggg)$loginc <- loginc
    V(ggg)$whites <- 1*(race == 1)
    V(ggg)$blacks <- 1*(race == 2)
    V(ggg)$hisps <- 1*(race == 4)
    
    V(ggg)$rich <- 1*(loginc >= loginc88_90)
    V(ggg)$rich2 <- 1*(loginc >= loginc88_50)
    V(ggg)$poor <- 1*(loginc <= loginc88_10)
    
    V(ggu)$race <- race
    V(ggu)$gender <- gender
    V(ggu)$loginc <- loginc
    V(ggu)$whites <- 1*(race == 1)
    V(ggu)$blacks <- 1*(race == 2)
    V(ggu)$hisps <- 1*(race == 4)
    
    V(ggu)$rich <- 1*(loginc >= loginc88_90)
    V(ggu)$rich2 <- 1*(loginc >= loginc88_50)
    V(ggu)$poor <- 1*(loginc <= loginc88_10)
    
    # compute freeman seg
    tables[policy, network,1] <- freeman(ggu, "gender")
    tables[policy, network,2] <- freeman(ggu, "race")
    tables[policy, network,3] <- freeman(ggu, "whites")
    tables[policy, network,4] <- freeman(ggu, "blacks")
    #tables[policy, network,5] <- freeman(ggu, "hisps")
    tables[policy, network,6] <- freeman(ggu, "rich")
    tables[policy, network,7] <- freeman(ggu, "rich2")
    tables[policy, network,8] <- freeman(ggu, "poor")
    
    # compute spectral segregation index
    tables[policy, network,9] <- mean(ssi(ggu, "gender"))
    tables[policy, network,10] <- mean(ssi(ggu, "race"))
    tables[policy, network,11] <- mean(ssi(ggu, "whites"))
    tables[policy, network,12] <- mean(ssi(ggu, "blacks"))
    #tables[policy, network,13] <- mean(ssi(ggu, "hisps"))
    tables[policy, network,14] <- mean(ssi(ggu, "rich"))
    tables[policy, network,15] <- mean(ssi(ggu, "rich2"))
    tables[policy, network,16] <- mean(ssi(ggu, "poor"))
    
    
    # compute coleman homophily
    tables[policy, network,17] <- coleman(ggg, "gender")[1] #male
    tables[policy, network,18] <- coleman(ggg, "gender")[2] #female
    tables[policy, network,19] <- coleman(ggg, "whites")[2]
    tables[policy, network,20] <- coleman(ggg, "blacks")[2]
    #tables[policy, network,21] <- coleman(ggg, "hisps")[2]
    tables[policy, network,22] <- coleman(ggg, "rich")[2]
    tables[policy, network,23] <- coleman(ggg, "rich2")[2]
    tables[policy, network,24] <- coleman(ggg, "poor")[2]
    
    # clustering
    tables[policy, network,25] <- transitivity(ggg)
    tables[policy, network,26] <- mean(transitivity(ggg), na.rm=T)
    
    # density
    tables[policy, network,27] <- ecount(ggg)/(vcount(ggg)*(vcount(ggg)-1))
    
    # most central players, in-degree
    deg <- degree(ggg, mode = "in")
    nn <- which(deg == max(deg))
    tables[policy, network,28] <- gender[nn[1]] # gender of most central 
    tables[policy, network,29] <- race[nn[1]] # race of most central 
    tables[policy, network,30] <- loginc[nn[1]] # log income of most central 
    # most central eigenvector
    ev <- evcent(ggg)$vector
    nn <- which(ev == max(ev))
    tables[policy, network,31] <- gender[nn[1]] # gender of most central 
    tables[policy, network,32] <- race[nn[1]] # race of most central 
    tables[policy, network,33] <- loginc[nn[1]] # log income of most central 
    # most central players, out-degree
    deg <- degree(ggg, mode = "out")
    nn <- which(deg == max(deg))
    tables[policy, network,34] <- gender[nn[1]] # gender of most central 
    tables[policy, network,35] <- race[nn[1]] # race of most central 
    tables[policy, network,36] <- loginc[nn[1]] # log income of most central 
    
    
    
    
    
    #table(race)/dim(gg)[1]
    #pr <- page.rank(ggg)$vector
    #nn <-which(pr == max(pr))
    #pr[nn]
    #ggg
    
    #race[nn]
    #gender[nn]
  }
}
tables_rich88 <- tables

urls <- "D:/segnet/results/policy/policy_richswap_88_106.RData"
save.image(file = urls)




###
# fraction of nonrich in schools
x88 <- c(50,55,60,65,70,75,80)/90
x106 <- c(35,30,25,20,15,10,5)/81



# plot of mean gender segregation (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich88[policy, ,1], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich88[policy, ,9], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich88[policy, ,17], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average gender segregation in school 88",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich106[policy, ,1], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich106[policy, ,9], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich106[policy, ,17], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average gender segregation in school 106",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")







# plot of mean income segregation (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich88[policy, ,6], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich88[policy, ,14], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich88[policy, ,22], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average income segregation in school 88",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich106[policy, ,6], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich106[policy, ,14], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich106[policy, ,22], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average income segregation in school 106",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")






# plot of mean income segregation (median) (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich88[policy, ,7], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich88[policy, ,15], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich88[policy, ,23], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average income segregation in school 88",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich106[policy, ,7], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich106[policy, ,15], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich106[policy, ,23], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average income segregation in school 106",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")






# plot of mean racial segregation, all races (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich88[policy, ,2], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich88[policy, ,10], na.rm = T))
  #mean_coleman <- c(mean_coleman, mean(tables_rich88[policy, ,23], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average racial segregation in school 88",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
#lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich106[policy, ,2], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich106[policy, ,10], na.rm = T))
  #mean_coleman <- c(mean_coleman, mean(tables_rich106[policy, ,23], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average racial segregation in school 106",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
#lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")



# plot of mean racial segregation, black (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich88[policy, ,4], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich88[policy, ,12], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich88[policy, ,20], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average black segregation in school 88",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich106[policy, ,4], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich106[policy, ,12], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich106[policy, ,20], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average black segregation in school 106",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")



# plot of mean racial segregation, whites (all)
x <- x88
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich88[policy, ,3], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich88[policy, ,11], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich88[policy, ,19], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average white segregation in school 88",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")


x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich106[policy, ,3], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich106[policy, ,11], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich106[policy, ,19], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average white segregation in school 106",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")




# plot of mean racial segregation, hispanics (all)
# only school 106

x <- x106
mean_freeman <- c()
mean_ssi <- c()
mean_coleman <- c()
for (policy in 2:8) {
  mean_freeman <- c(mean_freeman, mean(tables_rich106[policy, ,5], na.rm = T))
  mean_ssi <- c(mean_ssi, mean(tables_rich106[policy, ,13], na.rm = T))
  mean_coleman <- c(mean_coleman, mean(tables_rich106[policy, ,21], na.rm = T))
  #  x <- c(x, policy)
}

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average hispanic segregation in school 106",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
legend("topleft", c("freeman", "ssi"), 
       text.col = c("red", "blue"), 
       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")


