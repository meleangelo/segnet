# segregation for table 1 descriptive stats
rm(list=ls())
saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)

tables <- data.frame(matrix(NA, nrow = 36, ncol = length(saturated)))

for (school in 1:length(saturated)){ 
  urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/net",saturated[school],".Rdata",sep="")
  load(urls)
  

  library(network)
  # get attributes that are relevant
  loginc <- get.vertex.attribute(net, "loginc")
  loginc50 <- median(loginc)
  loginc90 <- quantile(loginc, 0.9)
  loginc10 <- median(loginc, 0.1)
  
  gender <- get.vertex.attribute(net, "gender")
  race <- get.vertex.attribute(net, "race")
  loginc <- get.vertex.attribute(net, "loginc")
  

    if("isnar" %in% (.packages())){
      detach("package:isnar", unload=TRUE) 
    }
    if("igraph" %in% (.packages())){
      detach("package:igraph", unload=TRUE) 
    }
    library(network)
    # create adj matrix for igraph
    gg <- as.sociomatrix(net)
    
  
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
    
    V(ggg)$rich <- 1*(loginc >= loginc90)
    V(ggg)$rich2 <- 1*(loginc >= loginc50)
    V(ggg)$poor <- 1*(loginc <= loginc10)
    
    V(ggu)$race <- race
    V(ggu)$gender <- gender
    V(ggu)$loginc <- loginc
    V(ggu)$whites <- 1*(race == 1)
    V(ggu)$blacks <- 1*(race == 2)
    V(ggu)$hisps <- 1*(race == 4)
    
    V(ggu)$rich <- 1*(loginc >= loginc90)
    V(ggu)$rich2 <- 1*(loginc >= loginc50)
    V(ggu)$poor <- 1*(loginc <= loginc10)
    
    # compute freeman seg
    tables[1,school] <- freeman(ggu, "gender")
    if (saturated[school]!=115){
    tables[2,school] <- freeman(ggu, "race")
    }
    #tables[3,school] <- freeman(ggu, "whites")
    #tables[4,school] <- freeman(ggu, "blacks")
    #tables[5,school] <- freeman(ggu, "hisps")
    tables[6,school] <- freeman(ggu, "rich")
    tables[7,school] <- freeman(ggu, "rich2")
    tables[8,school] <- freeman(ggu, "poor")
    
    # compute spectral segregation index
    tables[9,school] <- mean(ssi(ggu, "gender"))
    if (saturated[school]!=115){
      tables[10,school] <- mean(ssi(ggu, "race"))
    }
    #tables[11,school] <- mean(ssi(ggu, "whites"))
    #tables[12,school] <- mean(ssi(ggu, "blacks"))
    #tables[13,school] <- mean(ssi(ggu, "hisps"))
    tables[14,school] <- mean(ssi(ggu, "rich"))
    tables[15,school] <- mean(ssi(ggu, "rich2"))
    tables[16,school] <- mean(ssi(ggu, "poor"))
    
    
    # compute coleman homophily
    tables[17,school] <- coleman(ggg, "gender")[1] #male
    tables[18,school] <- coleman(ggg, "gender")[2] #female
    #tables[19,school] <- coleman(ggg, "whites")[2]
    #tables[20,school] <- coleman(ggg, "blacks")[2]
    #tables[21,school] <- coleman(ggg, "hisps")[2]
    tables[22,school] <- coleman(ggg, "rich")[2]
    tables[23,school] <- coleman(ggg, "rich2")[2]
    tables[24,school] <- coleman(ggg, "poor")[2]
    
    # clustering
    tables[25,school] <- transitivity(ggg)
    tables[26,school] <- mean(transitivity(ggg), na.rm=T)
    
    # density
    tables[27,school] <- ecount(ggg)/(vcount(ggg)*(vcount(ggg)-1))
    
    # most central players, in-degree
    deg <- degree(ggg, mode = "in")
    nn <- which(deg == max(deg))
    tables[28,school] <- gender[nn[1]] # gender of most central 
    tables[29,school] <- race[nn[1]] # race of most central 
    tables[30,school] <- loginc[nn[1]] # log income of most central 
    # most central eigenvector
    ev <- evcent(ggg)$vector
    nn <- which(ev == max(ev))
    tables[31,school] <- gender[nn[1]] # gender of most central 
    tables[32,school] <- race[nn[1]] # race of most central 
    tables[33,school] <- loginc[nn[1]] # log income of most central 
    # most central players, out-degree
    deg <- degree(ggg, mode = "out")
    nn <- which(deg == max(deg))
    tables[34,school] <- gender[nn[1]] # gender of most central 
    tables[35,school] <- race[nn[1]] # race of most central 
    tables[36,school] <- loginc[nn[1]] # log income of most central 
    
    
    
    
    
    #table(race)/dim(gg)[1]
    #pr <- page.rank(ggg)$vector
    #nn <-which(pr == max(pr))
    #pr[nn]
    #ggg
    
    #race[nn]
    #gender[nn]
  
}


urls <- "D:/segnet/results/table1_seg.Rdata"
save.image(urls)
