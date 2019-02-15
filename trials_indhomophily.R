#indhomR 
library(statnet)
library(ergm.userterms)
saturated<-c(1,2,3,7,8,28,81,88,106,115,126,175,194,369)
ss <-6
urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/net",saturated[ss],".Rdata",sep="")
load(urls)

summary(net ~ indhomophily)
summary(net ~ twopath)

adj <- as.sociomatrix(net)
n <- dim(adj)[1]
indww <- 0
for (i in 1:n) {
  for (j in 1: n) {
    if (adj[i,j] == 1) {
      for (k in 1:n) {
        if (k != i & k != j ){
          indww <- indww + 1*(adj[j,k]==1)
#          indww <- indww + 1*(adj[k,i]==1)
          
          
        }
        
      }
    }
  }
}
indww

x <- get.vertex.attribute(net, "race")
indww <- 0
for (i in 1:n) {
  for (j in 1: n) {
    if (adj[i,j] == 1) {
      for (k in 1:n) {
        if (k != i & k != j ){
          indww <- indww + 1*(adj[j,k]==1)*((x[i]==1) & (x[k]==1))
#          indww <- indww + 1*(adj[k,i]==1)*((x[i]==1) & (x[k]==1))
          
          
        }
        
      }
    }
  }
}
indww
indbb <- 0
for (i in 1:n) {
  for (j in 1: n) {
    if (adj[i,j] == 1) {
      for (k in 1:n) {
        if (k != i & k != j ){
          indbb <- indbb + 1*(adj[j,k]==1)*((x[i]==2) * (x[k]==2))
          #          indww <- indww + 1*(adj[k,i]==1)*((x[i]==1) & (x[k]==1))
          
          
        }
        
      }
    }
  }
}
indbb
indhh <- 0
for (i in 1:n) {
  for (j in 1: n) {
    if (adj[i,j] == 1) {
      for (k in 1:n) {
        if (k != i & k != j ){
          indhh <- indhh + 1*(adj[j,k]==1)*((x[i]==4) * (x[k]==4))
          #          indww <- indww + 1*(adj[k,i]==1)*((x[i]==1) & (x[k]==1))
          
          
        }
        
      }
    }
  }
}
indhh
summary(net ~ indhomophily("race", diff=T))


indww <- 0
for (i in 1:n) {
  for (j in 1: n) {
    if (adj[i,j] == 1) {
      for (k in 1:n) {
        if (k != i & k != j ){
          indww <- indww + 1*(adj[j,k]==1)*(x[i]==x[k])
          #indww <- indww + 1*(adj[k,i]==1)*((x[i]==1) & (x[k]==1))
          
          
        }
        
      }
    }
  }
}
indww
summary(net ~ indhomophily("race", diff=F))
xw <- 1*(x==1)
set.vertex.attribute(net, "white", xw)
summary(net ~ indhomophily("white", diff = T))

summary(net ~ indhomophily("white", diff = T))
summary(net ~ indhomophily("white", diff = T, levels = NULL))


        