#### THIS WORKS!!!!!!!

library(Rmpi)
# spawn slaves
mpi.spawn.Rslaves(nslaves = 2)
# call library
library(ergm)
# size of network
n <- 100
n1 <- 50
# parameters for simulations
true.parameters <- c(-6,6/n^2)
# generate network as erdos 
g0 <- network(matrix(rbinom(n^2,1,.1), ncol = n, nrow = n), mode="undirected")
# generate network simulating an ergm 
g0<-simulate.formula(g0 ~ edges + triangles, coef = true.parameters, 
                     control=control.simulate.formula(
                       MCMC.burnin=100000))
# generate network as erdos 
g1 <- network(matrix(rbinom(n1^2,1,.1), ncol = n1, nrow = n1), mode="undirected")
# generate network simulating an ergm 
g1<-simulate.formula(g1 ~ edges + triangles, coef = true.parameters, 
                     control=control.simulate.formula(
                       MCMC.burnin=100000))


## broadcast functions
mpi.bcast.Robj2slave(ergm)
mpi.bcast.Robj2slave(network)
mpi.bcast.Robj2slave(n)
mpi.bcast.Robj2slave(true.parameters)
#mpi.bcast.Robj2slave(flomarriage)
mpi.bcast.Robj2slave(simulate.formula)
mpi.bcast.Robj2slave(control.simulate.formula)

# send network g0 to slave 1
mpi.send.Robj(g0, dest = 1, tag = 1)

# generate network g1
#g0
#g1<-simulate.formula(g0 ~ edges + triangles, coef = true.parameters, 
#                     control=control.simulate.formula(
#                       MCMC.burnin=100000))
#g1
# send network g1 to slave 2
mpi.send.Robj(g1, dest = 2, tag = 1)

# function to receive network data 
recv <- function(){
  if(mpi.comm.rank() == 1){
    g0 <- mpi.recv.Robj(source = 0, 1 )
  } else if(mpi.comm.rank() == 2){
    g0 <- mpi.recv.Robj(source = 0, 1 )
  }
}
# broadcast function to receive data
mpi.bcast.Robj2slave(recv)
# broadcast command to receive data
mpi.bcast.cmd(recv)
# execute command to receive data and store in g0 in each slave
mpi.remote.exec(g0 <-recv())


# simulate network g in each slave
start <- Sys.time()
rr <- mpi.remote.exec(g<-simulate.formula(g ~ edges + triangles, coef = true.parameters, 
                                          basis = g0,
                                          control=control.simulate.formula(
                                            MCMC.burnin=1000000)))
Sys.time()-start

rr
# collect summary stats
tsim <- mpi.remote.exec(summary(g~edges + triangles), simplify = TRUE)
tsim


# close mpi session and close slaves
mpi.close.Rslaves()



library(Rmpi)
# spawn slaves
mpi.spawn.Rslaves(nslaves = 2)

library(ergm)
data(florentine)
#
# Fit a model where the propensity to form ties between
# families depends on the absolute difference in wealth
#

runmpi <- function(){
  library(ergm)
  data(florentine)
  gest <- ergm(flomarriage ~ edges + absdiff("wealth"))
  summary(gest)
}
mpi.bcast.Robj2slave(runmpi)

mpi.remote.exec(runmpi,
                simplify = TRUE, 
                comm = 1, 
                ret = TRUE) 
mpi.parReplicate(4, runmpi())
#

# close slaves
mpi.close.Rslaves()


library(Rmpi)
library(parallel)
library(snow)
cl <- makeMPIcluster(mpi.universe.size()-1)
A <- matrix(rnorm(1000000), 1000)
system.time(A %*% A)
system.time(parMM(cl, A, A))
clusterCall(cl, function() Sys.info()['nodename'])
stopCluster(cl)
mpi.quit()




library(Rmpi)
# spawn slaves
mpi.spawn.Rslaves(nslaves = 2)
library(ergm)
data(florentine)
n <- 100
true.parameters <- c(-6,6/n^2)
g0 <- network(matrix(rbinom(n^2,1,.1), ncol = n, nrow = n), mode="undirected")
g0<-simulate.formula(g0 ~ edges + triangles, coef = true.parameters, 
                    control=control.simulate.formula(
                      MCMC.burnin=100000))

g1 <- g0 
g2 <- simulate.formula(g0 ~ edges + triangles, coef = true.parameters, 
                       basis = g1,
                       control=control.simulate.formula(
                         MCMC.burnin=100000))
g0 <- network(matrix(0, ncol = n, nrow = n), mode="undirected")

net <- list(g1,g2)

mpi.bcast.Robj2slave(ergm)
mpi.bcast.Robj2slave(network)
mpi.bcast.Robj2slave(n)
mpi.bcast.Robj2slave(true.parameters)
#mpi.bcast.Robj2slave(flomarriage)
mpi.bcast.Robj2slave(simulate.formula)
mpi.bcast.Robj2slave(control.simulate.formula)
mpi.bcast.Robj2slave(g1)
mpi.bcast.Robj2slave(g2)
mpi.bcast.Robj2slave(g0)
#define function to receive network data in slave
srecv1<-function(){
  if(mpi.comm.rank()==1){
    g0 <- mpi.recv.Robj(source = 0, mpi.any.tag())
  }
}
srecv2 <- function(){
  if(mpi.comm.rank()==2) {
    g0 <- mpi.recv.Robj(source = 0, mpi.any.tag())
  }
}

# send network to slave 1 and 2
mpi.send.Robj(g1, dest = 1, tag = 1)
mpi.send.Robj(g2, dest = 2, tag = 1)


# send receiving function to slaves
mpi.bcast.Robj2slave(srecv)
mpi.bcast.Robj2slave(srecv1)
mpi.bcast.Robj2slave(srecv2)
mpi.bcast.cmd(srecv1())
mpi.bcast.cmd(srecv2())
mpi.remote.exec(srecv())
mpi.remote.exec(srecv1())
mpi.remote.exec(srecv2())



#mpi.bcast.Robj2slave(g0)
# send network to slave 1 and 2
mpi.send.Robj(g1, dest = 1, tag = 1)
mpi.send.Robj(g2, dest = 2, tag = 1)

# create empty network in slaves
mpi.remote.exec(g0 <- network(matrix(0, ncol = n, nrow = n), mode="undirected"))


mpi.bcast.cmd(srecv(1))
mpi.bcast.cmd(srecv(2))
mpi.bcast.cmd(srecv())
mpi.remote.exec(g0)
mpi.remote.exec(set.seed(1977))
#mpi.remote.exec(g0 <- network(matrix(rbinom(n^2,1,.1), ncol = n, nrow = n), mode="undirected"))


start <- Sys.time()
rr <- mpi.remote.exec(g<-simulate.formula(g ~ edges + triangles, coef = true.parameters, 
                                          basis = g0,
                                          control=control.simulate.formula(
                                            MCMC.burnin=1000)))
Sys.time()-start

tsim <- mpi.remote.exec(summary(g~edges + triangles), simplify = TRUE)
tsim
g0
mpi.close.Rslaves()











#on a slave
mpi.send(1:10,1,0,0)

#on master
x <- integer(10)
mpi.irecv(x,1,1,0)
x	
mpi.wait()
x



library('Rmpi')
mpi.spawn.Rslaves(nslaves=3)

#define function to receive data in slave
srecv<-function(){
  if(mpi.comm.rank()==2)
    x<-mpi.recv(x,1,0,1,1)
}

#send the function to all slaves
mpi.bcast.Robj2slave(srecv)

#send an integer from master
x<-as.integer(21.34)
mpi.send(x,1,2,1,1)

#create x to receive data in slaves
mpi.bcast.cmd(x<-integer(1))

                         #call the function
                         mpi.bcast.cmd(srecv())
                         
                         #check results
                         mpi.remote.exec(x)
                         mpi.close.Rslaves()