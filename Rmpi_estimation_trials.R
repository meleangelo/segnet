rm(list = ls())
library(Rmpi)
library(mvtnorm)
# spawn slaves
mpi.spawn.Rslaves(nslaves = 14)
# call library
library(ergm)

# saturated schools
#saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
saturated<-c(1,2,3,7,8,28,81,88,106,115,126,175,194,369)

# formula for estimation
estimation.formula <- g ~ edges + triangles

# parameters for simulations
true.parameters <- c(-6,6/100^2)

# load data and send to each node
ff <- list()
#tobs <- data.frame(matrix(NA, nrow = 2, ncol = 16))
tobs <- data.frame(matrix(NA, nrow = 2, ncol = 14))
#for (ss in 1:16) {
for (ss in 1:14) {
  cat(ss)
    urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/net",saturated[ss],".Rdata",sep="")
  load(urls)
  #mpi.send.Robj(net, dest = ss, tag = 1)
  ff[[ss]] <- net
  tobs[,ss] <- summary(net ~ edges + triangles) 
}
ff
tobs

## broadcast functions
mpi.bcast.Robj2slave(ergm)
mpi.bcast.Robj2slave(network)
mpi.bcast.Robj2slave(true.parameters)
#mpi.bcast.Robj2slave(flomarriage)
mpi.bcast.Robj2slave(estimation.formula)
mpi.bcast.Robj2slave(simulate.formula)
mpi.bcast.Robj2slave(control.simulate.formula)
mpi.bcast.Robj2slave(ff)

# # function to receive network data 
# recv <- function(){
#   if(mpi.comm.rank() == 1){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 2){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 3){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 4){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 5){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 6){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 7){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 8){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 9){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 10){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 11){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 12){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 13){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 14){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 15){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   } else if(mpi.comm.rank() == 16){
#     g0 <- mpi.recv.Robj(source = 0, 1 )
#   }
# }
# 
# # broadcast function to receive data
# mpi.bcast.Robj2slave(recv)
# # broadcast command to receive data
# mpi.bcast.cmd(recv)
# # execute command to receive data and store in g0 in each slave
# mpi.remote.exec(recv())

tobs_tot <- rowSums(tobs)
theta <- true.parameters
number_param_simulations <- 100
sigma.proposal <- diag(c(.5,.0001), nrow = 2, ncol = 2)
mean.prior <- c(-5,.002)
sigma.prior <- diag(c(10,1))
Theta <- data.frame(matrix(NA, nrow = number_param_simulations, ncol= 2) )
# simulate network g in each slave
start <- Sys.time()
for (s in 1:number_param_simulations){
  cat("simulation")
  cat(s)
  cat("\n")
  theta.new <- theta + rmvnorm(1,sigma = sigma.proposal)
  mpi.bcast.Robj2slave(theta.new)
  rr <- mpi.remote.exec(g<-simulate.formula(estimation.formula, coef = theta.new, 
                                            basis = ff[[mpi.comm.rank()]],
                                            control=control.simulate.formula(
                                              MCMC.burnin=1000)))
  tsim <- mpi.remote.exec(summary(estimation.formula), simplify = T)
  tsim_tot <- rowSums(tsim)
  
  pr <- dmvnorm(rbind(theta.new,theta), mean = mean.prior, sigma = sigma.prior)
  
  log_alphaex <- (theta - theta.new) %*% (tsim_tot-tobs_tot) + pr[1] - pr[2]
  if (log_alphaex >= log(runif(1))) {
    theta <- theta.new
  }
  Theta[s,] <- theta
}
Sys.time()-start

# close mpi session and close slaves
mpi.close.Rslaves()

