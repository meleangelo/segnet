rm(list = ls())
library(Rmpi)
library(mvtnorm)
# spawn slaves
mpi.spawn.Rslaves(nslaves = 16)
# call library
library(statnet)
library(ergm)
library(ergm.userterms)


# open privious run data saved
load("D:/segnet/results/estimation/Rmpi_estimation_newspec_run3.RData")

# use mean of Theta as starting value
theta.start <- as.numeric(colMeans(Theta))

# keep only var-cov matrix and starting value theta
rm(list=ls()[!ls() %in% c("qq", "theta.start")])

# need to modify the matrix of correlations to add 
# two parameters for the largest schools 
qq2 <- matrix(0,ncol = 50, nrow=50)
qq2[1:29,1:29] <- qq[1:29,1:29]
qq2[32:50,32:50] <- qq[30:48,30:48]
qq2[1:29,32:50] <- qq[1:29,30:48]
qq2[32:50,1:29] <- qq[30:48,1:29]
qq2[1:29,30:31] <- qq[1:29,30:31]
qq2[32:50,30:31] <- qq[30:48,30:31]
#qq2[30:31,1:29] <- qq[30:31,1:29]
#qq2[30:31,32:50] <- qq[30:31,30:48]
qq2[,30] <- qq2[,30] + rnorm(n = 50, mean = mean(qq2[,30]), sd = sd(qq2[,30]))
qq2[,31] <- qq2[,31] + rnorm(n = 50, mean = mean(qq2[,31]), sd = sd(qq2[,31]))
qq2[30:31,] <- t(qq2[,30:31])
qq2[30,30] <- qq[30,30] 
qq2[30,31] <- qq[30,30] 
qq2[31,31] <- qq[30,30] 
qq2[31,30] <- qq[30,30] 
qq <- qq2
rm("qq2")
#
theta.start <- c(theta.start[1:29], theta.start[30], theta.start[30], theta.start[30:48])


#
# saturated schools
saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
#saturated<-c(1,2,3,7,8,28,81,88,106,115,126,175,194,369)

# formula for estimation
estimation.formula <- g ~ edges + 
  nodeofactor("gender") +
  nodeocov("white") +
  nodeocov("black") +
  nodeocov("hisp") +
  nodeocov("loginc") +
  nodematch("gender") + nodematch("grade") +
  nodematch("race", diff = TRUE, keep = c(1,2,4)) + 
  nodeocov("phys_attr") + nodeicov("phys_attr") +
  nodeocov("pers_attr") + nodeicov("pers_attr") +
  diff("loginc") + nodecov("loginc") +
  nodeocov("fwhite") + nodeocov("fblack") + 
  nodeocov("fhisp") +
  edgecov(ff[[mpi.comm.rank()]],"ww_fwhite") + 
  edgecov(ff[[mpi.comm.rank()]],"bb_fblack") +
  edgecov(ff[[mpi.comm.rank()]],"hh_fhisp") +
  nodeocov("sch1") +
  nodeocov("sch2") +
  nodeocov("sch3") +
  nodeocov("sch4") +
  nodeocov("sch5") +
  nodeocov("sch6") +
  nodeocov("sch7") +
  nodeocov("sch8") +
  nodeocov("sch9") +
  nodeocov("sch10") +
  nodeocov("sch11") +
  nodeocov("sch12") +
  nodeocov("sch13") +
  nodeocov("sch14") +
  nodeocov("sch15") +
  mutual + mutual(same = "gender") +
  mutual(same = "grade") + 
  mutual(same = "race", diff = TRUE, keep = c(1,2,4)) +
  #mutual(same = "phys_attr", diff = TRUE) +
  #mutual(same = "pers_attr", diff = TRUE) +
  twopath +
  indhomophily("gender", diff = F) +
  indhomophily("grade", diff = F) +
  indhomophily("race", diff = T, levels = c(1,2,4))

# formula for estimation
tobs.formula <- net ~ edges + 
  nodeofactor("gender") +
  nodeocov("white") +
  nodeocov("black") +
  nodeocov("hisp") +
  nodeocov("loginc") +
  nodematch("gender") + nodematch("grade") +
  nodematch("race", diff = TRUE, keep = c(1,2,4)) + 
  nodeocov("phys_attr") + nodeicov("phys_attr") +
  nodeocov("pers_attr") + nodeicov("pers_attr") +
  diff("loginc") + nodecov("loginc") +
  nodeocov("fwhite") + nodeocov("fblack") + 
  nodeocov("fhisp") +
  edgecov(net,"ww_fwhite") + edgecov(net,"bb_fblack") +
  edgecov(net,"hh_fhisp") +
  nodeocov("sch1") +
  nodeocov("sch2") +
  nodeocov("sch3") +
  nodeocov("sch4") +
  nodeocov("sch5") +
  nodeocov("sch6") +
  nodeocov("sch7") +
  nodeocov("sch8") +
  nodeocov("sch9") +
  nodeocov("sch10") +
  nodeocov("sch11") +
  nodeocov("sch12") +
  nodeocov("sch13") +
  nodeocov("sch14") +
  nodeocov("sch15") +
  mutual + mutual(same = "gender") +
  mutual(same = "grade") + 
  mutual(same = "race", diff = TRUE, keep = c(1,2,4)) +
  #  mutual(same = "phys_attr", diff = TRUE) +
  #  mutual(same = "pers_attr", diff = TRUE) +
  twopath +
  indhomophily("gender", diff = F) +
  indhomophily("grade", diff = F) +
  indhomophily("race", diff = T, levels = c(1,2,4))

# parameters for simulations
#true.parameters <- c(-6,6/100^2)

# load data and send to each node
ff <- list()
#tobs <- data.frame(matrix(NA, nrow = 2, ncol = 16))
tobs <- data.frame(matrix(NA, nrow = 50, ncol = 16))
#for (ss in 1:16) {
for (ss in 1:16) {
  cat(ss)
  urls<-paste("E:/backup_phoenix/addhealth/schools/saturated/net",saturated[ss],".Rdata",sep="")
  load(urls)
  #mpi.send.Robj(net, dest = ss, tag = 1)
  ff[[ss]] <- net
  tobs[,ss] <- summary(tobs.formula) 
}
ff
tobs

## broadcast functions
mpi.bcast.Robj2slave(ergm)
mpi.bcast.Robj2slave(network)
#mpi.bcast.Robj2slave(true.parameters)
#mpi.bcast.Robj2slave(flomarriage)
mpi.bcast.Robj2slave(estimation.formula)
mpi.bcast.Robj2slave(simulate.formula)
mpi.bcast.Robj2slave(control.simulate.formula)
mpi.bcast.Robj2slave(ff)
mpi.remote.exec(g <- ff[[mpi.comm.rank()]])
mpi.remote.exec(library(statnet))
mpi.remote.exec(library(mvtnorm))
mpi.remote.exec(library(ergm.userterms))

# observed sufficient stats
tobs_tot <- rowSums(tobs)
# starting value for theta
theta <- theta.start

# parameter simulations
number_param_simulations <- 5000
# var-covar matrix for RW proposal
sigma.proposal <- qq # this is the cov of Theta in the previous run
sigma.proposal <- sigma.proposal*(2.38^2)/50

# prior mean
mean.prior <- rep(0,50)
# prior var-covar
sigma.prior <- diag(rep(10,50))

# data frame of all parameters simulations
Theta <- data.frame(matrix(NA, nrow = number_param_simulations, ncol= 50) )
accept.ratio <- rep(NA, number_param_simulations)
num.accept <- 0

# start loop
start <- Sys.time()
for (s in 1:number_param_simulations){
  cat("simulation ")
  cat(s)
  cat("\n")
  # propose new theta
  theta.new <- as.numeric(theta + rmvnorm(1,sigma = sigma.proposal) )
  
  # broadcast new theta to all slaves
  mpi.bcast.Robj2slave(theta.new)
  
  # simulate the network g' in all slaves
  rr <- mpi.remote.exec(g<-simulate.formula(estimation.formula, coef = theta.new, 
                                            basis = ff[[mpi.comm.rank()]],
                                            control=control.simulate.formula(
                                              MCMC.burnin=10000)))
  # compute summary stats of simulated networks
  tsim <- mpi.remote.exec(summary(estimation.formula), simplify = T)
  # aggregate summary stats
  tsim_tot <- rowSums(tsim)
  
  # prior
  pr <- dmvnorm(rbind(theta.new,theta), mean = mean.prior, sigma = sigma.prior)
  # log of acceptance rate of exchange algo
  log_alphaex <- (theta - theta.new) %*% (tsim_tot-tobs_tot) + pr[1] - pr[2]
  
  accept <- 0
  # accept or reject
  if (log_alphaex >= log(runif(1))) {
    theta <- theta.new
    accept <- 1
  }
  # write current theta to posterior
  Theta[s,] <- theta
  num.accept <- num.accept + accept
  accept.ratio[s] <- (num.accept)/s
  cat(paste("acceptance ratio = ", accept.ratio[s], sep = ""))
  cat("\n")
  
}
Sys.time()-start

# close mpi session and close slaves
mpi.close.Rslaves()

# we save the covariance of the simulations to use in the next run
qq <- cov(Theta) 

# save last value of Theta to start the next run
theta.start <- as.numeric(Theta[number_param_simulations,])



#save.image

save.image("D:/segnet/results/estimation/Rmpi_estimation_newspec_allsat_run1.RData")