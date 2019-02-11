rm(list = ls())
library(Rmpi)
library(mvtnorm)
# spawn slaves
mpi.spawn.Rslaves(nslaves = 14)
# call library
library(statnet)
library(ergm)
library(ergm.userterms)


# saturated schools
#saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
saturated<-c(1,2,3,7,8,28,81,88,106,115,126,175,194,369)

# formula for estimation
estimation.formula <- g ~ edges + 
  nodeofactor("gender") +
  nodeofactor("race", base = c(3,5), levels = c(1,2,4)) +
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
  nodeofactor("race", base = c(3,5), levels = c(1,2,4)) +
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
tobs <- data.frame(matrix(NA, nrow = 48, ncol = 14))
#for (ss in 1:16) {
for (ss in 1:14) {
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
theta<-c( -5.044157474, 1, 1, 1, 1, 0.2,
          0.259356084,  2.277058198, 0.575633848, 
          0.732410943, 0.206711923, 0.039190666, 0.212548895, 0.073068835, 
          0.405024586, 0.021722638, 0.049191195, -1.948541133,  
          2.280103490, 1.524823072, -0.490846072,  0.185039422, 
          -0.766403107, -1.468356295, 1.786659877, 0.448470280, 
          0.950610139, 1.597194852, -0.775861615, 1.070339499, 
          -1.500689484, 0.190595142, 2.885862629, -1.832684418, 
          2.217127585, -1.240819045, 3.324373506, 0.863145951, 
          -1.951783438, 0.157419958, -0.379926712, 0.535236616, 
          #-0.212981612, 0.106510027, 0.363385523, -0.073684082, 
          -0.094063802, -0.106751101, -0.002036975, 0.162612509, 
          0.169127327, 0.181000003)

# parameter simulations
number_param_simulations <- 10000
# var-covar matrix for RW proposal
sigma.proposal <- diag(c(.5,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.25,
                         .25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,
                         .5,.1,.1,.1,.1,.1,
                         #.1,.1,.1,.1,
                         .5,.05,.05,.05,.05,.05), 
                       nrow = 48, ncol = 48)

# prior mean
mean.prior <- rep(0,48)
# prior var-covar
sigma.prior <- diag(rep(10,48))

# data frame of all parameters simulations
Theta <- data.frame(matrix(NA, nrow = number_param_simulations, ncol= 48) )
accept.ratio <- rep(NA, number_param_simulations)

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
                                              MCMC.burnin=1000)))
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
}
Sys.time()-start

# close mpi session and close slaves
mpi.close.Rslaves()

# we save the covariance of the simulations to use in the next run
qq <- cov(Theta) 

# save last value of Theta to start the next run
theta.start <- as.numeric(Theta[number_param_simulations,])



#save.image

save.image("D:/segnet/results/estimation/Rmpi_estimation_newspec_run1.RData")