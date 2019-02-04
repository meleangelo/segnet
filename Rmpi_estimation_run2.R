rm(list = ls())
library(Rmpi)
library(mvtnorm)
# spawn slaves
mpi.spawn.Rslaves(nslaves = 14)
# call library
library(statnet)
library(ergm)
library(ergm.userterms)

# open privious run data saved
load("D:/segnet/results/estimation/Rmpi_estimation_run1.RData")
# keep only var-cov matrix and starting value theta
rm(list=ls()[!ls() %in% c("qq", "theta.start")])

# saturated schools
#saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
saturated<-c(1,2,3,7,8,28,81,88,106,115,126,175,194,369)

# formula for estimation
estimation.formula <- g ~ edges + nodematch("gender") + nodematch("grade") +
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
tobs.formula <- net ~ edges + nodematch("gender") + nodematch("grade") +
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
tobs <- data.frame(matrix(NA, nrow = 43, ncol = 14))
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

theta<- theta.start # last Theta in the previous run
# i explicitly write the values here for transparency
theta <- c(-5.425677996,  0.334882179,  2.247613835,  0.542199765,  0.275451985,  1.488615125,
         0.096068105,  0.315460064, -0.007396053,  0.707580972,  0.190660082, -0.043868358,
        -0.835630584,  3.105351152,  2.510228544,  0.964098193,  0.045665491, -1.165658215,
        -1.449118989,  1.491811510, -0.195990449,  1.239582356,  1.363710428,  0.269861532,
         2.389539540, -0.087188801, -1.323467811,  1.591779939, -1.458224240,  2.633851771,
         0.523456169,  1.237596779,  1.055281053, -1.589522019, -0.075191315,  0.380998440,
         0.675306479, -0.381824266,  0.138097741, -0.254170744,  0.307665314,  0.043217019,
         0.312494965)

# parameter simulations
number_param_simulations <- 10000
# var-covar matrix for RW proposal
sigma.proposal <- qq # this is the cov of Theta in the previous run
sigma.proposal <- sigma.proposal*(2.38^2)/43

# prior mean
mean.prior <- rep(0,43)
# prior var-covar
sigma.prior <- diag(rep(10,43))

# data frame of all parameters simulations
Theta <- data.frame(matrix(NA, nrow = number_param_simulations, ncol= 43) )
accept.ratio <- rep(NA, number_param_simulations)
num.accept <- 0 # initialize number of accepted proposal

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
                                              MCMC.burnin=5000)))
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

save.image("D:/segnet/results/estimation/Rmpi_estimation_run2.RData")

