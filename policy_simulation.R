## Policy_simulations

rm(list=ls())

#library
library(statnet)
library(ergm)
library(ergm.userterms)

# load posterior estimates
load("D:/segnet/results/estimation/Rmpi_estimation_newspec_allsat_run3.RData")
postmean <- colMeans(Theta)
postmean

# keep only var-cov matrix and starting value theta
rm(list=ls()[!ls() %in% c("postmean")])


estimation.formula <- net ~ edges + 
  nodeofactor("gender") +
  nodeocov("white") +
  nodeocov("black") +
  nodeocov("hisp") +
  nodeocov("loginc") +
  nodematch("gender") + nodematch("grade") +
  nodematch("race", diff = TRUE, levels = c(1,2,4)) + 
  nodeocov("phys_attr") + nodeicov("phys_attr") +
  nodeocov("pers_attr") + nodeicov("pers_attr") +
  diff("loginc") + nodecov("loginc") +
  nodeocov("fwhite") + nodeocov("fblack") + 
  nodeocov("fhisp") +
  edgecov(net,"ww_fwhite") + 
  edgecov(net,"bb_fblack") +
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
  twopath +
  indhomophily("gender", diff = F) +
  indhomophily("grade", diff = F) +
  indhomophily("race", diff = T, levels = c(1,2,4))


burnin = 100000
steps = 1000
num_simulations <- 1000

# load policy networks
for (i in 1:16) {
  urls <- paste("E:/backup_phoenix/addhealth/schools/saturated/netpolicy88_",i,".Rdata",sep="")
  load(urls)
  set.seed(1000)

  g <- simulate(estimation.formula, 
                nsim = num_simulations, 
                coef = postmean, 
                basis = net,
                  control = control.simulate.formula(MCMC.burnin = burnin,
                                                     MCMC.interval = steps))
  
  urls <- paste("D:/segnet/results/policy/netpolicy88_",i,".RData", sep = "")
  save(net, g, file = urls)
  rm(g)
}

# load policy networks
for (i in 1:16) {
  urls <- paste("E:/backup_phoenix/addhealth/schools/saturated/netpolicy106_",i,".Rdata",sep="")
  load(urls)
  set.seed(1000)
  
  g <- simulate(estimation.formula, 
                nsim = num_simulations, 
                coef = postmean, 
                basis = net,
                control = control.simulate.formula(MCMC.burnin = burnin,
                                                   MCMC.interval = steps))
  
  urls <- paste("D:/segnet/results/policy/netpolicy106_",i,".RData", sep = "")
  save(net, g, file = urls)
  rm(g)
}


#####################################################################
#####################################################################
#####################################################################

# policy swap of rich vs poor kids



rm(list=ls())

#library
library(statnet)
library(ergm)
library(ergm.userterms)

# load posterior estimates
load("D:/segnet/results/estimation/Rmpi_estimation_newspec_allsat_run3.RData")
postmean <- colMeans(Theta)
postmean

# keep only var-cov matrix and starting value theta
rm(list=ls()[!ls() %in% c("postmean")])


estimation.formula <- net ~ edges + 
  nodeofactor("gender") +
  nodeocov("white") +
  nodeocov("black") +
  nodeocov("hisp") +
  nodeocov("loginc") +
  nodematch("gender") + nodematch("grade") +
  nodematch("race", diff = TRUE, levels = c(1,2,4)) + 
  nodeocov("phys_attr") + nodeicov("phys_attr") +
  nodeocov("pers_attr") + nodeicov("pers_attr") +
  diff("loginc") + nodecov("loginc") +
  nodeocov("fwhite") + nodeocov("fblack") + 
  nodeocov("fhisp") +
  edgecov(net,"ww_fwhite") + 
  edgecov(net,"bb_fblack") +
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
  twopath +
  indhomophily("gender", diff = F) +
  indhomophily("grade", diff = F) +
  indhomophily("race", diff = T, levels = c(1,2,4))


burnin = 100000
steps = 1000
num_simulations <- 1000

# load policy networks
for (i in 1:8) {
  urls <- paste("E:/backup_phoenix/addhealth/schools/saturated/netpolicy_rich88_",i,".Rdata",sep="")
  load(urls)
  set.seed(1000)
  
  g <- simulate(estimation.formula, 
                nsim = num_simulations, 
                coef = postmean, 
                basis = net,
                control = control.simulate.formula(MCMC.burnin = burnin,
                                                   MCMC.interval = steps))
  
  urls <- paste("D:/segnet/results/policy/netpolicy_rich88_",i,".RData", sep = "")
  save(net, g, file = urls)
  rm(g)
}

# load policy networks
for (i in 1:8) {
  urls <- paste("E:/backup_phoenix/addhealth/schools/saturated/netpolicy_rich106_",i,".Rdata",sep="")
  load(urls)
  set.seed(1000)
  
  g <- simulate(estimation.formula, 
                nsim = num_simulations, 
                coef = postmean, 
                basis = net,
                control = control.simulate.formula(MCMC.burnin = burnin,
                                                   MCMC.interval = steps))
  
  urls <- paste("D:/segnet/results/policy/netpolicy_rich106_",i,".RData", sep = "")
  save(net, g, file = urls)
  rm(g)
}
