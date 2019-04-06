# figures of policy counterfactuals for paper april 2019

############################################################## 
# POLICY BASED ON INCOME 
# SWAP RICHER KIDS FROM SCHOOL 88 TO SCHOOL 106
###############################################################
rm(list=ls())

urls <- "D:/segnet/results/policy/policy_richswap_88_106.RData"

load(urls)
###
# fraction of nonrich in schools
x88 <- c(50,55,60,65,70,75,80)/90
x106 <- c(35,30,25,20,15,10,5)/81


urls <- "D:/segnet/results/policy/figures"
setwd(urls)

pdf("richswap88.pdf")
par(mfrow = c(3,2))
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
legend("bottomleft", c("freeman", "ssi", "coleman"), 
       text.col = c("red", "blue", "black"), 
       col = c("red", "blue", "black"),  pch = 21, lwd = 2, lty = c(1,2,3))
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

dev.off()




# sc hool 106

pdf("richswap106.pdf")
par(mfrow = c(3,2))

# plot of mean gender segregation (all)

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






# plot of mean racial segregation, all races (all)

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
legend("bottomleft", c("freeman", "ssi", "coleman"), 
       text.col = c("red", "blue", "black"), 
       col = c("red", "blue", "black"),  pch = 21, lwd = 2, lty = c(1,2,3))
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")



# plot of mean racial segregation, whites (all)

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




# plot of mean income segregation (all)

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






dev.off()



# school 106 hispanics
pdf("richswaphisp106.pdf")
par(mfrow=c(1,1))
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

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Average hispanic segregation in school 106",
     xlab = "fraction with income below median in original school", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
legend("bottomleft", c("freeman", "ssi"), 
       text.col = c("red", "blue"), 
       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))

dev.off()




# density and transitivity
pdf("rich_densitytrans.pdf")
par(mfrow=c(2,1))

# plot of density and transitivity school 88
x <- x88
mean_density <- c()
mean_trans <- c()
for (policy in 2:8) {
  mean_density <- c(mean_density, mean(tables_rich88[policy, ,27], na.rm = T))
  mean_trans <- c(mean_trans, mean(tables_rich88[policy, ,26], na.rm = T))
}

plot(x,mean_density, type = "b", col = "red", lwd = 2, ylim= c(0,.2), 
     main = "Average density and clustering in school 88",
     xlab = "fraction with income below median in original school", 
     ylab = "density and clustering level")
lines(x,mean_trans, type = "b", lwd = 2, lty = 2, col = "blue")
legend("topleft", c("density", "clustering"), 
       text.col = c("red", "blue"), 
       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))



# plot of density and transitivity school 106
x <- x106
mean_density <- c()
mean_trans <- c()
for (policy in 2:8) {
  mean_density <- c(mean_density, mean(tables_rich106[policy, ,27], na.rm = T))
  mean_trans <- c(mean_trans, mean(tables_rich106[policy, ,26], na.rm = T))
}

plot(x,mean_density, type = "b", col = "red", lwd = 2, ylim= c(0,.2), 
     main = "Average density and clustering in school 106",
     xlab = "fraction with income below median in original school", 
     ylab = "density and clustering level")
lines(x,mean_trans, type = "b", lwd = 2, lty = 2, col = "blue")
legend("topleft", c("density", "clustering"), 
       text.col = c("red", "blue"), 
       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))


dev.off()


# race and gender of central individuals

pdf("rich_racegendercentral.pdf")
par(mfrow=c(3,2))
#INDEGREE

# race and gender of most central
x <- x88
indeg_black <- c()
indeg_female <- c()
for (policy in 2:8) {
  indeg_black <- c(indeg_black, sum(tables_rich88[policy, ,29]==2)/1000)
  indeg_female <- c(indeg_female, sum(tables_rich88[policy, ,28]==1)/1000)
}

plot(x,indeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most popular, school 88",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,indeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
#legend("topleft", c("student is black", "student is female"), 
#       text.col = c("red", "blue"), 
#       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))



# race and gender of most central
x <- x106
indeg_black <- c()
indeg_female <- c()
for (policy in 2:8) {
  indeg_black <- c(indeg_black, sum(tables_rich106[policy, ,29]==2)/1000)
  indeg_female <- c(indeg_female, sum(tables_rich106[policy, ,28]==1)/1000)
}

plot(x,indeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most popular, school 106",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,indeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
#legend("topleft", c("student is black", "student is female"), 
#       text.col = c("red", "blue"), 
#       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))






# race and gender of central individuals

#OUTDEGREE

# race and gender of most central
x <- x88
outdeg_black <- c()
outdeg_female <- c()
for (policy in 2:8) {
  outdeg_black <- c(outdeg_black, sum(tables_rich88[policy, ,35]==2)/1000)
  outdeg_female <- c(outdeg_female, sum(tables_rich88[policy, ,34]==1)/1000)
}

plot(x,outdeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most active, school 88",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,outdeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
#legend("topleft", c("student is black", "student is female"), 
#       text.col = c("red", "blue"), 
#       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))



# race and gender of most central
x <- x106
outdeg_black <- c()
outdeg_female <- c()
for (policy in 2:8) {
  outdeg_black <- c(outdeg_black, sum(tables_rich106[policy, ,35]==2)/1000)
  outdeg_female <- c(outdeg_female, sum(tables_rich106[policy, ,34]==1)/1000)
}

plot(x,outdeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most active, school 106",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,outdeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
#legend("bottomright", c("student is black", "student is female"), 
 #      text.col = c("red", "blue"), 
#       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))




# race and gender of central individuals

# Eigen Vector centrality

# race and gender of most central
x <- x88
eigdeg_black <- c()
eigdeg_female <- c()
for (policy in 2:8) {
  eigdeg_black <- c(eigdeg_black, sum(tables_rich88[policy, ,32]==2)/1000)
  eigdeg_female <- c(eigdeg_female, sum(tables_rich88[policy, ,31]==1)/1000)
}

plot(x,eigdeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most central, school 88",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,eigdeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
#legend("bottomright", c("student is black", "student is female"), 
#       text.col = c("red", "blue"), 
#       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))



# race and gender of most central
x <- x106
eigdeg_black <- c()
eigdeg_female <- c()
for (policy in 2:8) {
  eigdeg_black <- c(eigdeg_black, sum(tables_rich106[policy, ,32]==2)/1000)
  eigdeg_female <- c(eigdeg_female, sum(tables_rich106[policy, ,31]==1)/1000)
}

plot(x,eigdeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most central, school 106",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,eigdeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
legend("bottomright", c("student is black", "student is female"), 
       text.col = c("red", "blue"), 
       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))

dev.off()





############################################################## 
# POLICY BASED ON RACE
# SWAP BLACK KIDS FROM SCHOOL 106 TO SCHOOL 88
###############################################################
rm(list=ls())

urls <- "D:/segnet/results/policy/policy_raceswap_88_106.RData"

load(urls)
###

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



urls <- "D:/segnet/results/policy/figures"
setwd(urls)



pdf("raceswap88.pdf")
par(mfrow = c(3,2))
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
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")


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
#lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")



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

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average black segregation in school 88",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")




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

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average white segregation in school 88",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
legend("bottomleft", c("freeman", "ssi", "coleman"), 
       text.col = c("red", "blue", "black"), 
       col = c("red", "blue", "black"),  pch = 21, lwd = 2, lty = c(1,2,3))
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")



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
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")


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
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")

dev.off()




# sc hool 106

pdf("raceswap106.pdf")
par(mfrow = c(3,2))

# plot of mean gender segregation (all)

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
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")






# plot of mean racial segregation, all races (all)

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
#lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")



# plot of mean racial segregation, black (all)

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

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average black segregation in school 106",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")



# plot of mean racial segregation, whites (all)

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

plot(x,mean_freeman, type = "b", col = "red", lwd = 2, ylim= c(-1,1), 
     main = "Average white segregation in school 106",
     xlab = "fraction of blacks", 
     ylab = "segregation level")
legend("bottomleft", c("freeman", "ssi", "coleman"), 
       text.col = c("red", "blue", "black"), 
       col = c("red", "blue", "black"),  pch = 21, lwd = 2, lty = c(1,2,3))
lines(x,mean_ssi, type = "b", lwd = 2, lty = 2, col = "blue")
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")




# plot of mean income segregation (all)

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
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")






# plot of mean income segregation (median) (all)

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
lines(x,mean_coleman, type = "b", lwd = 2, lty = 3, col = "black")






dev.off()



# school 106 hispanics
pdf("richswaphisp106.pdf")
par(mfrow=c(1,1))
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
legend("bottomleft", c("freeman", "ssi"), 
       text.col = c("red", "blue"), 
       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))

dev.off()




# density and transitivity
pdf("rich_densitytrans.pdf")
par(mfrow=c(2,1))

# plot of density and transitivity school 88
x <- x88
mean_density <- c()
mean_trans <- c()
for (policy in 2:16) {
  mean_density <- c(mean_density, mean(tables88[policy, ,27], na.rm = T))
  mean_trans <- c(mean_trans, mean(tables88[policy, ,26], na.rm = T))
}

plot(x,mean_density, type = "b", col = "red", lwd = 2, ylim= c(0,.2), 
     main = "Average density and clustering in school 88",
     xlab = "fraction of blacks", 
     ylab = "density and clustering level")
lines(x,mean_trans, type = "b", lwd = 2, lty = 2, col = "blue")
legend("topleft", c("density", "clustering"), 
       text.col = c("red", "blue"), 
       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))



# plot of density and transitivity school 106
x <- x106
mean_density <- c()
mean_trans <- c()
for (policy in 2:16) {
  mean_density <- c(mean_density, mean(tables106[policy, ,27], na.rm = T))
  mean_trans <- c(mean_trans, mean(tables106[policy, ,26], na.rm = T))
}

plot(x,mean_density, type = "b", col = "red", lwd = 2, ylim= c(0,.2), 
     main = "Average density and clustering in school 106",
     xlab = "fraction of blacks", 
     ylab = "density and clustering level")
lines(x,mean_trans, type = "b", lwd = 2, lty = 2, col = "blue")
legend("topleft", c("density", "clustering"), 
       text.col = c("red", "blue"), 
       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))


dev.off()


# race and gender of central individuals

pdf("rich_racegendercentral.pdf")
par(mfrow=c(3,2))
#INDEGREE

# race and gender of most central
x <- x88
indeg_black <- c()
indeg_female <- c()
for (policy in 2:16) {
  indeg_black <- c(indeg_black, sum(tables88[policy, ,29]==2)/1000)
  indeg_female <- c(indeg_female, sum(tables88[policy, ,28]==1)/1000)
}

plot(x,indeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most popular, school 88",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,indeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
#legend("topleft", c("student is black", "student is female"), 
#       text.col = c("red", "blue"), 
#       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))



# race and gender of most central
x <- x106
indeg_black <- c()
indeg_female <- c()
for (policy in 2:16) {
  indeg_black <- c(indeg_black, sum(tables106[policy, ,29]==2)/1000)
  indeg_female <- c(indeg_female, sum(tables106[policy, ,28]==1)/1000)
}

plot(x,indeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most popular, school 106",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,indeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
#legend("topleft", c("student is black", "student is female"), 
#       text.col = c("red", "blue"), 
#       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))






# race and gender of central individuals

#OUTDEGREE

# race and gender of most central
x <- x88
outdeg_black <- c()
outdeg_female <- c()
for (policy in 2:16) {
  outdeg_black <- c(outdeg_black, sum(tables88[policy, ,35]==2)/1000)
  outdeg_female <- c(outdeg_female, sum(tables88[policy, ,34]==1)/1000)
}

plot(x,outdeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most active, school 88",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,outdeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
#legend("topleft", c("student is black", "student is female"), 
#       text.col = c("red", "blue"), 
#       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))



# race and gender of most central
x <- x106
outdeg_black <- c()
outdeg_female <- c()
for (policy in 2:16) {
  outdeg_black <- c(outdeg_black, sum(tables106[policy, ,35]==2)/1000)
  outdeg_female <- c(outdeg_female, sum(tables106[policy, ,34]==1)/1000)
}

plot(x,outdeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most active, school 106",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,outdeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
#legend("bottomright", c("student is black", "student is female"), 
#      text.col = c("red", "blue"), 
#       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))




# race and gender of central individuals

# Eigen Vector centrality

# race and gender of most central
x <- x88
eigdeg_black <- c()
eigdeg_female <- c()
for (policy in 2:16) {
  eigdeg_black <- c(eigdeg_black, sum(tables88[policy, ,32]==2)/1000)
  eigdeg_female <- c(eigdeg_female, sum(tables88[policy, ,31]==1)/1000)
}

plot(x,eigdeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most central, school 88",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,eigdeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
#legend("bottomright", c("student is black", "student is female"), 
#       text.col = c("red", "blue"), 
#       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))



# race and gender of most central
x <- x106
eigdeg_black <- c()
eigdeg_female <- c()
for (policy in 2:16) {
  eigdeg_black <- c(eigdeg_black, sum(tables106[policy, ,32]==2)/1000)
  eigdeg_female <- c(eigdeg_female, sum(tables106[policy, ,31]==1)/1000)
}

plot(x,eigdeg_black, type = "b", col = "red", lwd = 2, ylim= c(0,1), 
     main = "Race and gender, most central, school 106",
     xlab = "fraction income below median (original school)", 
     ylab = "fraction of simulations")
lines(x,eigdeg_female, type = "b", lwd = 2, lty = 2, col = "blue")
legend("bottomright", c("student is black", "student is female"), 
       text.col = c("red", "blue"), 
       col = c("red", "blue"),  pch = 21, lwd = 2, lty = c(1,2))

dev.off()






