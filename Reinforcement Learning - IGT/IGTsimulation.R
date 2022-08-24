# Optional generic preliminaries:
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 

# Load the required libraries
library(runjags)
library(permute)
library(ggplot2)
library(GGally)
library(HDInterval)
#------------------------------------------------------------------------------- 

#set up gain/loss outcomes from the IGT ----
Nblocks = 30
Ntotal = Nblocks * 10
gainMat = cbind(
  rep(1.00, Ntotal), #deck A
  rep(1.00, Ntotal), #deck B
  rep(0.50, Ntotal), #deck C
  rep(0.50, Ntotal)  #deck D
)

lossA = c(-1.5, -2.0, -2.5, -3.0, -3.5, rep(0, 5))
lossB = c(-12.5, rep(0, 9))
lossC = c(-0.25, rep(-0.50, 3), -0.75, rep(0, 5))
lossD = c(-2.5, rep(0, 9))

lossMat = cbind(
  lossA[t(shuffleSet(n = 10, nset = Nblocks))[1:Ntotal]], # shuffleSet func creates repeated random sequences of 1:10
  lossB[t(shuffleSet(n = 10, nset = Nblocks))[1:Ntotal]], # note the transpose function t()
  lossC[t(shuffleSet(n = 10, nset = Nblocks))[1:Ntotal]], # transposing the shuffleSet output gives the desired order
  lossD[t(shuffleSet(n = 10, nset = Nblocks))[1:Ntotal]]  # when indexing 1:Ntotal loss values
)

# check the expected values of a couple random blocks
colSums(gainMat[1:10, ] + lossMat[1:10, ])
colSums(gainMat[51:60, ] + lossMat[51:60, ])

# define agent parameters ----
alpha = c(0.1, 0.6, 0.6)
# beta = c(1, 1, 1)
W = c(1, 1, 3) # here a weight on losses assuming the weight on gains = 1

# prepare the data for JAGS ----
D = dim(lossMat)[2] #number of decks
dat1 <- dump.format(list(T=Ntotal, D = D, losses = lossMat, gains = gainMat, alpha = alpha[1], W = W[1]))
dat2 <- dump.format(list(T=Ntotal, D = D, losses = lossMat, gains = gainMat, alpha = alpha[2], W = W[2]))
dat3 <- dump.format(list(T=Ntotal, D = D, losses = lossMat, gains = gainMat, alpha = alpha[3], W = W[3]))


# Tell JAGS what to monitor in the simulations ----
monitor = c("choices","q")

# Run the sims with JAGS using the model block
#   in this case "sample" is the number of unique simulations
# agent 1
out1 <- run.jags(model="models/simulate_IGT.txt",
                monitor=monitor, data=dat1,
                burnin =1, sample=1, thin = 1, n.chains=1, summarise=FALSE)
simMat1 = out1$mcmc[[1]]
sims1 = as.vector(simMat1[1,1:300]) #take one iteration of the simulations
Qs1 = c(simMat1[1,601],simMat1[1,902],simMat1[1,1203],simMat1[1,1504])

#agent 2
out2 <- run.jags(model="models/simulate_IGT.txt",
                 monitor=monitor, data=dat2,
                 burnin =1, sample=1, thin = 1, n.chains=1, summarise=FALSE)
simMat2 = out2$mcmc[[1]]
sims2 = as.vector(simMat2[1,1:300]) #take one iteration of the simulations
Qs2 = c(simMat2[1,601],simMat2[1,902],simMat2[1,1203],simMat2[1,1504])

#agent 3
out3 <- run.jags(model="models/simulate_IGT.txt",
                 monitor=monitor, data=dat3,
                 burnin =1, sample=1, thin = 1, n.chains=1, summarise=FALSE)
simMat3 = out3$mcmc[[1]]
sims3 = as.vector(simMat3[1,1:300]) #take one iteration of the simulations
Qs3 = c(simMat3[1,601],simMat3[1,902],simMat3[1,1203],simMat3[1,1504])

# count the times each deck was sampled
barplot(table(sims1))
barplot(table(sims2))
barplot(table(sims3))
