# Load the required libraries
library(runjags)

# the HDI function (Highest Density Interval function)
HDIofMCMC = function(sampleVec, credMass = 0.95) {
  sortedPts = sort(sampleVec)
  ciIdxInc = ceiling(credMass * length(sampleVec))
  nCIs = length(sampleVec) - ciIdxInc
  ciWidth = rep(0,nCIs)
  for (i in 1:nCIs) {
    ciWidth[i] = sortedPts[i+ciIdxInc] - sortedPts[i]
  }
  HDImin = sortedPts[which.min(ciWidth)]
  HDImax = sortedPts[which.min(ciWidth) + ciIdxInc]
  HDI = c(HDImin, HDImax)
  return(HDI)
}

# Original Data------------------------------------------------------------------------------- 
## Generate some random data
Ntotal = 18
x = seq(-5,5,by=0.001)
x = sample(x,Ntotal)
y = (3 + x) + rnorm(Ntotal,0,6)
## Add potential outlier
x = c(x,4,4.2)
y = c(y,35,36)

## plot the data
plot(x,y,lwd=2)

# Prepare Data-------------------------------------------------------------------------------
## prepare the data for JAGS
dat = dump.format(list(y=y,x=x,Ntotal=Ntotal))

# Define JAGS model-------------------------------------------------------------------------------
linear_regression_model = "
model {
  beta0 ~ dnorm(0,0.00001) # prior intercept
  beta1 ~ dnorm(0,0.00001) # prior slope
  sigma ~ dunif(0.00001,100) # prior for the standard deviation
  
  for (i in 1:Ntotal) {
    y[i] ~ dnorm(mu[i], 1/sigma^2)
    mu[i] <- beta0 + beta1 * x[i]
  }
}
"

writeLines( linear_regression_model, con = "models/linearRegr.txt")

# Initialize chains-------------------------------------------------------------------------------
inits1 = dump.format(list(beta0 = 0, beta1 = 0, sigma = 3, .RNG.name="base::Super-Duper", .RNG.seed=99999))
inits2 = dump.format(list(beta0 = 1, beta1 = 1, sigma = 2, .RNG.name="base::Wichmann-Hill", .RNG.seed=12345))
inits3 = dump.format(list(beta0 = 2, beta1 = 2, sigma = 4, .RNG.name="base::Mersenne-Twister", .RNG.seed=1802))
## Tell JAGS which latent variables to monitor
monitor = c("beta0", "beta1", "sigma")

# Run JAGS-------------------------------------------------------------------------------
results <- run.jags(model = "models/linearRegr.txt",
                    monitor = monitor,
                    data = dat,
                    n.chains = 3,
                    inits = c(inits1,inits2,inits3),
                    plots = FALSE,
                    burnin = 4000,
                    sample = 5000,
                    thin = 5)

## read the summary of the results
results

# Plot Chains-------------------------------------------------------------------------------
plot(results$mcmc)

# readout the 3 chains from the "results" structure and combine them into a single matrix
# each of the resulting matrix represent a single MCMC sample, the columns represent the monitored variables
chains = rbind(results$mcmc[[1]],results$mcmc[[2]],results$mcmc[[3]])

## store the estimates in vector variables for easier manipulation (see below)
beta0.est = chains[,"beta0"]
beta1.est = chains[,"beta1"]
sigma.est = chains[,"sigma"]

# Plot the independent empirical density of parameters
plot(density(beta0.est), xlim = c(-2,6), lwd = 3)

# Return the HDI of the MCMC of interest
hdi = HDIofMCMC(beta0.est, credMass = 0.9)

# Plot the median of the MCMC of interest
abline(v = median(beta0.est), col = "red", lwd = 2)

# add HDI limits of the MCMC of interest
abline(v = hdi[1], col = "red", lwd = 2)
abline(v = hdi[2], col = "red", lwd = 2)

# compare the HDI limits to 0
abline(v = 0, col = "grey", lwd = 2)

#Plot data and predictions
plot(x,y,lwd = 2)
beta0.median = median(beta0.est)
beta1.median = median(beta1.est)
abline(a = beta0.median, b = beta1.median, col="red", lwd = 2)
  
  
  
  
  
  
  
  
  
  
  













