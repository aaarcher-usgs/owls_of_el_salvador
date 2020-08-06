library(R2jags)

n.site <-150

humidity <- sort(runif(n = n.site, min = -1, max = 1))

alpha.occ <- 0
beta.occ <- 3

occ.prob <- exp(alpha.occ + beta.occ*humidity)/(1 + exp(alpha.occ + beta.occ*humidity))
plot(humidity, occ.prob)

true.presence <- rbinom(n = n.site, size = 1, prob = occ.prob)
table(true.presence)

# generate survey data

alpha.p <- 0
beta.p <- -5

det.prob <- exp(alpha.p + beta.p*humidity)/(1 + exp(alpha.p + beta.p*humidity))
plot(humidity, det.prob)

eff.det.prob <- true.presence*det.prob
plot(humidity, eff.det.prob)

R <- n.site
T <- 3
y <- array(dim=c(R,T))

for(ii in 1:T){
  y[,ii] <- rbinom(n = n.site, size = 1, prob = eff.det.prob)
}
sum(apply(y, 1, sum))


# Naive analysis
obs <- as.numeric(apply(y, 1, sum) > 0)
naive.analysis <- glm(obs ~ humidity, family = binomial)
summary(naive.analysis)
lin.pred <- naive.analysis$coefficients[1] + naive.analysis$coefficients[2]*humidity
plot(humidity, exp(lin.pred)/(1+exp(lin.pred)), ylim = c(0,1))


# Analysis in JAGS
model.occ <- function(){
  alpha.occ ~ dunif(-10,10)
  beta.occ ~ dunif(-10,10)
  alpha.p ~ dunif(-10,10)
  beta.p ~ dunif(-10,10)
  
  #alpha.occ ~ dnorm(0,0.01)
  #beta.occ ~ dnorm(0,0.01)
  #alpha.p ~ dnorm(0,0.01)
  #beta.p ~ dnorm(0,0.01)
  
  # Likelihood
  for(ii in 1:R){ # over sites
    z[ii] ~ dbern(psi[ii])
    logit.psi[ii] <- alpha.occ + beta.occ * humidity[ii]
    psi[ii] <- exp(logit.psi[ii])/(1 + exp(logit.psi[ii]))
    
    for(jj in 1:T){ # over returned surveys to those sites
      y[ii,jj] ~ dbern(eff.p[ii,jj])
      eff.p[ii,jj] <- z[ii]*p[ii,jj]
      logit.p[ii,jj] <- alpha.p + beta.p * humidity[ii]
      p[ii,jj] <- exp(logit.p[ii,jj])/(1 + exp(logit.p[ii,jj]))
      
      Presi[ii,jj] <- abs(y[ii,jj]-p[ii,jj])
      y.new[ii,jj] ~ dbern(eff.p[ii,jj])
      Presi.new[ii,jj] <- abs(y.new[ii,jj]-p[ii,jj])
    }
  }
  fit <- sum(Presi[,])
  fit.new <- sum(Presi.new[,])
  
  occ.fs <- sum(z[])
}

jags.data <- list(y = y,
                  humidity = humidity,
                  R = dim(y)[1],
                  T = dim(y)[2])

zst <- apply(y , 1, max)
inits <- function(){list(z = zst, alpha.occ = runif(1, -5, 5), beta.occ = runif(1,-5,5),
                         alpha.p = runif(1,-5,5), beta.p = runif(1,-5,5))}

params <- c("alpha.occ", "beta.occ", "alpha.p", "beta.p", "occ.fs", "fit", "fit.new")

nc <- 3
nb <- 2000
ni <- 12000
nt <- 5

out <- R2jags::jags(data = jags.data, 
                    inits = inits, 
                    parameters.to.save = params, 
                    model = model.occ,
                    n.chains = nc, 
                    n.iter = ni, 
                    n.burnin = nb, 
                    n.thin = nt)

# Inspection
plot(out$BU$sims.list$fit, out$BU$sims.list$fit.new)
abline(0, 1)

mean(out$BUGSoutput$sims.list$fit.new > out$BU$sims.list$fit)

alpha.occ
beta.occ
alpha.p
beta.p

sum(true.presence)

sum(apply(y, 1, sum))

out

plot(humidity, exp(lin.pred)/(1 + exp(lin.pred)), ylim = c(0,1))
points(humidity, occ.prob,type = "l")
lin.pred2 <- out$BU$mean$alpha.occ + out$BU$mean$beta.occ*humidity
points(humidity, exp(lin.pred2)/(1 + exp(lin.pred2)), type = "l", col = "blue")
