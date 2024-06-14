## Pert random number generation, inverse distribution, and density functions.  Uses the built-in beta function in R.
## By Francisco J. Zagmutt.  

## Pert distribution RNG

rpert = function(n, min=0, mode=0.5, max=1) {
  mu = (min+4*mode+max)/6
  if (mu==mode) {alpha = beta = 3} else {
    alpha = (mu-min)*(2*mode-min-max)/(mode-mu)/(max-min)
    beta  = alpha*(max-mu)/(mu-min)
    }
  min+(max-min)*rbeta(n,alpha,beta)
}

## PERT distribution inverse function

qpert = function(p, min=0, mode=0.5, max=1, lower.tail=T, log.p=F) {
  mu = (min+4*mode+max)/6
  if (mu==mode) alpha = beta = 3 else {
    alpha = (mu-min)*(2*mode-min-max)/(mode-mu)/(max-min)
    beta = alpha*(max-mu)/(mu-min)
    }
  min+(max-min)*qbeta(p,alpha,beta)
}

## PERT distribution density 

dpert = function(x, min=0, mode=0.5, max=1, lower.tail=T, log.p=F){
  mu = (min+4*mode+max)/6
  alpha=6*(mu-min)/(max-min)
  beta=6*(max-mu)/(max-min)
  num= (x-min)^(alpha-1)*(max-x)^(beta-1)
  den= beta(alpha,beta)*(max-min)^(alpha+beta-1)
  num/den
}

