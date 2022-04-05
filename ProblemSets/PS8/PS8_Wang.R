library(nloptr)
set.seed(100)

N=100000
k=10
X<-matrix(rnorm(N*k),N,k)
X[,1]<-1
eps<-rnorm(N,0,0.5)
beta <- as.vector(c(1.5,-1,-0.25,0.75,3.5,-2,0.5,1,1.25,2))
Y<-X%*%beta+eps
betahat<-solve(crossprod(X))%*%t(X)%*%Y


# Using gradient decent

alpha <- 0.0000003
# set up a number of iterations
maxiter <- 500000
# Our objective function
objfun <- function(beta,Y,X) {
  return ( sum((Y-X%*%beta)^2) )
}
# define the gradient of our objective function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),maxiter)
# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}
# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))


# Using nloptr
# Our objective function
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-X%*%beta) )
}
# Gradient of our objective function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

# initial values
beta0 <- runif(k) #start at uniform random numbers equal to number of coefficients
# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)

# Nelder-Mead
# Our objective function
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
}
# initial values
set.seed(100)
betastart <- runif(k)
# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6)
# Find the optimum!
res <- nloptr( x0=betastart,eval_f=objfun,Y=Y, X=X,opts=options)
print(res)


# MLE
# Our objective function
objfun  <- function(theta,Y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
# Gradient of our objective function
gradient <- function (theta ,Y,X) {
  grad <- as.vector (rep (0, length(theta)))
  beta <- theta [1:(length (theta) -1)]
  sig <- theta[length (theta)]
  grad [1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta)/(sig ^2)
  grad [length (theta)] <- dim(X)[1]/sig - crossprod (Y-X%*%beta)/(sig^3)
  return (grad)
}
# initial values
theta0 <- runif(k+1) #start at uniform random numbers equal to number of coefficients
theta0 <- append(betahat,runif(1))
# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)

#lm()

ylm<-lm(Y ~ X-1)
summary(ylm)

library(modelsummary)
modelsummary(ylm,output = "latex")
