##########
## @description          simple simulation of CLT 
## @n                    sample size 
## @M                    number of samples
## @dist                 data generating function 
##                       unif, beta, exp, norm
## @par1                 parameter 1 
## @par2.                parameter 2 
###########
## @return               outputs the true pop mean and standard deviation 
##                       and the sampling mean and standard deviation  
## @plot                 plots histogram of sampling distribution 
###########


demonstrate.clt <- function(n,M,dist,par1,par2=NULL){   
  if(missing(dist)){
    stop("argument 'dist' must be provided; input 'unif, beta, exp, or norm'")
  }
  if((dist == "unif" | dist == "beta" | dist == "norm") & (missing(par1) | missing(par2))){
    stop("parameter arguments must be provided")
  }
  if(missing(n) | ((n < 10 | n > 1000) | n != round(n))){
    n <- 100 
    cat("'n' must be an integer between 10 and 1000; using default 'n = 100'")
  }
  if(missing(M) | ((M < 100 | M > 1e06) | M != round(M))){
    M <- 1000
    cat("'M' must be an integer between 100 and 1e06; using default 'M = 1000'")
  }
  if((dist == "unif") & (length(c(par1,par2)) != 2)){
    stop("'unif' must have two parameters")
  }
  if((dist == "unif") & (par1 > par2)){
    stop("'unif' parameter 'a' cannot be larger than parameter 'b'")
  }
  if((dist == "exp") & (length(par) != 1 | par1 <= 0)){
    stop("'exp' must have one postive parameter")
  }
  if((dist == "beta") & (length(c(par1,par2)) != 2 | any(c(par1,par2)<= 0))){
    stop("'beta' must have two postive parameters")
  }
  if((dist == "norm") & (length(c(par1,par2)) != 2)){
    stop("'norm' must have two parameters")
  }
# body of code ------------------------------------------------------------------------
  if(dist == "unif"){
    data <- matrix(runif(n*M, par1, par2), nrow = n, ncol = M) 
    true.mean <- (par1 + par2)/2                    
    true.sd <- sqrt(((par1-par2)^2)/(12*n))
  }
  if(dist == "exp"){
    data <- matrix(rexp(n*M, par1), nrow = n, ncol = M) 
    true.mean <- 1/par1
    true.sd <- (1/par1)/sqrt(n)
  }
  if(dist == "beta"){
    data <- matrix(rbeta(n*M, par1, par2), nrow = n, ncol = M)
    true.mean <- par1/(par1+par2)
    true.sd <- sqrt((par1*par2)/(((par1+par2)^2)*(par1+par2+1)*n))
  }
  if(dist == "norm"){
    data <- matrix(rnorm(n*M, par1, par2), nrow = n, ncol = M)
    true.mean <- par1 
    true.sd <- par2
  }
  
  xbar <- colMeans(data)
  dist.xbar <- hist(xbar, 25)
  
  cat("The true mean of the sampling distribution of the sample mean is", true.mean, 
      "\n The true standard deviation of the sampling distribution of the sample mean is", true.sd)
  
  cat("\nThe estimated mean of the sampling distribution of the sample mean is", mean(xbar), 
      "\n The estimated standard deviation of the sampling distribution of the sample mean is", sd(xbar))
} 
