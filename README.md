# Algorithms-Optimization-Assignment

###Part One

library("ggplot2")
set.seed(123)
error_term <- rnorm(201)
xxx <- c(-100:100)/100
yyy_probs <- exp(5*xxx + 0.5 + error_term)/(1 + exp(5*xxx + 0.5 + error_term))

#plot function

plot(xxx, yyy_probs, type='l') 

#setting up function that minimizes 

min.reg <- function(params,yyy_probs,xxx) {
  B0 <- params[1]
  B1 <- params[2]
  mycost <- mean((yyy_probs - exp(B0 + B1*xxx) / (1 + exp(B0 + B1*xxx)))^2)
  return (mycost)
}

#estimate two parameters (“B0” and “B1”) using a logistic model 

optim(par=c(0,0.25), fn=min.reg, x=xxx, y=yyy_probs)

#change initial paraments to see if effects “B0” and “B1” values 

optim(par=c(-1,-0.5), fn=min.reg, x=xxx, y=yyy_probs)

optim(par=c(0,0.5), fn=min.reg, x=xxx, y=yyy_probs)

optim(par=c(0,0.25), fn=min.reg, x=xxx, y=yyy_probs)

optim(par=c(0.5, 1), fn=min.reg, x=xxx, y=yyy_probs)


#compare to linear regression model, just for fun
#wow! different values!

lm1 <- lm(yyy_probs~xxx)


###Part Two

library("Matching")
library("rgenoud")
library("ggplot2")
data(lalonde)

#minimize the sum of the squared residuals 
min.reg <- function(params){
  B0 <- params[1]
  B1 <- params[2]
  mycost <- sum( (lalonde$re78 - (B0 + B1*lalonde$treat))^2 )
  return (mycost)
}

#tested other starting values besides that chosen below; outcome not sensitive to change

myfit <- genoud(min.reg,nvars = 2, starting.values = c(2000, 2000))
cat('Using genoud - B0:', myfit$par[1], ' B1:', myfit$par[2], '\n')

#compare v. simple regression: yields same parameters!

mylm <- lm(re78~treat, data = lalonde)
cat('Using lm - B0:', mylm$coefficients[1], ' B1:', mylm$coefficients[2], '\n')


#median squared residual/robust residual regression

min.reg2 <- function(params){
  B0 <- params[1]
  B1 <- params[2]
  mycost <- median( (lalonde$re78 - (B0 + B1*lalonde$treat))^2 )
  return (mycost)
}

#this is sensitive to starting values; by leaving start value argument undefined in genound, 
## the function picks random values and yeilds a different solution each time!

myfit2 <- genoud(min.reg2,nvars = 2)
cat('Using genoud for robust - B0:', myfit2$par[1], ' B1:', myfit2$par[2], '\n')
