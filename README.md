# Algorithms-Optimization-Assignment

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
