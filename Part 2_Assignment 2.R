library("Matching")
library("rgenoud")
library("ggplot2")
data(lalonde)


min.reg <- function(params){
  B0 <- params[1]
  B1 <- params[2]
  mycost <- sum( (lalonde$re78 - (B0 + B1*lalonde$treat))^2 )
  return (mycost)
}

#Because I have not defined starting values below, genound will randomly choose each time run.
##To test whether parameters are sensitive to starting values, I just re-ran equation several times.
myfit <- genoud(min.reg,nvars = 2)
cat('Using genoud - B0:', myfit$par[1], ' B1:', myfit$par[2], '\n')

##Just in case, checked specific starting values --> same answer
myfit1 <- genoud(min.reg,nvars = 2, starting.values = c(2000, 2000))
cat('Using genoud - B0:', myfit1$par[1], ' B1:', myfit1$par[2], '\n')

#Comparing ansewer to regression: yeilds same result, confirming the parameters
##genound yielded
mylm <- lm(re78~treat, data = lalonde)
cat('Using lm - B0:', mylm$coefficients[1], ' B1:', mylm$coefficients[2], '\n')

##Robust regression fitness functions
min.reg2 <- function(params){
  B0 <- params[1]
  B1 <- params[2]
  mycost <- median( (lalonde$re78 - (B0 + B1*lalonde$treat))^2 )
  return (mycost)
}


#this is sensitive to starting values; by leaving this argument undefined in genound, 
## the function picks random values and yeilds a different solution each time!
myfit2 <- genoud(min.reg2,nvars = 2,)
cat('Using genoud for robust - B0:', myfit2$par[1], ' B1:', myfit2$par[2], '\n')

##tested again by inputting the starting values of the known solution from the logistic regression
### using lm.
myfit2 <- genoud(min.reg2,nvars = 2, starting.values = mylm$coefficients)
cat('Using genoud for robust - B0:', myfit2$par[1], ' B1:', myfit2$par[2], '\n')



ggplot(lalonde, aes(x=treat, y=re78)) + geom_point() + 
  scale_x_discrete(name="Experimental Group", limits=c(0, 1)) +
  geom_abline(intercept = coef(mylm)[1], slope = coef(mylm)[2], color = 'blue') +
  geom_abline(intercept = myfit2$par[1], slope = myfit2$par[2], color = 'red') +
  labs(x = "Experimental Group", y = "Real 1978 Earnings") + 
  annotate("text", x = 1.5, y = 15000, label = c("Least Sq./Simple Regression"), colour = "blue") +
  annotate("text", x = 0.5, y = 15000, label = c("Robust Regression"), colour = "red")
  

