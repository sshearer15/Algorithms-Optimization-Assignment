install.packages("Synth")

library(Synth)
data("basque")

#original: Basque v. synthetic
dataprep.out <-
  dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),                            
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier  = 17
    ,controls.identifier   = c(2:16,18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr     = c(1960:1969)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997) 
  )
dataprep.out$X1["school.high",] <- 
  dataprep.out$X1["school.high",] + 
  dataprep.out$X1["school.post.high",]
dataprep.out$X1                 <- 
  as.matrix(dataprep.out$X1[
    -which(rownames(dataprep.out$X1)=="school.post.high"),])
dataprep.out$X0["school.high",] <- 
  dataprep.out$X0["school.high",] + 
  dataprep.out$X0["school.post.high",]
dataprep.out$X0                 <- 
  dataprep.out$X0[
    -which(rownames(dataprep.out$X0)=="school.post.high"),]

lowest  <- which(rownames(dataprep.out$X0)=="school.illit")
highest <- which(rownames(dataprep.out$X0)=="school.high")

dataprep.out$X1[lowest:highest,] <- 
  (100 * dataprep.out$X1[lowest:highest,]) /
  sum(dataprep.out$X1[lowest:highest,])
dataprep.out$X0[lowest:highest,] <-  
  100 * scale(dataprep.out$X0[lowest:highest,],
              center=FALSE,
              scale=colSums(dataprep.out$X0[lowest:highest,])
  )

synth.out <- synth(data.prep.obj = dataprep.out)
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
) 
print(synth.tables)

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Basque country","synthetic Basque country"),
)
abline(v=1970, col="red",lwd=2)

gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out, 
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(-1.5,1.5), 
)
abline(v=1970, col="red",lwd=2)

#selecting Andalucia v. synthetic (no Basque) --> this is a very good synthetic for Andalucia
#suggests possible spillover effects from Basque?
dataprep.out1 <-
  dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),                            
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier  = 2
    ,controls.identifier   = c(3:16,18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr     = c(1960:1969)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997) 
  )
dataprep.out1$X1["school.high",] <- 
  dataprep.out1$X1["school.high",] + 
  dataprep.out1$X1["school.post.high",]
dataprep.out1$X1                 <- 
  as.matrix(dataprep.out1$X1[
    -which(rownames(dataprep.out1$X1)=="school.post.high"),])
dataprep.out1$X0["school.high",] <- 
  dataprep.out1$X0["school.high",] + 
  dataprep.out1$X0["school.post.high",]
dataprep.out1$X0                 <- 
  dataprep.out1$X0[
    -which(rownames(dataprep.out1$X0)=="school.post.high"),]

lowest  <- which(rownames(dataprep.out1$X0)=="school.illit")
highest <- which(rownames(dataprep.out1$X0)=="school.high")

dataprep.out1$X1[lowest:highest,] <- 
  (100 * dataprep.out1$X1[lowest:highest,]) /
  sum(dataprep.out1$X1[lowest:highest,])
dataprep.out1$X0[lowest:highest,] <-  
  100 * scale(dataprep.out1$X0[lowest:highest,],
              center=FALSE,
              scale=colSums(dataprep.out1$X0[lowest:highest,])
  )

synth.out1 <- synth(data.prep.obj = dataprep.out1)
synth.tables1 <- synth.tab(
  dataprep.res = dataprep.out1,
  synth.res = synth.out1
) 
print(synth.tables1)

path.plot(synth.res = synth.out1,
          dataprep.res = dataprep.out1,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Andalucia","synthetic country (not including Basque)"),
)
abline(v=1970, col="red",lwd=2)

gaps.plot(synth.res = synth.out1,
          dataprep.res = dataprep.out1, 
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(-1.5,1.5), 
)
abline(v=1970, col="red",lwd=2)


#Aragon v. synthetic (including Basque)
##Aragon was the name of my high school - go dons!
dataprep.out2 <-
  dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),                            
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier  = 3
    ,controls.identifier   = c(2,4:18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr     = c(1960:1969)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997) 
  )
dataprep.out2$X1["school.high",] <- 
  dataprep.out2$X1["school.high",] + 
  dataprep.out2$X1["school.post.high",]
dataprep.out2$X1                 <- 
  as.matrix(dataprep.out2$X1[
    -which(rownames(dataprep.out2$X1)=="school.post.high"),])
dataprep.out2$X0["school.high",] <- 
  dataprep.out2$X0["school.high",] + 
  dataprep.out2$X0["school.post.high",]
dataprep.out2$X0                 <- 
  dataprep.out2$X0[
    -which(rownames(dataprep.out2$X0)=="school.post.high"),]

lowest  <- which(rownames(dataprep.out2$X0)=="school.illit")
highest <- which(rownames(dataprep.out2$X0)=="school.high")

dataprep.out2$X1[lowest:highest,] <- 
  (100 * dataprep.out2$X1[lowest:highest,]) /
  sum(dataprep.out2$X1[lowest:highest,])
dataprep.out2$X0[lowest:highest,] <-  
  100 * scale(dataprep.out2$X0[lowest:highest,],
              center=FALSE,
              scale=colSums(dataprep.out2$X0[lowest:highest,])
  )

synth.out2 <- synth(data.prep.obj = dataprep.out2)
synth.tables2 <- synth.tab(
  dataprep.res = dataprep.out2,
  synth.res = synth.out2
) 
print(synth.tables2)

path.plot(synth.res = synth.out2,
          dataprep.res = dataprep.out2,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Aragon","synthetic country (including Basque)"),
)
abline(v=1970, col="red",lwd=2)

gaps.plot(synth.res = synth.out2,
          dataprep.res = dataprep.out2, 
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(-1.5,1.5), 
)
abline(v=1970, col="red",lwd=2)

#Aragon v. synthetic (NOT including Basque)
#Compared to the above synthetic country, which included Basque, we can see 
##that the synthetic control much more closely mirrors the real Aragon state.   
###and thus conclude that Basque would have biased the synthetic control even more if included in model.
dataprep.out3 <-
  dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),                            
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier  = 3
    ,controls.identifier   = c(2,4:16,18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr     = c(1960:1969)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997) 
  )
dataprep.out3$X1["school.high",] <- 
  dataprep.out3$X1["school.high",] + 
  dataprep.out3$X1["school.post.high",]
dataprep.out3$X1                 <- 
  as.matrix(dataprep.out3$X1[
    -which(rownames(dataprep.out3$X1)=="school.post.high"),])
dataprep.out3$X0["school.high",] <- 
  dataprep.out3$X0["school.high",] + 
  dataprep.out3$X0["school.post.high",]
dataprep.out3$X0                 <- 
  dataprep.out3$X0[
    -which(rownames(dataprep.out3$X0)=="school.post.high"),]

lowest  <- which(rownames(dataprep.out3$X0)=="school.illit")
highest <- which(rownames(dataprep.out3$X0)=="school.high")

dataprep.out3$X1[lowest:highest,] <- 
  (100 * dataprep.out3$X1[lowest:highest,]) /
  sum(dataprep.out3$X1[lowest:highest,])
dataprep.out3$X0[lowest:highest,] <-  
  100 * scale(dataprep.out3$X0[lowest:highest,],
              center=FALSE,
              scale=colSums(dataprep.out3$X0[lowest:highest,])
  )

synth.out3 <- synth(data.prep.obj = dataprep.out3)
synth.tables3 <- synth.tab(
  dataprep.res = dataprep.out3,
  synth.res = synth.out3
) 
print(synth.tables3)

path.plot(synth.res = synth.out3,
          dataprep.res = dataprep.out3,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Aragon","synthetic country (not including Basque)"),
)
abline(v=1970, col="red",lwd=2)

gaps.plot(synth.res = synth.out3,
          dataprep.res = dataprep.out3, 
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(-1.5,1.5), 
)
abline(v=1970, col="red",lwd=2)






#Castilla Y Leon v. Synthetic (including Basque)
## synthetic with Basque is very similar to actual state; possible spillover effects
### of that this state had similar internal shock to that which Basque experienced
dataprep.out4 <-
  dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),                            
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier  = 8
    ,controls.identifier   = c(2:7,9:18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr     = c(1960:1969)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997) 
  )
dataprep.out4$X1["school.high",] <- 
  dataprep.out4$X1["school.high",] + 
  dataprep.out4$X1["school.post.high",]
dataprep.out4$X1                 <- 
  as.matrix(dataprep.out4$X1[
    -which(rownames(dataprep.out4$X1)=="school.post.high"),])
dataprep.out4$X0["school.high",] <- 
  dataprep.out4$X0["school.high",] + 
  dataprep.out4$X0["school.post.high",]
dataprep.out4$X0                 <- 
  dataprep.out4$X0[
    -which(rownames(dataprep.out4$X0)=="school.post.high"),]

lowest  <- which(rownames(dataprep.out4$X0)=="school.illit")
highest <- which(rownames(dataprep.out4$X0)=="school.high")

dataprep.out4$X1[lowest:highest,] <- 
  (100 * dataprep.out4$X1[lowest:highest,]) /
  sum(dataprep.out4$X1[lowest:highest,])
dataprep.out4$X0[lowest:highest,] <-  
  100 * scale(dataprep.out4$X0[lowest:highest,],
              center=FALSE,
              scale=colSums(dataprep.out4$X0[lowest:highest,])
  )

synth.out4 <- synth(data.prep.obj = dataprep.out4)
synth.tables4 <- synth.tab(
  dataprep.res = dataprep.out4,
  synth.res = synth.out4
) 
print(synth.tables4)

path.plot(synth.res = synth.out4,
          dataprep.res = dataprep.out4,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Castilla Y Leon","synthetic country (including Basque)"),
)
abline(v=1970, col="red",lwd=2)

gaps.plot(synth.res = synth.out4,
          dataprep.res = dataprep.out4, 
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(-1.5,1.5), 
)
abline(v=1970, col="red",lwd=2)


# Castilla Y Leon v. Synthetic w/out basque
## Comparing the plots, its clear that removing basque made the synthetic country model
### worse, but only slightly. Not nearly as drastic as the change when comparing the with Basque/without Basque synthetics to Aragon
dataprep.out5 <-
  dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),                            
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier  = 8
    ,controls.identifier   = c(2:7,9:16,18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr     = c(1960:1969)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997) 
  )
dataprep.out5$X1["school.high",] <- 
  dataprep.out5$X1["school.high",] + 
  dataprep.out5$X1["school.post.high",]
dataprep.out5$X1                 <- 
  as.matrix(dataprep.out5$X1[
    -which(rownames(dataprep.out5$X1)=="school.post.high"),])
dataprep.out5$X0["school.high",] <- 
  dataprep.out5$X0["school.high",] + 
  dataprep.out5$X0["school.post.high",]
dataprep.out5$X0                 <- 
  dataprep.out5$X0[
    -which(rownames(dataprep.out5$X0)=="school.post.high"),]

lowest  <- which(rownames(dataprep.out5$X0)=="school.illit")
highest <- which(rownames(dataprep.out5$X0)=="school.high")

dataprep.out5$X1[lowest:highest,] <- 
  (100 * dataprep.out5$X1[lowest:highest,]) /
  sum(dataprep.out5$X1[lowest:highest,])
dataprep.out5$X0[lowest:highest,] <-  
  100 * scale(dataprep.out5$X0[lowest:highest,],
              center=FALSE,
              scale=colSums(dataprep.out5$X0[lowest:highest,])
  )

synth.out5 <- synth(data.prep.obj = dataprep.out5)
synth.tables5 <- synth.tab(
  dataprep.res = dataprep.out5,
  synth.res = synth.out5
) 
print(synth.tables5)

path.plot(synth.res = synth.out5,
          dataprep.res = dataprep.out5,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Castilla Y Leon","synthetic country (not including Basque)"),
)
abline(v=1970, col="red",lwd=2)

gaps.plot(synth.res = synth.out5,
          dataprep.res = dataprep.out5, 
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(-1.5,1.5), 
)
abline(v=1970, col="red",lwd=2)

#Madrid v. synthetic with Basque
dataprep.out6 <-dataprep(
  foo = basque
  ,predictors= c("school.illit",
                 "school.prim",
                 "school.med",
                 "school.high",
                 "school.post.high"
                 ,"invest"
  )
  ,predictors.op = c("mean")
  ,dependent     = c("gdpcap")
  ,unit.variable = c("regionno")
  ,time.variable = c("year")
  ,special.predictors = list(
    list("gdpcap",1960:1969,c("mean")),                            
    list("sec.agriculture",seq(1961,1969,2),c("mean")),
    list("sec.energy",seq(1961,1969,2),c("mean")),
    list("sec.industry",seq(1961,1969,2),c("mean")),
    list("sec.construction",seq(1961,1969,2),c("mean")),
    list("sec.services.venta",seq(1961,1969,2),c("mean")),
    list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
    list("popdens",1969,c("mean")))
  ,treatment.identifier  = 14
  ,controls.identifier   = c(2:13,15:18)
  ,time.predictors.prior = c(1964:1969)
  ,time.optimize.ssr     = c(1960:1969)
  ,unit.names.variable   = c("regionname")
  ,time.plot            = c(1955:1997) 
)
dataprep.out6$X1["school.high",] <- 
  dataprep.out6$X1["school.high",] + 
  dataprep.out6$X1["school.post.high",]
dataprep.out6$X1                 <- 
  as.matrix(dataprep.out6$X1[
    -which(rownames(dataprep.out6$X1)=="school.post.high"),])
dataprep.out6$X0["school.high",] <- 
  dataprep.out6$X0["school.high",] + 
  dataprep.out6$X0["school.post.high",]
dataprep.out6$X0                 <- 
  dataprep.out6$X0[
    -which(rownames(dataprep.out6$X0)=="school.post.high"),]

lowest  <- which(rownames(dataprep.out6$X0)=="school.illit")
highest <- which(rownames(dataprep.out6$X0)=="school.high")

dataprep.out6$X1[lowest:highest,] <- 
  (100 * dataprep.out6$X1[lowest:highest,]) /
  sum(dataprep.out6$X1[lowest:highest,])
dataprep.out6$X0[lowest:highest,] <-  
  100 * scale(dataprep.out6$X0[lowest:highest,],
              center=FALSE,
              scale=colSums(dataprep.out6$X0[lowest:highest,])
  )

synth.out6 <- synth(data.prep.obj = dataprep.out6)
synth.tables6 <- synth.tab(
  dataprep.res = dataprep.out6,
  synth.res = synth.out6
) 
print(synth.tables6)

path.plot(synth.res = synth.out6,
          dataprep.res = dataprep.out6,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Madrid","synthetic country including Basque)"),
)
abline(v=1970, col="red",lwd=2)

gaps.plot(synth.res = synth.out6,
          dataprep.res = dataprep.out6, 
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(-1.5,1.5), 
)
abline(v=1970, col="red",lwd=2)

##Madrid v. synthetic without Baque
dataprep.out7 <-dataprep(
  foo = basque
  ,predictors= c("school.illit",
                 "school.prim",
                 "school.med",
                 "school.high",
                 "school.post.high"
                 ,"invest"
  )
  ,predictors.op = c("mean")
  ,dependent     = c("gdpcap")
  ,unit.variable = c("regionno")
  ,time.variable = c("year")
  ,special.predictors = list(
    list("gdpcap",1960:1969,c("mean")),                            
    list("sec.agriculture",seq(1961,1969,2),c("mean")),
    list("sec.energy",seq(1961,1969,2),c("mean")),
    list("sec.industry",seq(1961,1969,2),c("mean")),
    list("sec.construction",seq(1961,1969,2),c("mean")),
    list("sec.services.venta",seq(1961,1969,2),c("mean")),
    list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
    list("popdens",1969,c("mean")))
  ,treatment.identifier  = 14
  ,controls.identifier   = c(2:13,15:16,18)
  ,time.predictors.prior = c(1964:1969)
  ,time.optimize.ssr     = c(1960:1969)
  ,unit.names.variable   = c("regionname")
  ,time.plot            = c(1955:1997) 
)
dataprep.out7$X1["school.high",] <- 
  dataprep.out7$X1["school.high",] + 
  dataprep.out7$X1["school.post.high",]
dataprep.out7$X1                 <- 
  as.matrix(dataprep.out7$X1[
    -which(rownames(dataprep.out7$X1)=="school.post.high"),])
dataprep.out7$X0["school.high",] <- 
  dataprep.out7$X0["school.high",] + 
  dataprep.out7$X0["school.post.high",]
dataprep.out7$X0                 <- 
  dataprep.out7$X0[
    -which(rownames(dataprep.out7$X0)=="school.post.high"),]

lowest  <- which(rownames(dataprep.out7$X0)=="school.illit")
highest <- which(rownames(dataprep.out7$X0)=="school.high")

dataprep.out7$X1[lowest:highest,] <- 
  (100 * dataprep.out7$X1[lowest:highest,]) /
  sum(dataprep.out7$X1[lowest:highest,])
dataprep.out7$X0[lowest:highest,] <-  
  100 * scale(dataprep.out7$X0[lowest:highest,],
              center=FALSE,
              scale=colSums(dataprep.out7$X0[lowest:highest,])
  )

synth.out7 <- synth(data.prep.obj = dataprep.out7)
synth.tables7 <- synth.tab(
  dataprep.res = dataprep.out7,
  synth.res = synth.out7
) 
print(synth.tables7)

path.plot(synth.res = synth.out7,
          dataprep.res = dataprep.out7,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Madrid","synthetic country without Basque)"),
)
abline(v=1970, col="red",lwd=2)

gaps.plot(synth.res = synth.out7,
          dataprep.res = dataprep.out7, 
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(-1.5,1.5), 
)
abline(v=1970, col="red",lwd=2)
