library(flexmix)

m <- as.matrix(rbind(
  c(0,0,1),
  c(1,1,0),
  c(1,1,0),
  c(1,1,0),
  c(1,1,0),
  c(1,1,0),
  c(1,1,0),
  c(1,1,0),
  c(1,1,0),
  c(0,0,1),
  c(0,0,1),
  c(0,0,1),
  c(0,0,1),
  c(0,0,1),
  c(0,0,1),
  c(0,0,1),
  c(1,0,1),
  c(1,1,0),
  c(0,0,0),
  c(0,1,0)
))

colnames(m) <- c("swimming","hiking","museum")
 


bernoulliMixture <- stepFlexmix(m~1, k=1:3, model = FLXMCmvbinary(), nrep=5, verbose=FALSE)
bernoulliMixture
best.Bernoulli <- getModel(bernoulliMixture)
plot(bernoulliMixture)
vac.m5 <- getModel(bernoulliMixture, "2")
propBarchart(m, clusters(vac.m5), alpha = 1, strip.prefix = "Segment ")
p <- round(parameters(best.Bernoulli),2)
p
