library(markovchain)
statesNames=c("healthy","sick")
transitionMatrix <- matrix(c(0.7,0.3,0.1,0.9),byrow=TRUE, nrow=2, 
                           dimnames=list(statesNames,statesNames))
mc<-new("markovchain", transitionMatrix=transitionMatrix)
mc^2
steadyStates(mc)
absorbingStates(mc)

# Three state markov chain.
statesNames=c("healthy", "dead", "sick")
transitionMatrix <- matrix(
  c(0.6,0.05,0.35,
    0  ,1  ,0  ,
    0.4,0.05,0.55),
  byrow=TRUE, nrow=3, 
  dimnames=list(statesNames,statesNames))
mc2<-new("markovchain", transitionMatrix=transitionMatrix)
outs <- rmarkovchain(n = 10, object = mc2, what = "list", t0="healthy");outs
steadyStates(mc2)
absorbingStates(mc2)
# plot the timeline
series <- replace(outs, outs=="healthy",3)
series <- replace(series, series=="sick",2)
series <- replace(series, series=="dead",1)
series <- as.numeric(series)
plot(series,type='l')
