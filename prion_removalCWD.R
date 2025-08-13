prion.removal = function(landscape.prions, prion.lifespan){
  landscape.prions.out = landscape.prions

  if(sum(landscape.prions.out[,3])>0){
  P.index = which(landscape.prions.out[,3]>0)
  
  for(i in 1:length(P.index)){
    p.lifespan.now = rgamma(1, shape = prion.lifespan, rate = 1)
    P.loss.rand = 1/p.lifespan.now
    
    landscape.prions.out[P.index[i], 3] = landscape.prions.out[P.index[i], 3]*exp(-P.loss.rand)
  }
  }
  
  return(landscape.prions.out)
}