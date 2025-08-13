extract_metrics = function(onerun, centroids){
  
  max.time = length(onerun[[1]])-1
  
  max.pop = max(onerun[[1]])
  max.pop.time = head(which(onerun[[1]] == max.pop), 1)
  
  max.I = max(onerun[[4]])
  max.I.time = head(which(onerun[[4]] == max.I), 1)
  
  last.I.time = max(which(onerun[[4]]>0))
  max.prev = max((onerun[[3]] + onerun[[4]])/onerun[[1]])
  
  total.I = sum(onerun[[11]])
  max.inc = max(onerun[[11]])
  time.max.inc = head(which(onerun[[11]] == max.inc), 1)
  
  max.spread = max(onerun[[9]][,3]) 
  max.number.I.cells = max(onerun[[9]][,1]) 
  max.spread.area = max(onerun[[9]][,2])
  
  final.prions = subset(onerun[[16]], time == max.time)
  final.avg.prion.density = mean(final.prions$prions)
  
  final.I = tail(onerun[[4]], 1)
  final.pop = tail(onerun[[1]], 1)

  final.spread = tail(onerun[[9]][,3], 1) 
  final.number.I.cells = tail(onerun[[9]][,1], 1) 
  final.spread.area = tail(onerun[[9]][,2], 1)
  final.max.sp.cent = tail(onerun[[9]][,4], 1) 
  
  final.E = tail(onerun[[3]], 1)
  final.I.cells = tail(onerun[[7]], 1)
  final.E.cells = tail(onerun[[6]],1)
  
  #prev. from surv. data and harvest
  final.prev = (final.I+final.E)/final.pop
  
  surv.data = onerun[[13]]
  if(is.null(surv.data) == TRUE){
    last.full.harv = NA
    last.harv.number = NA
    surv.data.numEI = NA
    final.surv.prev = NA
  }else{
  surv.data.agg = aggregate(surv.data, num ~ result + time, sum)
  
  harv.data = as.data.frame(onerun[[12]])
  harv.data.tall = gather(harv.data, key = "status", value = "num", S:I)
  
  harv.data.agg = aggregate(harv.data.tall, num ~ time, sum)
  
  harv.breaks = which(diff(harv.data.agg$time) > 1)
  last.full.harv = max(harv.breaks)
  last.harv.number = sum(harv.data.agg$num[(last.full.harv-3):last.full.harv])
  
  last.surv = max(surv.data.agg$time)
  surv.data.sub = subset(surv.data.agg, time == last.surv)
  surv.data.sub.EI = subset(surv.data.sub, result != "FP")
  surv.data.numEI = sum(surv.data.sub.EI$num)
  
  final.surv.prev = surv.data.numEI/(0.95*last.harv.number)
  }
  #prions
  prions.df = onerun[[16]]
  max.time = max(prions.df$time)
  prions.maxtime = subset(prions.df, time == max.time)
  prions.maxtime.infcells = subset(prions.maxtime, prions > 0)
  
  chull.prions = chull(prions.maxtime.infcells[,1:2])
  
  boundary = prions.maxtime.infcells[chull.prions,,drop = FALSE]
  boundary = rbind(boundary, boundary[1,])
  prions.A = abs(polyarea(boundary[,1], boundary[,2]))
  
  prop.inf.cells = dim(prions.maxtime.infcells)[1]/dim(prions.df)[1]
  
  prions.maxtime.ID50cells = subset(prions.maxtime, prions > 1.0)
  prop.ID50.cells = dim(prions.maxtime.ID50cells)[1]/dim(prions.df)[1]
  prop.ID50.infcells = dim(prions.maxtime.ID50cells)[1]/dim(prions.maxtime.infcells)[1]
  
  # if(dim(prions.maxtime.infcells)[1]>5){
  # fit.gamma.dist = fitdist(prions.maxtime.infcells$prions, distr = "gamma", method = "mle")
  # prions.gamma.shape = coef(fit.gamma.dist)[1]
  # prions.gamma.rate = coef(fit.gamma.dist)[2]
  # }else{
  prions.gamma.shape = NA
  prions.gamma.rate = NA
  # }
  
  #sharpshooting stats
  
  if(is.null(onerun[[14]])){ #if the df is NULL
    SS.S.prop = NA
    SS.E.prop = NA
    SS.I.prop = NA
  }else{
  if(dim(onerun[[14]])[1] == 0){ #if the df is empty which can happen if there were no individuals removed during any SS events
    SS.S.prop = NA
    SS.E.prop = NA
    SS.I.prop = NA
  }else{
      SS.data = as.data.frame(onerun[[14]])
      names(SS.data) = c("time", "rem.loc", "det.loc", "S", "E", "I")
      max.SS.time = max(SS.data$time)
      max.SS.time.seq = seq(max.SS.time - 2, max.SS.time, 1)
      SS.data.maxtime = subset(SS.data, time %in% max.SS.time.seq)
      
      SS.data.sums = colSums(SS.data.maxtime[,4:6])
      SS.maxtime.tot = sum(SS.data.sums)
      
      SS.S.prop = SS.data.sums[1]/SS.maxtime.tot
      SS.E.prop = SS.data.sums[2]/SS.maxtime.tot
      SS.I.prop = SS.data.sums[3]/SS.maxtime.tot
  }
    }
  
  #spread rate 
  spread = data.frame(onerun[[9]])
  
  names(spread) = c("num.inf.cells", "area", "maxdist", "max.dist.cent")

  del.area = NA
  del.dist = NA
  del.distcent = NA
  for(i in 1:(dim(spread)[1]-1)){
    del.area.now = spread$area[i+1] - spread$area[i]
    del.area = append(del.area, del.area.now)
    
    del.dist.now = spread$maxdist[i+1] - spread$maxdist[i]
    del.dist = append(del.dist, del.dist.now)
    
    del.distcent.now = spread$max.dist.cent[i+1] - spread$max.dist.cent[i]
    del.distcent = append(del.distcent , del.distcent.now)
  }
  
  spread.area.rate = mean(del.area, na.rm = TRUE)
  spread.dist.rate = mean(del.dist, na.rm = TRUE)
  spread.distcent.rate = mean(del.distcent, na.rm = TRUE)
  
  #characterize the traveling wave
  final.pop.df = onerun[[18]]
  
  midpoint = c(max(centroids[,1]/2),max(centroids[,2]/2))
  mid.cell = which(centroids[, 1] >= midpoint[1] & centroids[, 2] >= midpoint[2])[1] #location on grid closest to midpoint
  
  dist.cent = sqrt((centroids[final.pop.df[,3],1] - centroids[mid.cell,1])^2 + (centroids[final.pop.df[,3],2] - centroids[mid.cell,2])^2)
  
  final.pop.df = cbind(final.pop.df, dist.cent)
  
  
  E.cells = subset(final.pop.df, final.pop.df[, 9]>0)
  I.cells = subset(final.pop.df, final.pop.df[, 10]>0)
  
  if(dim(E.cells)[1]>1){
  mean.E.dist = mean(E.cells[, 13], na.rm = TRUE)
  std.E.dist = sqrt(var(E.cells[,13], na.rm = TRUE))}else{
    mean.E.dist = NA
    std.E.dist = NA
  }
  
  if(dim(I.cells)[1]>1){
    mean.I.dist = mean(I.cells[, 13], na.rm = TRUE)
    std.I.dist = sqrt(var(I.cells[,13], na.rm = TRUE))}else{
      mean.I.dist = NA
      std.I.dist = NA
    }
  
  #prev rate 
  prev = (onerun[[3]]+onerun[[4]])/onerun[[1]]
  
  del.prev = NA
  for(i in 1:(length(prev)-1)){
    del.prev.now = prev[i+1] - prev[i]
    
    del.prev = append(del.prev, del.prev.now)
  }
  
  mean.prev.rate = mean(del.prev, na.rm = TRUE)
  
  #calculate "local" prev.
  inf.fams.prev.mean = NA
  chull.fams.prev.mean = NA
  
  inf.fams.prev.I.mean = NA
  chull.fams.prev.I.mean = NA
  
  pop = onerun[[18]]
  pop.df = as.data.frame(pop)
  names(pop.df)[5:6] = c("x", "y") 
  fams.land.sf = st_as_sf(pop.df, coords = c("x", "y"), crs = 26910)
  
  infected = pop[pop[, 9]>0 | pop[, 10]>0, ,drop = FALSE]	
  
  #determine which cells infected
  infectcells = unique(infected[,3])
  xy = centroids[infectcells, , drop = FALSE]	

  chull.go = FALSE
  if(length(infectcells) > 2){ #to calc convex hull need 3 points
    if(length(unique(xy[,1]))>1 & length(unique(xy[,2]))>1){ #and the x and y coordinates must all be unique (i.e. they can't all lie on a line)      
      
      xy.test = matrix(0, nrow = nrow(xy)-1, ncol = ncol(xy))
      for(i in 2:nrow(xy)){
        xy.test[i-1,] = xy[i,] - xy[1,]
      }
      
      if(Rank(xy.test) != 1){chull.go = TRUE} #use the rank to determine if the rows are linearly independent, or that there exists a linear transformation from one point to another (i.e. that they don't form a diagonal line (Rank = 1))
      } 
  }
  
  if(chull.go == TRUE){
  #get all centroids xy coords
  xy = centroids[infectcells, , drop = FALSE]	
  
  xy = data.frame(xy)
  names(xy) = c("x", "y")
  xy.sf = st_as_sf(xy, coords = c("x", "y"), crs = 26910)
  xy.chull = st_convex_hull(st_union(xy.sf))
  xy.bchull = st_buffer(xy.chull, dist = 8)
  
  fams.in.chull = st_intersects(fams.land.sf, xy.bchull)
  fams.in.chull = lengths(fams.in.chull) > 0
  fams.in.chull = which(fams.in.chull == TRUE)
  fams.in.ch = pop[fams.in.chull, , drop = FALSE]
  
  inf.fams.prev = (infected[ ,9]+infected[ ,10])/infected[ ,1]
  inf.fams.prev.mean = mean(inf.fams.prev)
  
  inf.fams.prev.I = (infected[ ,10])/infected[ ,1]
  inf.fams.prev.I.mean = mean(inf.fams.prev.I)
  
  chull.fams.prev = sum(fams.in.ch[ ,9]+fams.in.ch[ ,10])/sum(fams.in.ch[ ,1])
  #chull.fams.prev.mean = mean(chull.fams.prev)
  chull.fams.prev.mean = chull.fams.prev
  
  chull.fams.prev.I = sum(fams.in.ch[ ,10])/sum(fams.in.ch[ ,1])
  #chull.fams.prev.I.mean = mean(chull.fams.prev.I)
  chull.fams.prev.I.mean = chull.fams.prev.I
  }
  
  #transmission contribution
  trans.df = onerun[[19]]
  
  tot.dir = sum(trans.df[,1])
  tot.env = sum(trans.df[,2])
  
  dir.prop = tot.dir/(tot.dir + tot.env)
  env.prop = tot.env/(tot.dir + tot.env)
  
  #combine all the metrics
  metrics.out = c(max.pop, max.pop.time,
                  max.I, max.I.time, last.I.time, max.prev,
                  total.I, max.inc, time.max.inc,
                  max.spread, max.number.I.cells, max.spread.area, #12
                  final.avg.prion.density,
                  final.I, final.pop, final.prev,
                  final.spread, final.number.I.cells, final.spread.area, final.max.sp.cent, #20
                  final.E, final.I.cells, final.E.cells,
                  final.surv.prev,
                  prions.A, prop.inf.cells, prop.ID50.cells,
                  prop.ID50.infcells, prions.gamma.shape, prions.gamma.rate,
                  SS.S.prop, SS.E.prop, SS.I.prop,#33
                  spread.area.rate, spread.dist.rate, spread.distcent.rate,
                  mean.E.dist, std.E.dist, mean.I.dist, std.I.dist, mean.prev.rate, 
                  inf.fams.prev.mean, chull.fams.prev.mean, inf.fams.prev.I.mean, chull.fams.prev.I.mean, #45
                  tot.dir, tot.env, dir.prop, env.prop) #49
  
  return(metrics.out)
}