########################################
#Sharpshooting function
########################################
#The purpose of this function is to simulate
#sharpshooting on a landscape. It has inputs of:
#pop = population matrix
#centroids = centroids matrix
#ss.loc = vector of grid ids where SS happens
#ss.time = vector of times when SS happens
#ss.radius = the radius around the cells centroid where SS is effective
#ss.eff = the effectiveness of sharpshooting, the proportion of the population that sharpshooting can remove 
#thyme = the current time of the simulation
#########################################

sharpshootingCWD = function(pop, centroids, surv.data, ss.shooters, ss.time, ss.radius, ss.num, ss.strat, 
                            ss.laccess, ss.laccess.dist, ss.edge.weight, thyme){
  
  surv.out = surv.data
  pop.out = pop
  SS.out = NULL
  
  if(thyme %in% ss.time & dim(surv.data)[1]>0){
    
    num.rem.out = NULL
    
    surv.counts = aggregate(num ~ loc, surv.data, sum, simplify = TRUE, drop = TRUE)
    surv.counts[,1] = as.numeric(as.character(surv.counts[,1]))
    ss.poss.locs = unique(surv.counts[,1])
    
    if(length(ss.poss.locs) < ss.shooters){ss.shooters = length(ss.poss.locs)}
    
    if(ss.strat == "random"){ss.loc = sample(ss.poss.locs, ss.shooters, replace = FALSE)}
    if(ss.strat == "priority"){
      ss.poss.loc.order = surv.counts[order(surv.counts[,2], decreasing = TRUE),]
      
      ss.loc = head(ss.poss.loc.order[,1], ss.shooters)
    }
    if(ss.strat == "edge"){
      if(length(ss.poss.locs)<2){
        ss.loc = ss.poss.locs
      }else{
      ss.poss.xy = centroids[ss.poss.locs,]
      ss.chull = ss.poss.xy[chull(ss.poss.xy),]
      
      dist.chull = NULL
      for(k in 1:dim(ss.poss.xy)[1]){
      dist.chull[k] = find_pt_dist_chull(ss.poss.xy[k,], ss.chull)
      }
      dist.0 = length(which(dist.chull == 0))
      
      ss.dist.order = surv.counts[order(dist.chull, decreasing = FALSE),]
      
      if(dist.0 > ss.shooters){
        ss.loc = sample(head(ss.dist.order[,1], dist.0), ss.shooters, replace = FALSE)  
      }else{
        ss.loc = head(ss.dist.order[,1], ss.shooters)
      }
      }
    }
    if(ss.strat == "edge2.0"){
      if(length(ss.poss.locs)<2){
        ss.loc = ss.poss.locs
      }else{
      ss.poss.xy = centroids[ss.poss.locs,]
      ss.chull = ss.poss.xy[chull(ss.poss.xy),]
      
      dist.chull = NULL
      for(k in 1:dim(ss.poss.xy)[1]){
        dist.chull[k] = find_pt_dist_chull(ss.poss.xy[k,], ss.chull)
      }
      
      surv.counts.dist = data.frame(surv.counts, dist = dist.chull)
      
      max.num = max(surv.counts.dist$num)
      det.scale = surv.counts.dist$num/max.num
      surv.counts.dist$det.scale = det.scale
      
      max.dist = max(surv.counts.dist$dist)
      min.dist = min(surv.counts.dist$dist)
      dist.scale = (max.dist - surv.counts.dist$dist)/(max.dist-min.dist)
      surv.counts.dist$dist.scale = dist.scale
      
      edge2.scale = (1-ss.edge.weight)*surv.counts.dist$det.scale + ss.edge.weight*surv.counts.dist$dist.scale
      surv.counts.dist$edge2.scale = edge2.scale
      
      ss.edge2.order = surv.counts.dist[order(surv.counts.dist$edge2.scale, decreasing = TRUE),]
      
      ss.loc = head(ss.edge2.order[,1], ss.shooters)}
    }
    
    
  for(q in 1:length(ss.loc)){
    
    num.rem.temp = 0
    
    ss.access = rbinom(1, 1, ss.laccess)
    
    if(ss.access == 1){
      ss.loc.id.now = ss.loc[q]
    }else{
      acc.dist = sqrt((centroids[ss.loc[q], 1] - centroids[, 1])^2 + (centroids[ss.loc[q], 2] - centroids[, 2])^2)
     
      pot.acc.cells = which(acc.dist <= ss.laccess.dist)
      pot.acc.cells = subset(pot.acc.cells, pot.acc.cells != ss.loc[q])
      if(length(pot.acc.cells) < 1){ss.loc.id.now = ss.loc[q]}else{ss.loc.id.now = sample(pot.acc.cells, 1)}
      } #end else
    
    ss.loc.xloc.now = centroids[ss.loc.id.now, 1]
    ss.loc.yloc.now = centroids[ss.loc.id.now, 2]
    
    cells.in.ss.radius = NULL
    for(w in 1:dim(centroids)[1]){
      potential.cell.x = centroids[w,1]
      potential.cell.y = centroids[w,2]
      
      distance = sqrt((ss.loc.xloc.now - potential.cell.x)^2 + (ss.loc.yloc.now - potential.cell.y)^2)
      
      if(distance <= ss.radius){
        # print("true")
        cells.in.ss.radius = append(cells.in.ss.radius, w)
        }
    }#end of distance calc loop
    
    fams.in.radius = intersect(pop.out[,3], cells.in.ss.radius)
    # print(paste("removing", length(fams.in.radius), "families"))
    
    if(length(fams.in.radius) > 0){ #if there are families within the radius
    fams.rem.index = which(pop.out[,3] %in% fams.in.radius)
   
    pop.sub = pop.out[fams.rem.index, , drop = FALSE]
    
    pop.classes = colSums(pop.sub[,8:10, drop = FALSE])
    
    deer.in.sub = rep(c("S", "E", "I"), pop.classes)
    
    S.in.sub = rep(pop.sub[,12], pop.sub[,8])
    E.in.sub = rep(pop.sub[,12], pop.sub[,9])
    I.in.sub = rep(pop.sub[,12], pop.sub[,10])
    
    if(ss.num > length(deer.in.sub)){ss.num = length(deer.in.sub)}
    
    if(length(deer.in.sub) > 1){rem.mems = sample(deer.in.sub, ss.num, replace = FALSE)}else{rem.mems = deer.in.sub}
    
    S.rem.pool = length(which(rem.mems == "S"))
    E.rem.pool = length(which(rem.mems == "E"))
    I.rem.pool = length(which(rem.mems == "I"))
    
    if(S.rem.pool > 0){if(length(S.in.sub) == 1){S.rem.fid = rep(S.in.sub, S.rem.pool)}else{S.rem.fid = sample(S.in.sub, S.rem.pool, replace = FALSE)}}else{S.rem.fid = S.in.sub}
    if(E.rem.pool > 0){if(length(E.in.sub) == 1){E.rem.fid = rep(E.in.sub, E.rem.pool)}else{E.rem.fid = sample(E.in.sub, E.rem.pool, replace = FALSE)}}else{E.rem.fid = E.in.sub}
    if(I.rem.pool > 0){if(length(I.in.sub) == 1){I.rem.fid = rep(I.in.sub, I.rem.pool)}else{I.rem.fid = sample(I.in.sub, I.rem.pool, replace = FALSE)}}else{I.rem.fid = I.in.sub}
    
    S.rem.ct = data.frame(table(S.rem.fid))
    if(dim(S.rem.ct)[1]>0){S.rem.ct$S.rem.fid = unfactor(S.rem.ct$S.rem.fid)}
    E.rem.ct = data.frame(table(E.rem.fid))
    if(dim(E.rem.ct)[1]>0){E.rem.ct$E.rem.fid = unfactor(E.rem.ct$E.rem.fid)}
    I.rem.ct = data.frame(table(I.rem.fid))
    if(dim(I.rem.ct)[1]>0){I.rem.ct$I.rem.fid = unfactor(I.rem.ct$I.rem.fid)}
    
    for(f in 1:length(fams.rem.index)){
      
      S.rem = 0
      E.rem = 0
      I.rem = 0
      
      fid = pop[fams.rem.index[f], 12]
      
      if(dim(S.rem.ct)[1]>0){if(fid %in% S.rem.ct$S.rem.fid){S.rem = S.rem.ct$Freq[which(S.rem.ct$S.rem.fid==fid)]}}
      
      if(dim(E.rem.ct)[1]>0){if(fid %in% E.rem.ct$E.rem.fid){E.rem = E.rem.ct$Freq[which(E.rem.ct$E.rem.fid==fid)]}}
      
      if(dim(I.rem.ct)[1]>0){if(fid %in% I.rem.ct$I.rem.fid){I.rem = I.rem.ct$Freq[which(I.rem.ct$I.rem.fid==fid)]}}
      
      pop.out[fams.rem.index[f], 8] = pop.out[fams.rem.index[f], 8] - S.rem
      pop.out[fams.rem.index[f], 9] = pop.out[fams.rem.index[f], 9] - E.rem
      pop.out[fams.rem.index[f], 10] = pop.out[fams.rem.index[f], 10] - I.rem
      
      pop.out[fams.rem.index[f], 1] = sum(pop.out[fams.rem.index[f], 8:10])
      
      SS.out.temp = c(time = thyme, loc = pop.out[fams.rem.index[f], 3], SS.loc = ss.loc[q], S = S.rem, E = E.rem, I = I.rem)
      
      SS.out = rbind(SS.out, unname(SS.out.temp))
      
    } #end for loop to remove individuals from each cell
    }else{#if there are no familes in the radius
      SS.out.temp = c(time = thyme, loc = ss.loc.id.now, SS.loc = ss.loc.id.now, S = 0, E = 0, I = 0)
      SS.out = rbind(SS.out, unname(SS.out.temp))
    }
    
    surv.out = surv.out[-which(surv.out[,1] == ss.loc[q]),]  
  }#end of ss.loc loop
    
    if(is.null(SS.out) == FALSE){ #if SS.out is not NULL
      if(is.null(dim(SS.out)) == TRUE){ #if SS.out is a vector
        if(sum(SS.out[4:6]) == TRUE){SS.out = NULL} #if SS.out is a vector with no ind. removed
      }else{SS.out = SS.out[-which(rowSums(SS.out[,4:6, drop = FALSE]) == 0),]} #otherwise just remove the rows that removed no ind.
      }
    
    pop.out = pop.out[which(pop.out[,1]>0),]

  }#end of ss.time if
  
  return(list(pop.out, surv.out, SS.out))
}#end of function
