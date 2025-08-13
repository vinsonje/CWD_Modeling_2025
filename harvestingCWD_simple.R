harvestingCWD_simple = function(pop, h.time, h.harvest, thyme){
  
  pop.out = pop
  harvest.out = matrix(c(0, 0, 0, 0, 0), nrow = 1)
  
  if(thyme %in% h.time){
    
  N.tot = sum(pop.out[,1])
  
  S.tot = rep("S", sum(pop.out[,8]))
  S.fid = rep(pop.out[,12], pop.out[,8])
  
  E.tot = rep("E", sum(pop.out[,9]))
  E.fid = rep(pop.out[,12], pop.out[,9])
  
  I.tot = rep("I", sum(pop.out[,10]))
  I.fid = rep(pop.out[,12], pop.out[,10])
  
  all.ind = c(S.tot, E.tot, I.tot)
  
  if(length(all.ind) < h.harvest){h.harvest = ceiling(length(all.ind)*0.5)} #you added this line because of an error when pop size is less than number harvested
  
  if(length(all.ind) > 1){ind.rem = sample(all.ind, h.harvest, replace = FALSE)}else{ind.rem = all.ind}
  
  S.rem = length(which(ind.rem == "S"))
  E.rem = length(which(ind.rem == "E"))
  I.rem = length(which(ind.rem == "I"))
  
  if(S.rem > 0){if(length(S.fid) == 1){S.rem.fid = rep(S.fid, S.rem)}else{S.rem.fid = c(0, sample(S.fid, S.rem, replace = FALSE))}}else{S.rem.fid = c(0)}
  if(E.rem > 0){if(length(E.fid) == 1){E.rem.fid = rep(E.fid, E.rem)}else{E.rem.fid = c(0, sample(E.fid, E.rem, replace = FALSE))}}else{E.rem.fid = c(0)}
  if(I.rem > 0){if(length(I.fid) == 1){I.rem.fid = rep(I.fid, I.rem)}else{I.rem.fid = c(0, sample(I.fid, I.rem, replace = FALSE))}}else{I.rem.fid = c(0)}
  
  S.rem.fid.counts = data.frame(table(S.rem.fid), status = "S")
  names(S.rem.fid.counts) = c("fid", "num.rem", "status")
  E.rem.fid.counts = data.frame(table(E.rem.fid), status = "E")
  names(E.rem.fid.counts) = c("fid", "num.rem", "status")
  I.rem.fid.counts = data.frame(table(I.rem.fid), status = "I")
  names(I.rem.fid.counts) = c("fid", "num.rem", "status")
  
  all.fid.counts = rbind(S.rem.fid.counts, E.rem.fid.counts, I.rem.fid.counts)
  all.fid.counts$fid = unfactor(all.fid.counts$fid)
  all.fid.counts.df = spread(all.fid.counts, key = status, value = num.rem, fill = 0)
  all.fid.counts.df = subset(all.fid.counts.df, fid != 0)
  all.fid.counts.df
  
  
  for(z in 1:length(all.fid.counts.df$fid)){
    pop.out.index = which(pop.out[,12] == all.fid.counts.df$fid[z])
    
    pop.out[pop.out.index,1] = pop.out[pop.out.index, 1] - sum(all.fid.counts.df[z, c("E", "I", "S")])
    pop.out[pop.out.index,8] = pop.out[pop.out.index, 8] - all.fid.counts.df[z, c("S")]
    pop.out[pop.out.index,9] = pop.out[pop.out.index, 9] - all.fid.counts.df[z, c("E")]
    pop.out[pop.out.index,10] = pop.out[pop.out.index, 10] - all.fid.counts.df[z, c("I")]
    
    harvest.out.temp = c(time = thyme, loc = pop.out[pop.out.index, 3], S = all.fid.counts.df[z, c("S")], E = all.fid.counts.df[z, c("E")], I = all.fid.counts.df[z, c("I")])
    harvest.out = rbind(harvest.out, harvest.out.temp)
  } #end for loop for removals
  
  pop.out = pop.out[which(pop.out[,1]>0),]
  harvest.out = harvest.out[which(rowSums(harvest.out[,3:5, drop = FALSE])>0), , drop = FALSE]
  } #end if h.time
  
  rownames(harvest.out) = NULL
    return(list(pop.out, harvest.out))
}