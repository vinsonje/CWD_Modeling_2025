find_pt_dist_chull = function(point, chull){
  
  # close the chull
  if(!identical(chull[1,], chull[nrow(chull), ])){chull = rbind(chull, chull[1, ])}
  
  # A simple distance function
  dis = function(x0, x1, y0, y1){sqrt((x0 - x1)^2 +(y0 - y1)^2)}
  
  d = NULL   # Your distance vector
  for(i in 1:(nrow(chull) - 1)){
    
    ba = c((point[1] - chull[i, 1]), (point[2] - chull[i,2])) #Vector BA 
    bc = c((chull[i+1, 1] - chull[i, 1]), (chull[i+1, 2] - chull[i, 2])) #Vector BC
    dbc = dis(chull[i+1, 1], chull[i, 1], chull[i+1, 2], chull[i, 2]) #Distance BC
    dp = (ba[1] * bc[1] + ba[2] * bc[2])/dbc          #Projection of A on BC
    
    d[i] = sqrt(abs((ba[1]^2 + ba[2]^2) - dp^2))
    
    if(dp <= 0){ #If projection is outside of BC on B side
      d[i] = dis(point[1], chull[i,1], point[2], chull[i,2])
    }
    
    if(dp >= dbc){ #If projection is outside of BC on C side
      d[i] = dis(chull[i+1, 1], point[1], chull[i+1, 2], point[2])
    }
  } #end for loop
  min(d)
}

# #testing
# npoints = 100
# 
# x = runif(npoints, 1, 100)
# y = runif(npoints, 1, 100)
# 
# xy.mat = matrix(c(x,  y), nrow = npoints, byrow = FALSE)
# ch.pts = xy.mat[chull(xy.mat),]
# 
# plot(xy.mat, cex = 0.5)
# lines(ch.pts)
# point = matrix(c(50, 50), nrow = 1)
# points(point, col = "red")
# 
# find_pt_dist_chull(xy.mat[1,], ch.pts)
