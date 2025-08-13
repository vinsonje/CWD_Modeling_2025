areaOfinfectionCWD = function(pop, centroids, inc){

#subset out infected rows
infected = pop[pop[, 9]>0 | pop[, 10]>0, ,drop = FALSE]	

#determine which cells infected
infectcells = unique(infected[,3])

midpoint = c(max(centroids[,1]/2),max(centroids[,2]/2))
mid.cell = which(centroids[, 1] >= midpoint[1] & centroids[, 2] >= midpoint[2])[1] #location on grid closest to midpoint
	
if(length(infectcells) == 1){ #get the area of one grid cell

dist.cent = sqrt((centroids[infectcells,1] - centroids[mid.cell,1])^2 + (centroids[infectcells,2] - centroids[mid.cell,2])^2)
  
out = c(1, inc^2, 0, max(dist.cent)) #number of infected cells, area of infection, maximum distance between two infected cells

} else if(length(infectcells) == 2){ #area of two grid cells, get distance between their centroids

dist = sqrt((centroids[infectcells, ][1, 1] - centroids[infectcells, ][2, 1])^2 + (centroids[infectcells, ][1, 2] - centroids[infectcells, ][2, 2])^2)
dist.cent = sqrt((centroids[infectcells,1] - centroids[mid.cell,1])^2 + (centroids[infectcells,2] - centroids[mid.cell,2])^2)

out = c(2, 2*(inc^2), dist, max(dist.cent))

} else {

#get all centroids xy coords
xy = centroids[infectcells, , drop = FALSE]	

#compute convex hull of points
ch = chull(xy)

#get boundary coordinates of convex hull
boundary = xy[ch,,drop = FALSE]

#get area within boundary
A = abs(polyarea(boundary[,1], boundary[,2]))


maxdist = max(pdist(xy))

dist.cent = sqrt((centroids[infectcells,1] - centroids[mid.cell,1])^2 + (centroids[infectcells,2] - centroids[mid.cell,2])^2)

#number of unique infected cells, area of infection, max distance between infected cells
out = c(length(infectcells), A, maxdist, max(dist.cent))

	} #greater than 2 infected closing bracket
return(out)
	} #function closing bracket
