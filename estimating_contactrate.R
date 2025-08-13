#Figuring out the contact rate from simulations on a landscape:


glm.model = function(inter, coef, data){
  pred = matrix(0, dim(data)[1], dim(data)[1])
  
  for(j in 1:dim(data)[1]){
  pred[j,] = 1/(1+exp(-(inter + coef*data[j,])))
  }
  return(pred)
}


#generate the landscape
setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Metapop_Model/Scripts") #for my PC
grid.xmax = 100
grid.ymax = 100

cell.x.size = 1.0
cell.y.size = 1.0

grid = create.grid(grid.xmax, grid.ymax, cell.x.size, cell.y.size)

#create a matrix of distances
dist.mat = matrix(0, dim(grid)[1], dim(grid)[1])

for(i in 1:dim(grid)[1]){
dist.vec = sqrt((grid[i, 6] - grid[,6])^2 + (grid[i, 7] - grid[,7])^2) 
dist.mat[i,] = dist.vec
}


dir.means = NULL
indir.means = NULL
for(j in 1:100){
#create a matrix of probability of direct contact
inter.direct = 3.33
coef.direct = -7.01

direct.mat = glm.model(inter.direct, coef.direct, dist.mat)

#simulate if a contact does happen
dir.contact.mat = matrix(rbinom(length(direct.mat), 1, direct.mat), nrow = nrow(direct.mat), ncol = ncol(direct.mat))

dir.contact.by.cell = rowSums(dir.contact.mat)
dir.mean = mean(dir.contact.by.cell)

dir.means = c(dir.means, dir.mean)

#create a mtrix of probability of indirect contact 
inter.indirect = 3.99
coef.indirect = -3.31

indirect.mat = glm.model(inter.indirect, coef.indirect, dist.mat)

#simulate if a contact does happen
indir.contact.mat = matrix(rbinom(length(indirect.mat), 1, indirect.mat), nrow = nrow(indirect.mat), ncol = ncol(indirect.mat))

indir.contact.by.cell = rowSums(indir.contact.mat)
hist(indir.contact.by.cell)

indir.mean = mean(indir.contact.by.cell)

indir.means = c(indir.means, indir.mean)
}