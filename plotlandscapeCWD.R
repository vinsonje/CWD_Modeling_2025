require(cowplot)
pop = cbind(pop, pop[,9]+pop[,10])
ggplot() + geom_tile(aes(x = pop[,5], y = pop[,6], fill = pop[,13])) + 
  # geom_text(aes(x = grid[,6], y = grid[,7], label=grid[,1]), alpha=0.75, color="#997009", size = 3.0) +
  theme_cowplot() + ylab("Y (km)") + xlab("X (km)") + 
  scale_fill_gradient(name = "family size", low = "white", high = "#301934")

centroids2 = data.frame(centroids, id = seq(1:dim(centroids)[1]))

ggplot(centroids2) + geom_tile(aes(x = X1, y = X2), fill = "white") + 
  geom_text(aes(x = X1, y = X2, label = id), alpha=0.75, color="#997009", size = 3.0) +
  theme_cowplot() + ylab("Y (km)") + xlab("X (km)") 
