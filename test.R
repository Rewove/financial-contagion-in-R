install.packages("igraph")
library(igraph)

er <- sample_gnp(n=10, p=3/9, TRUE)
e <- E(er)
length(e)

head(e)

e[1]
plot(er, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5)
er[]
a= er[]
a
a[1][2]
# use for store results
res <- data.frame()
res <- rbind(res, r)

system.time({
  G = create_network(1000, 3/999)
  #plot(G, layout=layout_in_circle)
  b = simulate_bankrupt(G, type ='num')
  b
})


doing <- function(i){
  r = i*100
  sprintf('Get the number: %s', i)
  Sys.sleep(1)
  res <- cbind(r, i)
  abs <- cbind(data.frame(),  res)
  return(abs)
}
x_average_dgree <- seq(0, 10.1, 1)

# lapply is equivalant to loop
system.time({
  results <- lapply(x_average_dgree, doing)
})

starts <- rep(100, 40)
fx <- function(nstart) kmeans(Boston, 4, nstart=nstart)
numCores <- detectCores()
numCores
system.time(
  results1, results2 <- mclapply(x_average_dgree, doing, mc.cores = numCores)
)

results
plot(results)


