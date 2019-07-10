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


network_size <- 100
simulation_times <- 10
x_average_dgree <- seq(0, 10.1, 1)
prob <- x_average_dgree/(network_size -1)
contagion_threshould <- 0.05
threshould <- network_size * contagion_threshould

main <- function(){
  y_prob <- c()
  y_exte <- c()
  for (j in prob) {
    print('Doing ')
    count_contagion <- 0
    sum_percentages <- 0
    for (i in 1:simulation_times){
      #print('Doing No:')
      #print(i+1)
      #print('At average degree:')
      #print(round(j*(network_size-1)))
      G <- create_network(network_size, j)
      r <- simulate_bankrupt(G, type = 'num')
      r <- as.numeric(r)
      #print('Here in this simulation have bankrupt banks:')
      #print(r)
      if (r > threshould){
        count_contagion <- count_contagion +1
        percentage_cont <- r/network_size
        sum_percentages <- sum_percentages + percentage_cont
      } 
      proba_contagion <- count_contagion / simulation_times
      if (count_contagion != 0){
        exten_contagion <- sum_percentages / count_contagion
      } else{
        exten_contagion <- 0
      }
      
    }
    y_prob <- cbind(y_prob, proba_contagion)
    y_exte <- cbind(y_exte, exten_contagion)
  }
  plot(x_average_dgree, y_prob, pch=4)
  points(x_average_dgree, y_exte, pch=16)
}

main()


test <- function(){
  for ( i in 1:10){
    for (j in 1:10){
      sprintf('%s',i*j)
    }
  }
}
test()

print('dafaaf',1)
a = 11
print('dafaaf'+a)
