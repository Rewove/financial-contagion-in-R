# install.packages("igraph")
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
  
    G = SBM_network(1000, 3, 0.003001501)
    in_degree = sum(degree(G, mode='out'))
    in_degree
    out_degree = sum(degree(G, mod = 'in'))
    out_degree
    print(length(E(G))/1000)
    
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
  results <- mclapply(x_average_dgree, doing, mc.cores = numCores)
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
    count_contagion <- 0
    sum_percentages <- 0
    print('Doing simulation on Average Degree:')
    print(j*(network_size-1))
    
    for (i in 1:simulation_times){
      # print('Doing No:')
      # print(i+1)
      # print('At average degree:')
      # print(round(j*(network_size-1)))
      G <- create_network(network_size, j)
      r <- simulate_bankrupt(G, type = 'num')
      r <- as.numeric(r)
      # print('Here in this simulation have bankrupt banks:')
      # print(r)
      if (r > threshould){
        count_contagion <- count_contagion +1
        percentage_cont <- r/network_size
        sum_percentages <- sum_percentages + percentage_cont
      }
    }
    
    proba_contagion <- count_contagion / simulation_times
    if (count_contagion != 0){
      exten_contagion <- sum_percentages / count_contagion
    } else{
      exten_contagion <- 0
    }
    y_prob <- cbind(y_prob, proba_contagion)
    y_exte <- cbind(y_exte, exten_contagion)
  }
  
  results = data.frame(y_prob, y_exte)
  write.table(results,file="results.csv",quote=F,col.name=F,row.names=F)
  
  plot(x_average_dgree, y_prob, pch=4, ylim=c(0,1),
       ylab = 'Probability and Extent of Contagion',
       xlab = 'Average Degree (Connectivity)')
  points(x_average_dgree, y_exte, pch=16)
  titil(main='Probability and Extent of Contagion', 
        sub='Random Choose One Bank Bankrupt on ER Random Network')
  
}


test <- function(){
  for ( i in 1:10){
    for (j in 1:10){
      cat()
    }
  }
}
test()

print('dafaaf',1)
a = 11
print('dafaaf'+a)


simulate_function <- function(prob){
  count_contagion <- 0
  sum_percentages <- 0
  
  for (i in 1:simulation_times){
    if (i %% 10 == 0){
      cat('Doing test No.')
      cat(i)
      cat('\n')
    }
    G <- create_network(network_size, prob)
    r <- simulate_bankrupt(G, type = 'num')
    r <- as.numeric(r)
    if (r > threshould){
      count_contagion <- count_contagion +1
      percentage_cont <- r/network_size
      sum_percentages <- sum_percentages + percentage_cont
    }
  }
  proba_contagion <- count_contagion / simulation_times
  if (count_contagion != 0){
    exten_contagion <- sum_percentages / count_contagion
  } else{
    exten_contagion <- 0
  }
  results <- list(proba_contagion, exten_contagion)
  results=data.frame(results)
  write.table(results,file="results.csv",append=T,quote=F,col.name=F,row.names=F)
}


numCores <- detectCores()
numCores
system.time(
  results <- mclapply(prob, simulate_function, mc.cores = numCores)
)


a = simulate_function(3/999)

results=data.frame(a)
write.table(results,file="results.csv",append=T,quote=F,col.name=F,row.names=F)
b=read.csv('results.csv',header=F)
b
