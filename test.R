# install.packages("igraph")
library(igraph)

G <- sample_gnp(n=1000, p=3/999, TRUE)
e <- E(er)
length(e)

head(e)

node_degree = degree(G, mode='out') + degree(G, mode='in')
node_degree_all  = degree(G,v=1, mode='all') 

biggest_node = which(node_degree==max(node_degree))
as.numeric(quantile(node_degree, 0.95))



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


testing <- function(){
  a=0
  b=0
  list_b=list()
  list_a=list()
  for (i in 1:10){
    for (j in 100:110){
      a=b+i
      b=a+j
    }
    list_b= cbind(list_b, b)
    list_a= cbind(list_a, a)
  }
  return(do.call(rbind, Map(data.frame, list_a=list_a, list_b=list_b)))
}
a=testing()
a
a$A


ER_network <- function(network_size, prob){
  prob = prob+1/(network_size-1)
  a = matrix(0, network_size, network_size)
  links = sample(seq(1,network_size,0.1), size = network_size * network_size, replace = T)
  links = matrix(links, ncol = network_size, byrow = T)
  threshold = prob * network_size
  for (i in 1:network_size){
    for (j in 1:network_size){
      if (i != j){
        if (links[i,j] < threshold){
          a[i,j] = 1
        }
      }
    }
  }
  G = graph_from_adjacency_matrix(a, mode = 'directed')
  return (G)
}
# test
# a = ER_network(100, 3/99)
# cat(length(E(a))/20)
length(E(sample_gnp(n=1000, p=4/999, TRUE)))/1000
length(E(ER_network(1000, 3.2/999)))/1000

SBM_network <- function(network_size, average_degree, p_cc){
  adjust = 1/(network_size-1)
  core_size <- network_size/2
  periphery_size <- network_size/2
  p_pp <- (average_degree - (network_size - 1) * p_cc / 4) * (4/(3 * network_size -1))
  #cat('p_pp is:')
  #cat(p_pp)
  #cat('\n')
  p_cp <- p_pp
  p_cc <- p_cc+adjust
  p_cp <- p_cp+adjust
  p_pp <- p_pp+adjust
  a = matrix(0, network_size, network_size)
  links = sample(seq(1,network_size,0.1), size = network_size * network_size, replace = T)
  links = matrix(links, ncol = network_size, byrow = T)
  threshold_ppp = p_pp * network_size
  threshold_pcp = p_cp * network_size
  threshold_pcc = p_cc * network_size
  
  for (i in 1:network_size){
    for (j in 1:network_size){
      if (i != j){
        if (i <= network_size/2){
          # left region (Core) in field
          if (j <= network_size/2){
            # left and upper region -> cc
            threshold <- threshold_pcc
          } else {
            # left and lower region -> cp
            threshold <- threshold_pcp
          }
        } else {
          # right reigion in field
          if (j <= network_size/2){
            # right upper region -> cp / pc
            threshold <- threshold_pcp
          } else {
            # right lower rigion -> pp
            threshold <- threshold_ppp
          }
        }
        
        if (links[i,j] < threshold){
          a[i,j] = 1
        }
        
      }
    }
  }
  
  G = graph_from_adjacency_matrix(a, mode = 'directed')
  return (G)
}
a=3.2
length(E(SBM_network(1000,a,a/999.5)))/1000


g= sample_pa(1000, power =3, m =2, directed = T)
length(E(g))/1000

g= sample_smallworld(dim=1, size=1000, nei=3.2, p=0.05, loops = FALSE, multiple = FALSE)
length(E(g))/1000