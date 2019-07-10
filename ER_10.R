#source('~/Desktop/network/create_network.R')
#source('~/Desktop/network/judge_bankrupt.R')
#source('~/Desktop/network/simulate_bankrupt.R')
source('create_network.R')
source('judge_bankrupt.R')
source('simulate_bankrupt.R')
library(parallel)

network_size <- 1000
simulation_times <- 100
x_average_dgree <- seq(0, 10.1, 0.2)
prob <- x_average_dgree/(network_size -1)
contagion_threshould <- 0.05
threshould <- network_size * contagion_threshould


main <- function(){
  y_prob <- c()
  y_exte <- c()
  for (j in prob) {
    count_contagion <- 0
    sum_percentages <- 0
    for (i in 1:simulation_times){
      sprintf('Doing No. %s test at AD %s', i+1, round(j*(network_size-1)))
      G <- create_network(network_size, prob)
      r <- simulate_bankrupt(G, type = 'num')
      r <- as.numeric(r)
      if (r > threshould){
        count_contagion <- count_contagion +1
        percentage_cont <- r/network_size
        sum_percentages <- sum_percentages + percentage_cont
      }
    proba_contagion <- count_contagion / simulation_times
    exten_contagion <- sum_percentages / count_contagion
    
    y_prob <- cbind(y_prob, proba_contagion)
    y_exte <- cbind(y_exte, exten_contagion)
    }
    plot(x_average_dgree, y_prob, pch=4)
    points(x_average_dgree, y_exte, pch=16)
  }
  
}

main()
