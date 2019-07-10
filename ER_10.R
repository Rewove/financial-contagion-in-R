#source('~/Desktop/network/create_network.R')
#source('~/Desktop/network/judge_bankrupt.R')
#source('~/Desktop/network/simulate_bankrupt.R')
source('create_network.R')
source('judge_bankrupt.R')
source('simulate_bankrupt.R')
library(parallel)

network_size <- 1000
simulation_times <- 100
x_average_dgree <- seq(0, 10.1, 0.2)  # 0.2
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

main()
