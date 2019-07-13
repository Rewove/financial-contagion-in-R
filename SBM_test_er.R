source('create_network.R')
source('judge_bankrupt.R')
source('simulate_bankrupt.R')

network_size <- 1000
simulation_times <- 50  # 100
average_degree = seq(0,10,1)
cat('The average degree cosnsidered here is:')
cat(average_degree)
cat('\n')
p_cc <- average_degree/(network_size-0.5)
contagion_threshould <- 0.05
threshould <- network_size * contagion_threshould


main <- function(){
  y_prob = list()
  y_exte = list()
  for (j in p_cc) {
    count_contagion <- 0
    sum_percentages <- 0
    cat('Doing simulation on p_cc: ')
    cat(j)
    cat('\n')
    
    for (i in 1:simulation_times){
      # print('Doing No:')
      # print(i+1)
      # print('At average degree:')
      # print(round(j*(network_size-1)))
      G <- create_network(network_size, parameter = average_dgeree, 
                          p_cc = j,  type = 'sbm')
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
  
  
  plot(average_degree, y_prob, pch=4, ylim=c(0,1),
       ylab = 'Probability and Extent of Contagion',
       xlab = 'Average Degree')
  points(average_degree, y_exte, pch=16)
  title(main='SBM in ER mode', 
        sub='Random Choose One Bank Bankrupt on SBM Network')
  
  results = data.frame(y_prob, y_exte)
  write.table(results,file="results_sbm.csv",quote=F,col.name=F,row.names=F)
  
}

system.time(
  main()
)

