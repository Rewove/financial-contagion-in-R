source('create_network.R')
source('judge_bankrupt.R')
source('simulate_bankrupt.R')

network_size <- 1000
simulation_times <- 100  # 100
average_degree = seq(0,10,0.2)

p_cc <- average_degree/(network_size-0.5)
contagion_threshould <- 0.05
threshould <- network_size * contagion_threshould


main <- function(){
  y_prob = list()
  y_exte = list()
  for (j in length(average_degree)) {
    count_contagion <- 0
    sum_percentages <- 0
    cat('Doing simulation on p_cc: ')
    cat(p_cc[j])
    cat('\n')
    
    for (i in 1:simulation_times){
      if (i==100){
        cat('No.')
        cat(i)
        cat('\n')
      } else if (i %% 10 == 0) {
        cat('No.')
        cat(i)
        cat('...')
      }
      G <- create_network(network_size, parameter = average_dgeree[j], 
                          p_cc = p_cc[j],  type = 'sbm')
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
  write.table(results,file="results_sbm_er.csv",quote=F,col.name=F,row.names=F)
  
}

system.time(
  main()
)

