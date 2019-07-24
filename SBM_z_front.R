source('create_network.R')
source('judge_bankrupt.R')
source('simulate_bankrupt.R')
source('plot_figuer.R')

network_size <- 1000
simulation_times <- 100 
contagion_threshould <- 0.05
threshould <- network_size * contagion_threshould


main <- function(average_degree = 3){
  cat('The average degree cosnsidered here is:')
  cat(average_degree)
  cat('\n')
  
  p_pp <- seq(0, get_low_bound_pp(average_degree), 0.0005)
  
  y_prob = list()
  y_exte = list()
  for (j in p_pp) {
    count_contagion <- 0
    sum_percentages <- 0
    cat('Doing simulation on p_cc: ')
    cat(j)
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
      G <- create_network(network_size, parameter = average_degree, 
                          p = j,  type = 'sbm fomer')
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
  file_name = paste("results_sbm_f_",average_degree,".csv", sep='')
  results = do.call(rbind, Map(data.frame, y_prob=y_prob, y_exte=y_exte))
  cat('Saving the results ... ')
  cat('\n')
  write.table(results,file=file_name,sep=",",quote=F,col.name=F,row.names=F)
  return(results)
}


system.time({
  results <- main(3)
  plot_the_figure(p_pp, results$y_prob, results$y_exte, 
                  network_name = 'SBM Network',
                  xlab = 'p_pp: Probability of Periphery Connectivity',
                  notes = 'file_name')
})

