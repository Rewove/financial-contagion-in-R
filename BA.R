#source('~/Desktop/network/create_network.R')
#source('~/Desktop/network/judge_bankrupt.R')
#source('~/Desktop/network/simulate_bankrupt.R')
source('create_network.R')
source('judge_bankrupt.R')
source('simulate_bankrupt.R')

network_size <- 1000
simulation_times <- 100  # 100
x_average_dgree <- seq(0, 10.1, 1)  # 0.2

contagion_threshould <- 0.05
threshould <- network_size * contagion_threshould


main <- function(){
  y_prob <- c()
  y_exte <- c()
  for (j in x_average_dgree) {
    count_contagion <- 0
    sum_percentages <- 0
    cat('Doing simulation on Average Degree: ')
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
      G <- create_network(network_size, j, type = 'ba')
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
    y_prob <- cbind(y_prob, proba_contagion)
    y_exte <- cbind(y_exte, exten_contagion)
  }
  return(do.call(rbind, Map(data.frame, y_prob=y_prob, y_exte=y_exte)))
}

system.time({
  results <- main()
  cat('Saving the results ... ')
  cat('\n')
  write.table(results,file="results_ab_1.csv",sep=",",quote=F,col.name=F,row.names=F)
  plot_the_figure(x_average_dgree, results$y_prob, results$y_exte, 
                  network_name = 'BA Network',
                  xlab = 'Average Degree (Connectivity)',
                  notes = ' ')
})

