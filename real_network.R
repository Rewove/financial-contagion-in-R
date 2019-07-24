source('create_network.R')
source('judge_bankrupt.R')
source('simulate_bankrupt.R')

main <- function(){
  #G <- create_network(parameter = "soc-Slashdot0902.txt", type = "read")
  #E(G)$weight[is.na(E(G)$weight)] <- 0
  simulation_times = 100
  y_prob <- c()
  y_exte <- c()
  rs <- c()
  threshould <- length(V(G))*0.05
  count_contagion <- 0
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
    r <- simulate_bankrupt(G, method = 'biggest', type = 'num')
    r <- as.numeric(r)
    rs <- cbind(rs,r)
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
    y_prob <- cbind(y_prob, proba_contagion)
    y_exte <- cbind(y_exte, exten_contagion)
  }
  return(do.call(rbind, Map(data.frame, y_prob=y_prob, y_exte=y_exte, results = rs)))
}

results2 <- main()
