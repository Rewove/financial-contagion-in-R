source('create_network.R')
source('judge_bankrupt.R')
source('simulate_bankrupt.R')
library(foreach)
library(doParallel)

network_size <- 1000
simulation_times <- 100
x_average_dgree <- seq(0, 10.1, 1)  # 0.2
prob <- x_average_dgree/(network_size -1)
contagion_threshould <- 0.05
threshould <- network_size * contagion_threshould

numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)
# use for z=1:10 your range, the .combine declares how to combine your dataframe afterwrads,
#.inorder makes sure it's sorted and the values are in the right order (TRUE is default)
df<-foreach(z = 1:10, .combine=rbind, .inorder=TRUE) %dopar%{
  etwork_size <- 1000
  simulation_times <- 100
  x_average_dgree <- seq(0, 10.1, 1)  # 0.2
  prob <- x_average_dgree/(network_size -1)
  contagion_threshould <- 0.05
  threshould <- network_size * contagion_threshould
  y_prob = list()
  y_exte = list()
  for (i in 1:porb){
    count_contagion <- 0
    sum_percentages <- 0
    cat('Doing simulation on Average Degree:')
    cat(prob[i]*(network_size-1))
    cat('\n')
    for (j in 1:simulation_times){
      library(igraph)
      G <- create_network(network_size, j)
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
  #make sure you return the values, otherwise they don't get combined by foreach
  plot(x_average_dgree, y_prob, pch=4, ylim=c(0,1),
       ylab = 'Probability and Extent of Contagion',
       xlab = 'Average Degree (Connectivity)')
  points(x_average_dgree, y_exte, pch=16)
  title(main='Probability and Extent of Contagion', 
        sub='Random Choose One Bank Bankrupt on ER Random Network')
  return(do.call(rbind, Map(data.frame, A=y_prob, B=y_exte)))
}
#foreach returns nested lists, so you can change it to a dataframe easily
df= as.data.frame(df)
results = df
write.table(results,file="results.csv",quote=F,col.name=F,row.names=F)
#View(df)
stopCluster(cl)

