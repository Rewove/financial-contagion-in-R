library(igraph)
# source('~/Desktop/network/judge_bankrupt.R')
source('judge_bankrupt.R')

simulate_bankrupt <- function(network, type = 'banks') {
  G <- network
  network_size <- length(V(G))
  initial_point <- as.numeric(sample(V(G), 1))
  
  bankrupt_bank = c()
  this_batch_bankrupt = c()
  bankrupt_bank = append(bankrupt_bank, initial_point)
  this_batch_bankrupt = append(this_batch_bankrupt, initial_point)
  
  count_batch = 1
  number_bankrupt = 1
  simulate = TRUE
  
  while (simulate == TRUE){
    this_batch_neighbor = c()
    for (i in this_batch_bankrupt){
      nodes_out <- as.numeric(neighbors(G, i, 'out'))
      this_batch_neighbor = append(this_batch_neighbor, nodes_out)
    }
    this_batch_neighbor = unique(this_batch_neighbor)
    this_batch_bankrupt = judge_bankrupt(G, this_batch_neighbor, bankrupt_bank)
    this_batch_bankrupt = unique(this_batch_bankrupt)
    bankrupt_bank = append(bankrupt_bank, this_batch_bankrupt)
    bankrupt_bank = unique(bankrupt_bank)
    
    if (length(bankrupt_bank) == number_bankrupt){
      simulate = FALSE
    } else{
      number_bankrupt = length(bankrupt_bank)
    }
  }
  if (type == 'banks'){
    return (bankrupt_bank)
  } else{
    return (length(bankrupt_bank))
  }
  
}
