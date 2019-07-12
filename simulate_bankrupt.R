library(igraph)
# source('~/Desktop/network/judge_bankrupt.R')
source('judge_bankrupt.R')

simulate_bankrupt <- function(network, type = 'banks', print_out=FALSE) {
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
    if (print_out == TRUE){
      cat('This batch bankrupt banks: ')
      cat(this_batch_bankrupt)
      cat('\n')
    }
    
    for (i in this_batch_bankrupt){
      nodes_out <- as.numeric(neighbors(G, i, 'out'))
      this_batch_neighbor = append(this_batch_neighbor, nodes_out)
    }
    this_batch_neighbor = unique(this_batch_neighbor)
    if (print_out == TRUE){
      cat('This batch neighbot: ')
      cat(this_batch_neighbor)
      cat('\n')
    }
    
    this_batch_bankrupt = judge_bankrupt(G, this_batch_neighbor, bankrupt_bank)
    this_batch_bankrupt = unique(this_batch_bankrupt)
    if (print_out == TRUE){
      cat('This batch bankrupt (after): ')
      cat(this_batch_bankrupt)
      cat('\n')
    }
    
    bankrupt_bank = append(bankrupt_bank, this_batch_bankrupt)
    bankrupt_bank = unique(bankrupt_bank)
    if (print_out == TRUE){
      cat('The bankrupt (total): ')
      cat(bankrupt_bank)
      cat('\n')
    }
    
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
