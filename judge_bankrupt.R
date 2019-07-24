library(igraph)


judge_bankrupt <- function(network, this_batch_neighbor, allready_bankrupt, target_policy = FALSE){
  G <- network
  node_degree_all  = degree(G, mode='all') 
  bankrupt_banks <- c()
  Ki = 0.04
  
  for (i in this_batch_neighbor){
    
    if (target_policy == TRUE){
      node_degree <- degree(G, v=i, mode = "all")
      threshold <- as.numeric(quantile(node_degree_all, 0.95))
      if (node_degree >= threshold){
        Ki = 0.1
      }
    }
    
    nodes_in <- as.numeric(neighbors(G, i, 'in'))
    nodes_in_bankrupt <- c()
    
    for (j in nodes_in){
      if (j %in% allready_bankrupt){
        nodes_in_bankrupt=append(nodes_in_bankrupt, j)
      }
    }
    nodes_in_bankrupt = unique(nodes_in_bankrupt)
    
    if (is.null(nodes_in_bankrupt) == FALSE){
      ENDS <- as.vector(rbind(as.numeric(nodes_in_bankrupt), i))
      SpecialEdges <- get.edge.ids(G, ENDS)
      sum_weight <- sum(as.numeric(E(G)$weight[SpecialEdges]))
      
      if (sum_weight > Ki) {
        bankrupt_banks <- append(bankrupt_banks, i)
      }
    }
  }
  bankrupt_banks = unique(bankrupt_banks)
  return (bankrupt_banks)
}
