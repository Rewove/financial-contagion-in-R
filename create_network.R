# require package igraph, if not used before please install it
# install.packages("igraph")
library(igraph)
# basic paramters
AiIB = 0.2

create_network <- function(network_size, prob, print_out = FALSE){
  
  # first creat the graph without weights
  G <- sample_gnp(n=network_size, p=prob, TRUE)
  M <- length(E(G))
  edge_df <- ends(G, E(G))
  if (print_out == TRUE) sprintf('The network have %s edges, hence the average degree is %s',
                                 M, M/network_size)
  
  # creat the list of weights
  edge_weight <- list(rep(0,M))
  #print('before')
  #print(edge_weight)
  for (i in 1:network_size) {
    #print('node:')
    #print(i)
    
    nodes_in <- neighbors(G, i, 'in')
    #print('has neighbors:')
    #print(nodes_in)
    
    number_of_in <- length(nodes_in)
    if (number_of_in != 0){
      this_weight <- AiIB / number_of_in
      ENDS = as.vector(rbind(as.numeric(nodes_in), i))
      SpecialEdges = get.edge.ids(G, ENDS)
      #print('the edges need to change')
      #print(SpecialEdges)
      E(G)$weight[SpecialEdges] = c(rep(this_weight, number_of_in))
      #print('after')
      #print(E(G)$weight[SpecialEdges])
    } else{
      this_weight <- 0
    }
  }
  #print(E(G)$weight)
  if (print_out == TRUE) E(G)$weight
  return (G)
}

