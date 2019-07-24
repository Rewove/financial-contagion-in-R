# require package igraph, if not used before please install it
# install.packages("igraph")
library(igraph)
# basic paramters
AiIB = 0.2

ER_network <- function(network_size, prob){
  a = matrix(0, network_size, network_size)
  #links = sample(c(1:network_size), size = network_size * network_size, replace = T)
  prob = prob+1/(network_size-1)
  links = sample(seq(1,network_size,0.1), size = network_size * network_size, replace = T)
  links = matrix(links, ncol = network_size, byrow = T)
  threshold = prob * network_size
  for (i in 1:network_size){
    for (j in 1:network_size){
      if (i != j){
        if (links[i,j] < threshold){
          a[i,j] = 1
        }
      }
    }
  }
  G = graph_from_adjacency_matrix(a, mode = 'directed')
  return (G)
}
# test
# a = ER_network(100, 3/99)
# cat(length(E(a))/20)
#length(E(sample_gnp(n=1000, p=4/999, TRUE)))/1000

# if SBM = ER:
# z = p * (N - 1/2)
# so for a certain z the p is:
# p = z/(network_size - 1/2)

SBM_network <- function(network_size, average_degree, p, later = TRUE){
  adjust = 1/(network_size-1)
  core_size <- network_size/2
  periphery_size <- network_size/2
  if (later == TRUE){
    p_cc = p
    p_pp <- (average_degree - (network_size - 1) * p_cc / 4) * (4/(3 * network_size -1))
    p_cp <- p_pp
  } else {
    p_pp = p
    p_cc <- (average_degree - (network_size -1) * p_pp / 4) * (4/(3 * network_size -1))
    p_cp <- p_cc
  }
  
  p_cc <- p_cc+adjust
  p_cp <- p_cp+adjust
  p_pp <- p_pp+adjust
  a = matrix(0, network_size, network_size)
  links = sample(seq(1,network_size,0.1), size = network_size * network_size, replace = T)
  links = matrix(links, ncol = network_size, byrow = T)
  threshold_ppp = p_pp * network_size
  threshold_pcp = p_cp * network_size
  threshold_pcc = p_cc * network_size
  
  for (i in 1:network_size){
    for (j in 1:network_size){
      if (i != j){
        if (i <= network_size/2){
          # left region (Core) in field
          if (j <= network_size/2){
            # left and upper region -> cc
            threshold <- threshold_pcc
          } else {
            # left and lower region -> cp
            threshold <- threshold_pcp
          }
        } else {
          # right reigion in field
          if (j <= network_size/2){
            # right upper region -> cp / pc
            threshold <- threshold_pcp
          } else {
            # right lower rigion -> pp
            threshold <- threshold_ppp
          }
        }
        
        if (links[i,j] < threshold){
          a[i,j] = 1
        }
        
      }
    }
  }
  
  G = graph_from_adjacency_matrix(a, mode = 'directed')
  return (G)
}


get_low_bound_cc <- function(average_degree, network_size = 1000){
  # this bound happened at the pp = cc
  pcc = average_degree/(network_size-0.5)
  return(pcc)
}

get_up_bound_cc <- function(average_degree, network_size = 1000){
  # this bound happened at the pp = 0
  ppp = 0
  pcc = (average_degree - ppp / (4*(3*network_size - 1)))*4/(network_size-1)
  return (pcc)
}

get_up_bound_pp <- function(average_degree, network_size = 1000){
  # this bound happened at the pp = cc
  ppp = average_degree/(network_size-0.5)
  return(ppp)
}

get_low_bound_pp <- function(average_degree, network_size = 1000){
  # this bound happened at the cc = 0
  pcc = 0
  ppp = (average_degree - pcc / (4*(3*network_size - 1)))*4/(network_size-1)
  return (ppp)
}


create_network <- function(network_size=1000, parameter, type , p = 0.003001501, print_out = FALSE){
  
  # first creat the graph without weights
  if (type == 'er'){
    # G <- sample_gnp(n=network_size, p=prob, TRUE)
    G <- ER_network(network_size, prob = parameter)
  } else if (type == 'sbm later'){
    G <- SBM_network(network_size, average_degree = parameter, p)
  } else if (type == 'sbm fomer'){
    G <- SBM_network(network_size, average_degree = parameter, p, later = FALSE)
  } else if (type == 'ba'){
    G <- sample_pa(n=network_size, m = parameter, power = 1, directed = TRUE)
  } else if (type == 'read'){
    G <- read_graph(file = parameter)
  } else {
    cat('The setting of the network type is wrong.')
  }
  
  M <- length(E(G))
  edge_df <- ends(G, E(G))
  if (print_out == TRUE) sprintf('The network have %s edges, hence the average degree is %s',
                                 M, M/network_size)
  
  # creat the list of weights
  edge_weight <- list(rep(0,M))
  #print('before')
  #print(edge_weight)
  for (i in 1:network_size) {
    if (print_out == TRUE){
      cat('node:')
      cat(i)
      cat('\n')
    }
    
    nodes_in <- neighbors(G, i, 'in')
    if (print_out == TRUE){
      cat('has neighbors:')
      cat(nodes_in)
      cat('\n')
    }
    
    number_of_in <- length(nodes_in)
    if (number_of_in != 0){
      this_weight <- AiIB / number_of_in
      ENDS = as.vector(rbind(as.numeric(nodes_in), i))
      SpecialEdges = get.edge.ids(G, ENDS)
      if (print_out == TRUE){
        cat('the edges need to change')
        cat(SpecialEdges)
        cat('\n')
      }
      E(G)$weight[SpecialEdges] = c(rep(this_weight, number_of_in))
      if (print_out == TRUE){
        cat('after')
        cat(E(G)$weight[SpecialEdges])
        cat('\n')
      }
    } else{
      this_weight <- 0
    }
  }
  if (print_out == TRUE){
    cat(E(G)$weight)
    cat('\n')
  }
  return (G)
}

