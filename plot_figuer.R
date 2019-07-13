plot_the_figure <- function(x, y1, y2, xlab, network_name = '', notes = ''){
  # xlab = 'p_cc: Probability of Core Connectivity'
  # xlab = 'Average Degree (Connectivity)'
  # notes = 'at z=7'
  plot(x, y1, pch=4, ylim=c(0,1),
       ylab = 'Probability and Extent of Contagion',
       xlab = xlab)
  points(x, y2, pch=16)
  maintitle = paste('Probability and Extent of Contagion for', network_name)
  subtitle = paste('Random Choose One Bank Bankrupt', notes)
  title(main=maintitle, 
        sub=subtitle)
}

