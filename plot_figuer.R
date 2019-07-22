plot_the_figure <- function(x, y1, y2, xlab = '', network_name = '', notes = ''){
  # xlab = 'p_cc: Probability of Core Connectivity'
  # xlab = 'Average Degree (Connectivity)_'
  # notes = 'at z=7'
  plot(x, y1, pch=4, ylim=c(0,1),
       ylab = 'Probability and Extent of Contagion',
       xlab = xlab)
  points(x, y2, pch=16)
  maintitle = paste('Probability and Extent of Contagion for', network_name)
  subtitle = paste(notes)
  title(main=maintitle, 
        sub=subtitle)
}

file_name = 'results_sbm_3.csv'
average_degree = 3
# the csv file need changed with add comma
data = read.csv(file_name, header =FALSE)
x = seq(get_low_bound_cc(average_degree), get_up_bound_cc(average_degree), 0.0005)
plot_the_figure(x, as.numeric(data$V1),as.numeric(data$V2),
                xlab = 'p_cc: Probability of Core Connectivity',
                network_name = 'SBM Network', notes = 'at z=3')