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



# the final ER
file_name = 'FINAL_ER.csv'
x = seq(0,10.1,0.2)

data = read.csv(file_name, header = FALSE)
y1 = as.numeric(data$V1)
y2 = as.numeric(data$V2)

xtick = seq(0,10,1)

plot(x, y1,  pch=4, ylim=c(0,1.03),
     ylab = 'Probability and Extent of Contagion',
     xlab = 'Average Degree (Connectivity)', xaxt = "n")
axis(side = 1, at = xtick, labels=xtick )
grid()
points(x, y2, pch=16)
arrows(0.8,1.02,7,1.02,code=3,length = 0.25, angle = 10)
text(4,1.04,"Contagion Window")
abline(v=0.8,lty=2)
abline(v=7,lty=2)
legend(8.5,1.1,c("Contagion\nProbability\n", "Contagion\nExtent"), pch=c(4,16),bty="n")
title(main='Probability and Extent of Contagion for ER Network', 
      sub='Randomly Chose One Bank Bankrupt')


# the final ER as line 
# plot Biggest
file_name = 'FINAL_ER.csv'
x = seq(0,10.1,0.2)

data = read.csv(file_name, header = FALSE)
y1 = as.numeric(data$V1)
y2 = as.numeric(data$V2)
xtick = seq(0,10,1)
#y1 line for probability
#y2 dashlien for extent
plot(x, y1,  type="l", lty=1, ylim=c(0,1.03),
     ylab = 'Probability and Extent of Contagion',
     xlab = 'Average Degree (Connectivity)', xaxt = "n")
axis(side = 1, at = xtick, labels=xtick )
grid()
points(x, y2, type="l", lty=2)

second_file_name = "results_er_biggest.csv"
second_data = read.csv(second_file_name, header = FALSE)
prob = as.numeric(second_data$V1)
exte = as.numeric(second_data$V2)

points(x, prob, pch=4)
points(x, exte, pch=16)

arrows(0.6,1.02,7.8,1.02,code=3,length = 0.25, angle = 10)
text(4.5,1.04,"Contagion Window")
abline(v=0.8,lty=2)
abline(v=7,lty=2)
legend(8.2,1,c("Contagion\nProbability\n", "Contagion\nExtent"), pch=c(4,16), lty=c(1,2),bty="n")
title(main='Probability and Extent of Contagion for ER Network', 
      sub='The Bank with the Biggest Connectivity Bankrupt')

# the final ER as line 
# plot TARGET
file_name = 'FINAL_ER.csv'
x = seq(0,10.1,0.2)

data = read.csv(file_name, header = FALSE)
y1 = as.numeric(data$V1)
y2 = as.numeric(data$V2)
xtick = seq(0,10,1)
#y1 line for probability
#y2 dashlien for extent
plot(x, y1,  type="l", lty=1, ylim=c(0,1.03),
     ylab = 'Probability and Extent of Contagion',
     xlab = 'Average Degree (Connectivity)', xaxt = "n")
axis(side = 1, at = xtick, labels=xtick )
grid()
points(x, y2, type="l", lty=2)

second_file_name = "results_er_target_0.1.csv"
second_data = read.csv(second_file_name, header = FALSE)
prob = as.numeric(second_data$V1)
exte = as.numeric(second_data$V2)

points(x, prob, pch=4)
points(x, exte, pch=16)

arrows(1,0.4,5,0.4,code=3,length = 0.25, angle = 10)
text(3,0.425,"Contagion Window")
abline(v=0.8,lty=2)
abline(v=7,lty=2)
legend(8.2,1,c("Contagion\nProbability\n", "Contagion\nExtent"), pch=c(4,16), lty=c(1,2),bty="n")
title(main='Probability and Extent of Contagion for ER Network', 
      sub='The Bank with the Target Policy')



# SBM
plot_sbm <- function(average_degree = 2){
  file_name = paste("results_sbm_",average_degree,".csv", sep='')
  up = round(get_low_bound_cc(average_degree),3)
  down = round(get_up_bound_cc(average_degree),3)
  x = seq(up, down, 0.0005)
  xtick = seq(up, down, 0.001)
  data = read.csv(file_name, header = FALSE)
  y1 = as.numeric(data$V1)
  y2 = as.numeric(data$V2)
  
  plot(x, y1,  pch=4, ylim=c(0,1.0),
       ylab='',
       xlab = 'p_cc', xaxt = "n",
       main=paste("Average Degree: ",average_degree ))
  axis(side = 1, at = xtick, labels= xtick)
  grid()
  points(x, y2, pch=16)
}

par(mfrow=c(2,3))
for (i in 2:7){
  plot_sbm(i)
}
mtext(expression(bold("SBM Networks Simulation on Different Average Degrees")), side = 3, line = 0, outer = T)


# SBM cc=cp
plot_sbm <- function(average_degree = 2){
  file_name = paste("results_sbm_f_",average_degree,".csv", sep='')
  down = round(get_up_bound_pp(average_degree),3)
  x = seq(0, down, 0.0005)
  xtick = seq(0, down, 0.001)
  data = read.csv(file_name, header = FALSE)
  y1 = as.numeric(data$V1)
  y2 = as.numeric(data$V2)
  
  plot(x, y1,  pch=4, ylim=c(0,1.0),
       ylab='',
       xlab = 'p_pp', xaxt = "n",
       main=paste("Average Degree: ",average_degree ))
  axis(side = 1, at = xtick, labels= xtick)
  grid()
  points(x, y2, pch=16)
}

par(mfrow=c(2,3))
mtext(expression(bold("SBM Networks Simulation on Different Average Degrees")), side = 3, line = 0)
for (i in 2:7){
  plot_sbm(i)
}
mtext(expression(bold("SBM Networks Simulation on Different Average Degrees")), side = 3, line = 0, outer =T)
