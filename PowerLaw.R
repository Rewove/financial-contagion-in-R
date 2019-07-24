library(igraph)
set.seed(202)
g <- static.power.law.game(1000, 15000, exponent.out= 2.2, exponent.in = -1, loops = FALSE, multiple = TRUE, finite.size.correction = TRUE)
length(E(g))/length(V(g))

library(ggplot2)

data <- degree(g)
data <- data[data>0]

library(poweRlaw)
m_pl <- displ$new(data)
est_pl <- estimate_xmin(m_pl)

est_pl$xmin #k_min

m_pl$setXmin(est_pl)
plot.data <- plot(m_pl, draw = F)
fit.data <- lines(m_pl, draw = F)


plot(plot.data, log="xy",
     xlab="Degree",
     ylab="Cummulative Degree Distribution")
title("Power Law Network Degree Distribution")
