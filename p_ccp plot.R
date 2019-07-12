# install.packages("plot3D")
# library(plot3D)

network_size = 1000
average_degree1 = 0
average_degree2 = 10
#p_cc <- seq(0,1,0.01)
p_pp1 <- (average_degree1 - (network_size - 1) * p_cc / 4) * (4/(3 * network_size -1))
p_pp2 <- (average_degree2 - (network_size - 1) * p_cc / 4) * (4/(3 * network_size -1))


#scatter3D(average_degree, p_cc, p_pp)
plot(p_cc, p_pp1)
points(p_cc, p_pp2)

p_cc <- 0.003
average_degree1 = 3
network_size =1000
p_pp <- (average_degree1 - (network_size - 1) * p_cc / 4) * (4/(3 * network_size -1))
p_pp

ppp = 0
average_degree = 7
pcc = (average_degree - ppp / (4*(3*network_size - 1)))*4/(network_size-1)
pcc
# z=3 0.012, z=10, 0.0400, z=7 0.0280
pcc = seq(0,0.012, 0.0005)
network_size = 1000
average_degree = 3
ppp <- (average_degree - (network_size - 1) * pcc / 4) * (4/(3 * network_size -1))
plot(pcc, ppp)
abline(a=0,b=1)

# z = 3
pcc = seq(0.003, 0.012, 0.0005)
network_size = 1000
average_degree = 3
ppp <- (average_degree - (network_size - 1) * pcc / 4) * (4/(3 * network_size -1))
plot(pcc, ppp)
abline(a=0,b=1)
