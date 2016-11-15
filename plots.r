#Age plots
plot(density(acs3$AGEP[acs3$manager == 1 & acs3$asian ==1]) , col = "red",
     main = "Managers Age by Race" , xlab = "Age (Years)" , lwd = 4)
lines(density(acs3$AGEP[acs3$manager == 1 & acs3$asian ==0]) , col = "blue" , lwd = 4)
legend("topleft" , c("white" , "asian"), fill = c("blue" , "red") , cex = .9)

ggplot(subset(acs3, manager ==1), aes(x = AGEP)) +   
  geom_density(aes(group = factor(asian), colour = factor(asian) ,
                   fill = factor(asian)) , alpha = 0.2  , size = 1.1) +
  labs(x = "Age" , y = "Density") 

ggplot(acs3, aes(x = AGEP)) +   
  geom_density(aes(group = factor(asian), colour = factor(asian) ,
                   fill = factor(asian)) , alpha = 0.2  , size = 1.1) +
  labs(x = "Age" , y = "Density") 

#Oaxaca Plots
plot.oaxaca(oaxaca , decomposition = "twofold" , weight = -1, components = "explained")


plot.oaxaca(oaxaca2 , decomposition = "twofold" , weight = -1, components = "explained")

