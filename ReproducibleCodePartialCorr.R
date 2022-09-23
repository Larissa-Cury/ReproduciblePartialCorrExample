
library(ppcor)
library(RVAideMemoire) ## alternative corr function
library(tidyverse)

### 

data <- read_csv("mydata.csv")

### filter Nas (pairwise)

data1 <- data %>% 
  filter(!is.na(X1) & !is.na(X2))

### run partial corr: 
# rho = 0.096 (Pearson) # rho = 0.11 "spearman" 
### - Question: HOW CAN i plot the Spearman's correlation?

pcor.test(data1$X1, data1$X2, data1$Z, method = "spearman")

### I want to see the corr between X1 and X2 controlling for Z: 

# m1 <- lm(X1 ~ X2 + Z, data = data1)

### now I'm ploting the isolated lms getting the sd. residuals:

rX1 <-  rstandard(lm(X1 ~ Z, data = data1))
rX2 <- rstandard(lm(X2 ~ Z, data = data1))

### the second plot:

m2 <- lm(rX1 ~ rX2)

### plot :

plot(rX1, rX2)
abline(m2, col = "red", lwd = 2)  ### the slope is ascending

### store in a df to plot it do use ggplot2

dataRes <- data.frame(res1 = rX1,
                      res2 = rX2)

### using ggplot2: 

dataRes %>% 
  ggplot(., aes(x = res2, y = res1)) +
  geom_point(alpha = 0.1, color = "black") +
  stat_smooth(formula = y ~ x, method = lm, color = "blue", se=F) +
  # stat_cor(aes(label =paste(label, cut(..p.., 
  #                                      breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
  #                                      labels = c("'***'", "'***'", "'**'", "'*'", "'ns'")), 
  #                           sep = "~")), method="pearson", cor.coef.name = c("r"), 
  #          label.sep = ", ",hjust = -1.0, size = 5) +
  labs(x = "Variable 2",
       y = "Variable 1",
       title= "Partial Correlation between Var 1 and Var2 while controling for Var Z") +
  theme_bw()

### Question: I'm used to use ggpubr to plot canonical corr in a plot, any ideas on how to plot a partial
### correlation in the same way?
