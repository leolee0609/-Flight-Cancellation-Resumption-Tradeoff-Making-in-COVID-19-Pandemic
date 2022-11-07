install.packages("Rcpp","tidyverse")
library(tidyverse)
install.packages("car")
library("car")

dataset <- read.csv("IIP_IIR.csv")
IIP <- dataset$IIP
IIR <- dataset$ICC


plot(IIP, IIR, main = "Estimate of Input Infected population by Flight (IIP)-Increased Infection Rate (IIR) Scatterplot",
     xlab = "IIP", ylab = "IIR",
     pch = 19, frame = FALSE)
# Add regression line
abline(lm(IIR ~ IIP, data = mtcars), col = "blue")


# Add labels
scatterplot(IIR ~ IIP, 
            main = "Outlier detection of IIP and IIR",
            smoother = FALSE, grid = FALSE,
            frame = FALSE, ellipse = TRUE)

cor(log(IIP), log(IIR))

