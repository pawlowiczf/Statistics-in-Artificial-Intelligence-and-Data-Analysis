setwd("C:/Users/Filip/Desktop/UMISI/Statistics-in-Artificial-Intelligence-and-Data-Analysis/report01")
getwd()

library(ordinal)

df <- read.csv("clm.data", header = TRUE)

df$id <- NULL
df$satysfakcja <- factor(df$satysfakcja, ordered = TRUE)

str(df)
summary(df)



model_clm <- clm(satysfakcja ~ cena + obsluga, data = df)

summary(model_clm)