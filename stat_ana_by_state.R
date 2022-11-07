library(car)

dataset <- read.csv("IIP_IIR.csv")
States = c()
for (i in 1: 497) {
  if (dataset$State[i] %in% States == TRUE) {
    next
  }
  States = c(States, dataset$State[i])
}

df_list = list()
coe <- c()
i = 1
for (state in States) {
  df <- read.csv(paste0("df_", state, ".csv"))
  df_list[[i]] <- df
  pcc <- cor(df$OIPE, log(df$IRCC))
  coe <- c(coe, pcc)
  i = i + 1
}

log_coe <- c()
i = 1
for (state in States) {
  df <- read.csv(paste0("df_", state, ".csv"))
  df_list[[i]] <- df
  pcc <- cor(log(df$SIIP), log(df$IRCC))
  log_coe <- c(log_coe, pcc)
  i = i + 1
}


# iip qqplot
qqnorm(dataset$IIP, pch = 1, frame = FALSE)
qqline(dataset$IIP, col = "steelblue", lwd = 2)

# log_iip qqplot
qqnorm(log(dataset$IIP), pch = 1, frame = FALSE)
qqline(log(dataset$IIP), col = "steelblue", lwd = 2)

# ircc qqplot
qqnorm(dataset$IRCC, pch = 1, frame = FALSE)
qqline(dataset$IRCC, col = "steelblue", lwd = 2)

# log_ircc qqplot
qqnorm(log(dataset$IRCC), pch = 1, frame = FALSE)
qqline(log(dataset$IRCC), col = "steelblue", lwd = 2)

# ircc qqplot
qqnorm(dataset$IC, pch = 1, frame = FALSE)
qqline(dataset$IC, col = "steelblue", lwd = 2)

# log_ircc qqplot
qqnorm(log(dataset$IC), pch = 1, frame = FALSE)
qqline(log(dataset$IC), col = "steelblue", lwd = 2)

# siip qqplot
qqnorm(dataset$SIIP, pch = 1, frame = FALSE)
qqline(dataset$SIIP, col = "steelblue", lwd = 2)

# log_siip qqplot
qqnorm(log(dataset$SIIP), pch = 1, frame = FALSE)
qqline(log(dataset$SIIP), col = "steelblue", lwd = 2)

pairs(dataset[,5:8], pch = 19)

pairs(log(dataset[,5:8]), pch = 19)

df_Alabama <- read.csv("df_Alabama.csv")
cor(df_Alabama$IIP[1:8], df_Alabama$IC[2:9])


df_Illinois <- read.csv("df_Illinois.csv")
cor(df_Illinois$IIP[1:8], df_Illinois$IC[2:9])


cor(dataset$SIIP, dataset$IRCC)


SIIP = c()
IRCC = c()
for (i in 1: 497) {
  month = dataset$Month[i]
  state = dataset$State[i]
 
  SIIP = c(SIIP, dataset$SIIP[i])
  
  exi = FALSE
  
  for (j in 1: 497) {
    if (dataset$State[j] == state && dataset$Month[j] == month + 1) {
      IRCC = c(IRCC, dataset$IRCC[j])
      exi = TRUE
    }
  }
  
  if (exi == FALSE) {
    SIIP = SIIP[1:length(SIIP)-1]
  }
}

# linear regression
i = 1
for (state in States) {
  df <- read.csv(paste0("df_", state, ".csv"))
  df_list[[i]] <- df
  lm <- lm(log(IRCC)~log(SIIP), data = df)
  print(summary(lm))
  i = i + 1
}

lm <- lm(log(IRCC)~SNIIP, data = dataset)
print(summary(lm))

lm_who <- lm(log(IRCC)~log(SOIPE), data = dataset)
print(summary(lm_who))

scatterplot(log(dataset$SOIPE), log(dataset$IRCC))


df_NY <- read.csv("df_New York.csv")
cor(df_NY$SNIIP, df_NY$IRCC)
