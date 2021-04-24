df <- read.csv('clustered.csv', row.names = 'X')

tapply(df$OP_NMU_EVER, df$DEM_STDNT,mean) 
tapply(df$ILL_USE, df$DEM_STDNT,mean)
# students are more likely to use opiods in a non-medical purpose and illicit drugs


tapply(df$OP_NMU_EVER, df$DEM_MARITAL,mean) 
tapply(df$ILL_USE, df$DEM_MARITAL,mean)
# SINGLE people are more likely to use illicit drugs 

# Rich people are less likely to use opiods 
plot(tapply(df$OP_NMU_EVER, df$DEM_INCOME,mean) * 100, type  = 'b', 
     xlab = "Income level (scale 1-9)", ylab = "Percentage of people that have used opiods in a non-medical context") # trend here


plot(tapply(df$ILL_USE, df$DEM_EDU,mean) * 100, type  = 'b')
plot(tapply(df$OP_NMU_NTY, df$DEM_EDU,mean) * 100, type  = 'b')

# no real significant difference between the groups 
tapply(df$OP_NMU_EVER, df$DEM_VET, mean)
tapply(df$ILL_USE, df$DEM_VET, mean)


# performing an ANOVA TEST to see how education influences whether or not a person uses opoids for non-medical purposes

model <- aov(df$OP_NMU_EVER ~  as.factor(df$DEM_EDU))

summary(model)

library(DescTools)
PostHocTest(model, method = "hsd", conf.level = 0.95)

