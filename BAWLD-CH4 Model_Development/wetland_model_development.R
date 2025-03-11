
# Code by McKenzie Kuhn
#BAWLD-CH4 wetland scaling model development

#loading in the needed packages 

library(ggpubr)
library(lme4)
library(ggeffects)
library(nlme)
library(dbplyr)
library(lmerTest)
library(boot)
library(MuMIn)
library(sjstats)
library(tidyr)
library(tidyverse)
library(car)
library(performance)
library(see)


#Loading in the metadata flux file (From kuhn et al. 2021 ESSD)
#Includes temperatures from worldclim2

wetlands_temps = read.csv("BAWLD_Wetland_Cover_Monthly_Temps.csv")
head(wetlands_temps)
attach(wetlands_temps)


#Best model (see model comparison below)
wetland.model = lmer(data = wetlands_temps, log.CH4 ~ Annual.Temp + Class + WTAv + (1|TotalID), weights = Occ.w, REML = TRUE)

summary(wetland.model)  #summary of significance 
r.squaredGLMM(wetland.model)  #checking the conditional and marginal R2 values (m= 0.58, c = 0.76)
confint(wetland.model) #confidence intervals used in the scaling process
rand(wetland.model)    
wet_hist = hist(resid(wetland.model),main = "Histogram of Residuals", xlab = "Residuals")


check_model(wetland.model, 
            check = c("linearity", "qq", "vif", "outliers", "normality"))

#Test for normality
shapiro.test(residuals(wetland.model))


###cross validation
#complete cases to get fluxes. 
attach(wetlands_temps)
df1 = data.frame(TotalID, log.CH4, Class, Occ.w, Annual.Temp, WTAv)
rdf1 = df1[complete.cases(df1), ]


## randomly selection 50 data rows for model comparison
set.seed(4)
random = rdf1[sample(nrow(rdf1), 50),]

random

test.data = random

dat2 = rdf1[-c(78, 375, 74, 239, 357, 274, 63, 142, 181, 217, 112, 371, 346,  48,
               252, 285, 210, 54, 293, 108, 259,68, 402, 358, 176, 385, 79, 62, 193, 345,
               398, 288, 292, 227, 366, 179, 43, 223, 22, 11, 26, 365, 201, 156,
               279, 337, 2, 390,35, 254), ]


# running the model on dataset without test data
class.model = lmer(data = dat2, log.CH4 ~ Annual.Temp + Class + WTAv + (1|TotalID),weights = Occ.w, REML = FALSE)

summary(class.model)  #summary of significance 
r.squaredGLMM(class.model)  #checking the conditional and marginal R2 values (m= 0.62, c = 0.82)
confint(class.model) #confidence intervals used in the scaling process
rand(class.model)           


m.predict = predict(class.model, newdata = test.data, allow.new.levels = TRUE, na.action = "na.omit")
test.data$model.output = m.predict
summary(lm(model.output~log.CH4, data = test.data)) # 83% R2, p-value < 0.0001

# scatter plot for model validation 

test.data$Class <- factor(test.data$Class, levels = c("PermBog","Bog",
                                                      "Fen","PermWet","Marshes"))

test.plot = ggplot(data = test.data, aes(x = log.CH4, y = model.output))+  
  geom_point(size = 5, shape = 21, aes(fill = Class)) +
  scale_fill_brewer(palette="YlGn")+ 
  scale_y_continuous(breaks=seq(0, 2, by = 0.5)) +
  scale_x_continuous(breaks=seq(0, 2, by = 0.5)) +
  theme_classic(base_size = 18) + theme(legend.position = c(0.15, 0.8), panel.border = element_rect(colour = "black", fill=NA))+ 
  geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed", size=1.25) +
  ylab(" ") + xlab(" ")

test.plot

#RSME

rmse <- sqrt(mean((test.data$model.output - test.data$log.CH4)^2))
rmse # 0.17



##### AIC model comparison ### 
### now testing with precipitation and other models 

obs2c <- wetlands_temps %>%
  select(TotalID, Class, Occ.w, WTAv, CD_Pcp_An, log.CH4, Annual.Temp, Occ.w, TsoilB)

obs2c1 = obs2c[complete.cases(obs2c), ]

class.model1 = lmer(data = obs2c1, log.CH4 ~ Class + (1|TotalID), weights = Occ.w, REML = TRUE)
class.model2 = lmer(data = obs2c1, log.CH4 ~ Annual.Temp + Class + (1|TotalID), weights = Occ.w, REML = TRUE)
class.model3 = lmer(data = obs2c1, log.CH4 ~ Annual.Temp + Class + WTAv + (1|TotalID), weights = Occ.w, REML = TRUE)
class.model4 = lmer(data = obs2c1, log.CH4 ~ Annual.Temp + Class + WTAv + CD_Pcp_An + (1|TotalID), weights = Occ.w, REML = TRUE)

AIC(class.model1, class.model2, class.model3,class.model4)

#class.model3 = best model 
