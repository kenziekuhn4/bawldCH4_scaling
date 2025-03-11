
# Code by McKenzie Kuhn
#BAWLD-CH4 lake scaling model development

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

#Loading in the metadata flux file (From kuhn et al. 2021 ESSD)

dat4 = read.csv("lakes_bawld.csv", header= TRUE, na.strings = "-")


#### testing summer temperatures vs MAAT against water temperature. 

lake2 = dat4[, c("log.CH4.D.FLUX", "CD_Pcp_An", "ANN_T",
                 "TEMP", "SUM_T", "CLASS")]


lake2 = lake2[complete.cases(lake2), ]


## Testing annual vs summer time temperature
MAAT.Model = lm(data =lake2, TEMP ~ ANN_T, REML = TRUE)
summer.Model = lm(data = lake2, TEMP ~ SUM_T, REML = TRUE)

anova(MAAT.Model, summer.Model)


## Comparing models

lake.model1 = lm(data = lake2, log.CH4.D.FLUX ~ CLASS + SUM_T)

lake.model2 = lm(data = lake2, log.CH4.D.FLUX ~ CLASS +ANN_T)# Best model for diffusion

lake.model3 = lm(data = lake2, log.CH4.D.FLUX ~ CLASS +ANN_T + CD_Pcp_An)

AIC(lake.model1,lake.model2,lake.model3)

anova(lake.model2, lake.model1)


###### best model with annual temp #############
class.m.sum = lm(data = dat4, log.CH4.D.FLUX ~ CLASS + ANN_T) 
summary(class.m.sum)  #summary of significance 
r.squaredGLMM(class.m.sum) # R2 = o0.44

lake_residuals = resid(class.m.sum)
hist(lake_residuals, breaks = 10)
check_model(class.m.sum, 
            check = c("linearity", "qq", "vif", "outliers", "normality",))

### Model validation
attach(dat4)
df2 = data.frame(SITE, YEAR.S, log.CH4.D.FLUX, ANN_T, CLASS)
rdf2 = df2[complete.cases(df2), ]


## randomly selection 50 data rows for model comparison

#random1 = rdf2[sample(nrow(rdf2), 50),]

dat5 = rdf2[-c(164,111,185,176,115,184,119,71,264,270,231,23,112,157,154,233,128,181,
               106,102,248,265,178,110,25,241,113,172,131,243,195,166,37,220,74,215,
               217,212,136,224,122,130,210,232,33,137,169,17,244,68), ]

dat6 = rdf2[c(164,111,185,176,115,184,119,71,264,270,231,23,112,157,154,233,128,181,
              106,102,248,265,178,110,25,241,113,172,131,243,195,166,37,220,74,215,
              217,212,136,224,122,130,210,232,33,137,169,17,244,68), ]


class.model = lm(data = dat5, log.CH4.D.FLUX ~ CLASS + ANN_T)
summary(class.model)

m.predict.l = predict(class.model, newdata = dat6,  na.action = "na.omit")

m.predict.l

dat6$model.output = m.predict.l


summary(lm(model.output~log.CH4.D.FLUX, data = dat6)) # 42% R2 
rmse <- sqrt(mean((dat6$model.output - dat6$log.CH4.D.FLUX)^2))
rmse # 0.57

# a plot with the test data


test.plot.l = ggplot(data = dat6, aes(x = log.CH4.D.FLUX, y = model.output))+ 
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1) +  
  geom_point(size = 5, shape = 21, aes(fill = CLASS)) +
  scale_y_continuous(breaks=seq(0, 2, by = 1)) +
  xlim(-0.5,2.5) +
  scale_fill_brewer(palette="PuBu")+ 
  theme_classic() + 
  theme(legend.position = c(0.1, 0.8), panel.border = element_rect(colour = "black", fill=NA))

test.plot.l

