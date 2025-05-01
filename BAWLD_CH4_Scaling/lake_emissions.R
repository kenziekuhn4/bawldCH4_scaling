

####Current Lake and River Emissions #######


#loading the required packages

library(ggplot2)   # for general plotting
library(tidyverse)
library(matrixStats)
library(rgdal)     # R wrapper around GDAL/OGR
library(sf)        #for simple features 



#reading in the shape file. BAWLD Landcover model with land cover distributions

shape = read.csv("bawld_full_lakes.csv", header = TRUE)

head(shape)



#calculating lake percent and areas similar to terrestrial class above. 
LAL_P = shape$LAL/100
MPL_P = shape$MPL/100
MYL_P = shape$MYL/100
MGL_P = shape$MGL/100
SPL_P = shape$SPL/100
SYL_P = shape$SYL/100
SGL_P = shape$SGL/100

LAL_area= shape$Shape_Area*LAL_P
MPL_area= shape$Shape_Area*MPL_P
MYL_area= shape$Shape_Area*MYL_P
MGL_area= shape$Shape_Area*MGL_P
SPL_area= shape$Shape_Area*SPL_P
SYL_area= shape$Shape_Area*SYL_P
SGL_area= shape$Shape_Area*SGL_P

LAL_km = sum(LAL_area)/1000000/1000000
MPL_km =sum(MPL_area)/1000000/1000000
MYL_km =sum(MYL_area)/1000000/1000000
MGL_km =sum(MGL_area)/1000000/1000000
SPL_km =sum(SPL_area)/1000000/1000000
SYL_km =sum(SYL_area)/1000000/1000000
SGL_km =sum(SGL_area)/1000000/1000000

total_a = (LAL_km+MPL_km+MYL_km+MGL_km+SPL_km+SYL_km +SGL_km)
total_a # 1.44

###

##coefficient code used to create the coefficients csv file below

# set the shape file to a dataframe for manipulations 

df = as.data.frame(shape)
# 
# df_coefsA <- data.frame(matrix(0, ncol = 1, nrow = 200))
# 
# # making the coefficients vary 
# 
# #intercept
# FIA = 0.64
# FIA_H = 1.13
# FIA_L = 0.20
# 
# 
# df_coefsA$FIA = runif(200)
# 
# x_FIA = df_temp$I
# 
# m = NULL;
# for (I in 1:length(x_FIA)) {
#   coef_FIA = if (I <0.5){
#     print(rnorm(1,0.64,0.22))
#     break 
#     
#   } else { print (rnorm(1,0.64, 0.24))
#   }
#   m = rbind(m, coef_FIA)
# }
# 
# df_coefsA$FIA_coef = m[,1]
# head(df_coefsA)
# 
# 
# #temperature coefficient
# FTCA = 0.042
# FTCA_H = 0.06
# FTCA_L = 0.027
# 
# 
# df_coefsA$FTCA = runif(200)
# 
# x_FTCA = df_temp$I
# 
# m = NULL;
# for (I in 1:length(x_FTCA)) {
#   coef_FTCA = if (I <0.5){
#     print(rnorm(1,0.042,0.0075))
#     break 
#     
#   } else { print (rnorm(1,0.042, 0.009))
#   }
#   m = rbind(m, coef_FTCA)
# }
# 
# df_coefsA$FTCA_coef = m[,1]
# head(df_coefsA)
# 
# 
# 
# #class intercepts high low and central 
# LALCoef = 0
# LALCoef_H = 0
# LALCoef_L = 0
# 
# # midsize glacial lakes 
# MGLCoef = 0.38
# MGLCoef_H = 0.86
# MGLCoef_L = -0.13
# 
# df_coefsA$MGL = runif(200)
# 
# x_MGL = df_coefsA$MGL
# 
# m = NULL;
# for (I in 1:length(x_MGL)) {
#   coef_MGL = if (I <0.5){
#     print(rnorm(1,0.38,0.25))
#     break 
#     
#   } else { print (rnorm(1,0.38, 0.24))
#   }
#   m = rbind(m, coef_MGL)
# }
# 
# df_coefsA$MGL_coef = m[,1]
# head(df_coefsA)
# 
# #midsize peatland lakes 
# 
# MPLCoef = 0.98
# MPLCoef_H = 1.51
# MPLCoef_L = 0.39
# 
# df_coefsA$MPL = runif(200)
# 
# x_MPL = df_coefsA$MPL
# 
# m = NULL;
# for (I in 1:length(x_MPL)) {
#   coef_MPL = if (I <0.5){
#     print(rnorm(1,0.38,0.25))
#     break 
#     
#   } else { print (rnorm(1,0.38, 0.24))
#   }
#   m = rbind(m, coef_MPL)
# }
# 
# df_coefsA$MPL_coef = m[,1]
# head(df_coefsA)
# 
# #####
# 
# #midsize yedoma lakes 
# MYLCoef = 0.38
# MYLCoef_H = 1.06
# MYLCoef_L = -0.13
# 
# df_coefsA$MYL = runif(200)
# 
# x_MYL = df_coefsA$MYL
# 
# m = NULL;
# for (I in 1:length(x_MYL)) {
#   coef_MYL = if (I <0.5){
#     print(rnorm(1,0.38,0.25))
#     break 
#     
#   } else { print (rnorm(1,0.38, 0.34))
#   }
#   m = rbind(m, coef_MYL)
# }
# 
# df_coefsA$MYL_coef = m[,1]
# head(df_coefsA)
# 
# #small glacial lakes 
# 
# SGLCoef = 0.06
# SGLCoef_H = -0.13
# SGLCoef_L = 0.56
# 
# df_coefsA$SGL = runif(200)
# 
# x_SGL = df_coefsA$SGL
# 
# m = NULL;
# for (I in 1:length(x_SGL)) {
#   coef_SGL = if (I <0.5){
#     print(rnorm(1,0.06,0.095))
#     break 
#     
#   } else { print (rnorm(1,0.06, 0.35))
#   }
#   m = rbind(m, coef_SGL)
# }
# 
# df_coefsA$SGL_coef = m[,1]
# head(df_coefsA)
# 
# #small peatland lakes 
# 
# 
# SPLCoef = 1.13
# SPLCoef_H = 1.59
# SPLCoef_L = 0.62
# 
# df_coefsA$SPL = runif(200)
# 
# x_SPL = df_coefsA$SPL
# 
# m = NULL;
# for (I in 1:length(x_SPL)) {
#   coef_SPL = if (I <0.5){
#     print(rnorm(1,1.13,0.255))
#     break 
#     
#   } else { print (rnorm(1,1.13, 0.23))
#   }
#   m = rbind(m, coef_SPL)
# }
# 
# df_coefsA$SPL_coef = m[,1]
# head(df_coefsA)
# 
# #small yedoma lakes 
# 
# SYLCoef = 1.34
# SYLCoef_H = 1.95
# SYLCoef_L = 0.75
# 
# df_coefsA$SYL= runif(200)
# 
# x_SYL = df_coefsA$SYL
# 
# m = NULL;
# for (I in 1:length(x_SYL)) {
#   coef_SYL = if (I <0.5){
#     print(rnorm(1,1.13,0.255))
#     break 
#     
#   } else { print (rnorm(1,1.13, 0.23))
#   }
#   m = rbind(m, coef_SYL)
# }
# 
# df_coefsA$SYL_coef = m[,1]
# head(df_coefsA)


#creating a dataframe for just the coefficient outputs

#df_coefsA1 = df_coefsA[,c(3,5,7,9,11,13,15,17)]

#head(df_coefsA1)

#write.csv(df_coefsA1, "df_coefs_lake.csv")

### loading in the coefficients

df_coefsA1 = read.csv("df_coefs_lake.csv", header = TRUE)

#setting shape to df

df = shape 

### temperature addtions (0 is current)
df$CD_Tmp_An = df$CD_Tmp_An + 0

head(df$CD_Tmp_An)



#full daily CH4 equation for aquatic systems. Full equation is being exponentially transformed to unlog the result

#daily aquatic emissions per class per cell (mg CH4 m-2 d-1). Not linked to area or percent cover per cell yet 

#example of terrestrial equation
#df$MarD_CH4_1 = 10^(df_coefs$FI_coef[1] +df_coefs$FTC_coef[1]*dfT1$MarTs + df_coefs$MAR_coef[1])-10

#midsize glacial lake 
for (i in 1:200) {
  df[[paste0("MGLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$CD_Tmp_An + df_coefsA1$MGL_coef[i])
}

match("MGLD_CH4_1",names(df)) #176
match("MGLD_CH4_200",names(df)) #375

MGL_df = df[, c(176:375)]

head(MGL_df)

#MPL 

for (i in 1:200) {
  df[[paste0("MPLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$CD_Tmp_An + df_coefsA1$MPL_coef[i])
}

match("MPLD_CH4_1",names(df)) #376
match("MPLD_CH4_200",names(df)) #575

MPL_df = df[, c(376:575)]

head(MPL_df)

#MYL 

for (i in 1:200) {
  df[[paste0("MYLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$CD_Tmp_An + df_coefsA1$MYL_coef[i])
}

match("MYLD_CH4_1",names(df)) #576
match("MYLD_CH4_200",names(df)) #775

MYL_df = df[, c(576:775)]

head(MYL_df)

##small glacial lakes

for (i in 1:200) {
  df[[paste0("SGLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$CD_Tmp_An + df_coefsA1$SGL_coef[i])
}


match("SGLD_CH4_1",names(df)) #776
match("SGLD_CH4_200",names(df)) #975

SGL_df = df[, c(776:975)]

head(SGL_df)

#small peatland lakes 

for (i in 1:200) {
  df[[paste0("SPLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$CD_Tmp_An + df_coefsA1$SPL_coef[i])
}


match("SPLD_CH4_1",names(df)) #976
match("SPLD_CH4_200",names(df)) #10275

SPL_df = df[, c(976:1175)]

head(SPL_df)

## Small yedoma lakes ##

for (i in 1:200) {
  df[[paste0("SYLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$CD_Tmp_An + df_coefsA1$SYL_coef[i])
}

match("SYLD_CH4_1",names(df)) #1176
match("SYLD_CH4_200",names(df)) #1375

SYL_df = df[, c(1176:1375)]

head(SYL_df)

## large lakes 

for (i in 1:200) {
  df[[paste0("LALD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$CD_Tmp_An)
}

match("LALD_CH4_1",names(df)) #1376
match("LALD_CH4_200",names(df)) #1575

LAL_df = df[, c(1376:1575)]

head(LAL_df)

#turn all of the dataframes into matrices to add them together

MGL_matrix = as.matrix(MGL_df)
MPL_matrix = as.matrix(MPL_df)
MYL_matrix = as.matrix(MYL_df)
SGL_matrix = as.matrix(SGL_df)
SPL_matrix = as.matrix(SPL_df)
SYL_matrix = as.matrix(SYL_df)
LAL_matrix = as.matrix(LAL_df)


# calculating the class open water season fluxes as separate columns in mg m-2

df$IFD = (df$IFD + 0)

head(df$IFD)


# calculating the diffusive fluxes 

shape$MGLflux = rowMeans(MGL_matrix)
shape$MPLflux = rowMeans(MPL_matrix)
shape$MYLflux = rowMeans(MYL_matrix)
shape$SGLflux = rowMeans(SGL_matrix)
shape$SPLflux = rowMeans(SPL_matrix)
shape$SYLflux = rowMeans(SYL_matrix)
shape$LALflux = rowMeans(LAL_matrix)

shape$MGLflux[shape$MGLflux<0] <- 0
shape$MPLflux[shape$MPLflux<0] <- 0
shape$MYLflux[shape$MYLflux<0] <- 0
shape$SGLflux[shape$SGLflux<0] <- 0
shape$SYLflux[shape$SYLflux<0] <- 0
shape$LALflux[shape$LALflux<0] <- 0
shape$SPLflux[shape$SPLflux<0] <- 0


#standard deviations

shape$MGLfluxsd = rowDiffs(MGL_matrix)
shape$MPLfluxsd = rowDiffs(MPL_matrix)
shape$MYLfluxsd = rowDiffs(MYL_matrix)
shape$SGLfluxsd = rowDiffs(SGL_matrix)
shape$SPLfluxsd = rowDiffs(SPL_matrix)
shape$SYLfluxsd = rowDiffs(SYL_matrix)
shape$LALfluxsd = rowDiffs(LAL_matrix)

shape$MGLfluxsd[shape$MGLfluxsd<0] <- 0
shape$MPLfluxsd[shape$MPLfluxsd<0] <- 0
shape$MYLfluxsd[shape$MYLfluxsd<0] <- 0
shape$SGLfluxsd[shape$SGLfluxsd<0] <- 0
shape$SYLfluxsd[shape$SYLfluxsd<0] <- 0
shape$LALfluxsd[shape$LALfluxsd<0] <- 0
shape$SPLfluxsd[shape$SPLfluxsd<0] <- 0


shape$MGLflux95 = 1.96*(shape$MGLfluxsd/14.1)
shape$MPLflux95 = 1.96*(shape$MPLfluxsd/14.1)
shape$MYLflux95 = 1.96*(shape$MYLfluxsd/14.1)
shape$SGLflux95 = 1.96*(shape$SGLfluxsd/14.1)
shape$SPLflux95 = 1.96*(shape$SPLfluxsd/14.1)
shape$SYLflux95 = 1.96*(shape$SYLfluxsd/14.1)
shape$LALflux95 = 1.96*(shape$LALfluxsd/14.1)


#calculating total flux by adding ebullition ratio
#0.80 for yed and 0.62 for peatlands


(100/(100-62)) #2.63 for non-yed

(100/(100-80)) #5 for yedoma lakes

shape$MGLfluxt = shape$MGLflux*2.63
shape$MPLfluxt = shape$MPLflux*2.63
shape$MYLfluxt = shape$MYLflux*5
shape$SGLfluxt = shape$SGLflux*2.63
shape$SPLfluxt = shape$SPLflux*2.63
shape$SYLfluxt = shape$SYLflux*5
shape$LALfluxt = shape$LALflux

shape$MGLfluxth = (shape$MGLflux+shape$MGLflux95)
shape$MPLfluxth = (shape$MPLflux+shape$MGLflux95)*2.63
shape$MYLfluxth = (shape$MYLflux+shape$MGLflux95)*5
shape$SGLfluxth = (shape$SGLflux+shape$MGLflux95)*2.63
shape$SPLfluxth = (shape$SPLflux+shape$MGLflux95)*2.63
shape$SYLfluxth = (shape$SYLflux+shape$MGLflux95)*5
shape$LALfluxth = shape$LALflux+shape$MGLflux95

shape$MGLfluxtl = (shape$MGLflux-shape$MGLflux95)
shape$MPLfluxtl= (shape$MPLflux-shape$MGLflux95)*2.63
shape$MYLfluxtl = (shape$MYLflux-shape$MGLflux95)*5
shape$SGLfluxtl = (shape$SGLflux-shape$MGLflux95)*2.63
shape$SPLfluxtl = (shape$SPLflux-shape$MGLflux95)*2.63
shape$SYLfluxtl = (shape$SYLflux-shape$MGLflux95)*5
shape$LALfluxtl = shape$LALflux-shape$MGLflux95

#totals for midsize glacial lakes and large larges
#only the 3 meters from the shore is considered for ebulltion in these lakes

#loading in the dataset that includes shore_length

join = read.csv("join_shore_line1.csv")
head(join)

# creating the areas for shorelength from large lakes 


shape$LL_shore_area =(3*(join$HL_SHORE_L*(join$HL_FRAC_L/100)))

head(join$HL_SHORE_L)
head(join$HL_FRAC_L)
head(shape$LL_shore_area)



# totals for small lakes, yedoma lakes and peatland lakes

MPL_an = (sum(shape$MPLfluxt*MPL_P*df$IFD*shape$Shape_Area))*1e-15
MPL_an
MYL_an = (sum(shape$MYLfluxt*MYL_P*df$IFD*shape$Shape_Area))*1e-15
MYL_an
MGL_an = ((sum(shape$MGLflux*MGL_P*df$IFD*shape$Shape_Area))+
            (sum((shape$MGLflux*2.63)*df$IFD*shape$LL_shore_area, na.rm = TRUE)))*1e-15
MGL_an
SPL_an = (sum(shape$SPLfluxt*SPL_P*df$IFD*shape$Shape_Area))*1e-15
SPL_an
SGL_an = (sum(shape$SGLfluxt*SGL_P*df$IFD*shape$Shape_Area))*1e-15
SGL_an
SYL_an = (sum(shape$SYLfluxt*SYL_P*df$IFD*shape$Shape_Area))*1e-15
SYL_an
LAL_an = ((sum(shape$LALflux*LAL_P*df$IFD*shape$Shape_Area))+
            (sum((shape$LALflux*2.63)*df$IFD*shape$LL_shore_area, na.rm = TRUE)))*1e-15
LAL_an


#open water emissions
# To calculate total annual emissions, apply a diel fact of 0.85 (Sciezcko et al. 2021)
# And an ice out factor of 1.3 (Wik et al. 2016)

total_an = sum(MPL_an, MYL_an, MGL_an, SPL_an, SGL_an, SYL_an, LAL_an)
total_an

### area uncertainty 
#calculating low end lake area uncertainty 
LAL_P_L = shape$LAL_L/100
MPL_P_L = shape$MPL_L/100
MYL_P_L = shape$MYL_L/100
MGL_P_L = shape$MGL_L/100
SPL_P_L = shape$SPL_L/100
SYL_P_L = shape$SYL_L/100
SGL_P_L = shape$SGL_L/100
RIV_P_L = shape$RIV_L/100

MPL_an_L = (sum(shape$MPLfluxt*MPL_P_L*df$IFD*shape$Shape_Area))*1e-15
MPL_an_L
MYL_an_L = (sum(shape$MYLfluxt*MYL_P_L*df$IFD*shape$Shape_Area))*1e-15
MYL_an_L
MGL_an_L= ((sum(shape$MGLflux*MGL_P_L*df$IFD*shape$Shape_Area))+
             (sum((shape$MGLflux*2.63)*df$IFD*shape$LL_shore_area, na.rm = TRUE)))*1e-15
MGL_an_L
SPL_an_L = (sum(shape$SPLfluxt*SPL_P_L*df$IFD*shape$Shape_Area))*1e-15
SPL_an_L
SGL_an_L = (sum(shape$SGLfluxt*SGL_P_L*df$IFD*shape$Shape_Area))*1e-15
SGL_an_L
SYL_an_L = (sum(shape$SYLfluxt*SYL_P_L*df$IFD*shape$Shape_Area))*1e-15
SYL_an_L
LAL_an_L = ((sum((shape$LALflux)*LAL_P_L*df$IFD*shape$Shape_Area))+
              (sum(((shape$LALflux)*2.6)*df$IFD*shape$LL_shore_area_m, na.rm = TRUE)))*1e-15
LAL_an_L

total_an_L = sum(MPL_an_L, MYL_an_L, MGL_an_L, SPL_an_L, SGL_an_L, SYL_an_L, LAL_an_L)
total_an_L


### high area

#calculating high end lake area uncertainty . 
LAL_P_H = shape$LAL_H/100
MPL_P_H = shape$MPL_H/100
MYL_P_H = shape$MYL_H/100
MGL_P_H = shape$MGL_H/100
SPL_P_H = shape$SPL_H/100
SYL_P_H = shape$SYL_H/100
SGL_P_H = shape$SGL_H/100
RIV_P_H = shape$RIV_H/100

MPL_an_H = (sum(shape$MPLfluxt*MPL_P_H*df$IFD*shape$Shape_Area))*1e-15
MPL_an_H
MYL_an_H = (sum(shape$MYLfluxt*MYL_P_H*df$IFD*shape$Shape_Area))*1e-15
MYL_an_H
MGL_an_H= ((sum(shape$MGLflux*MGL_P_H*df$IFD*shape$Shape_Area))+
             (sum((shape$MGLflux*2.63)*df$IFD*shape$LL_shore_area, na.rm = TRUE)))*1e-15
MGL_an_H
SPL_an_H = (sum(shape$SPLfluxt*SPL_P_H*df$IFD*shape$Shape_Area))*1e-15
SPL_an_H
SGL_an_H = (sum(shape$SGLfluxt*SGL_P_H*df$IFD*shape$Shape_Area))*1e-15
SGL_an_H
SYL_an_H = (sum(shape$SYLfluxt*SYL_P_H*df$IFD*shape$Shape_Area))*1e-15
SYL_an_H
LAL_an_H = ((sum(shape$LALflux*LAL_P_H*df$IFD*shape$Shape_Area))+
              (sum((shape$LALflux*2.6)*df$IFD*shape$LL_shore_area_m, na.rm = TRUE)))*1e-15
LAL_an_H

total_an_H = sum(MPL_an_H, MYL_an_H, MGL_an_H, SPL_an_H, SGL_an_H, SYL_an_H, LAL_an_H)
total_an_H

###### Rivers #############

#fluxes and 95%CI from Stanley et al. 2022 Grimes Database
#Organic rich rivers (SRR)- 148.8 +/-88
#other rivers 99.2 +/- 53.12

SRR_P = df$SRR/100
SRP_P = df$SRP/100
LAR_P = df$LAR/100

df$SRR_em = ((148.8*SRP_P*(df$IFD)*df$Shape_Area)*1e-15)/df$Shape_Area*1e12 

SRR_an = (sum(148.8*SRR_P*df$IFD*shape$Shape_Area))*1e-15
SRR_an
SRP_an = (sum(99.2*SRP_P*df$IFD*shape$Shape_Area))*1e-15
SRP_an
LAR_an = (sum(99.2*LAR_P*df$IFD*shape$Shape_Area))*1e-15
LAR_an

#Low flux

SRR_an_lf = (sum((148.8-88)*SRR_P*df$IFD*shape$Shape_Area))*1e-15
SRR_an_lf
SRP_an_lf = (sum((99.2-53)*SRP_P*df$IFD*shape$Shape_Area))*1e-15
SRP_an_lf
LAR_an_lf = (sum((99.2-53)*LAR_P*df$IFD*shape$Shape_Area))*1e-15
LAR_an_lf

#high flux

SRR_an_hf = (sum((148.8+88)*SRR_P*df$IFD*shape$Shape_Area))*1e-15
SRR_an_hf
SRP_an_hf = (sum((99.2+53)*SRP_P*df$IFD*shape$Shape_Area))*1e-15
SRP_an_hf
LAR_an_hf = (sum((99.2+53)*LAR_P*df$IFD*shape$Shape_Area))*1e-15
LAR_an_hf

#River area uncertainty

#low bound
SRR_P_L = df$SRR_L/100
SRP_P_L = df$SRP_L/100
LAR_P_L = df$LAR_L/100

#high bound
SRR_P_H = df$SRR_H/100
SRP_P_H = df$SRP_H/100
LAR_P_H = df$LAR_H/100

## replace areas below 

#Low 
SRR_an_L = (sum(148.8*SRR_P_L*df$IFD*shape$Shape_Area))*1e-15
SRR_an_L
SRP_an_L = (sum(99.2*SRP_P_L*df$IFD*shape$Shape_Area))*1e-15
SRP_an_L
LAR_an_L = (sum(99.2*LAR_P_L*df$IFD*shape$Shape_Area))*1e-15
LAR_an_L

#high 
SRR_an_H = (sum(148.8*SRR_P_H*df$IFD*shape$Shape_Area))*1e-15
SRR_an_H
SRP_an_H = (sum(99.2*SRP_P_H*df$IFD*shape$Shape_Area))*1e-15
SRP_an_H
LAR_an_H = (sum(99.2*LAR_P_H*df$IFD*shape$Shape_Area))*1e-15
LAR_an_H