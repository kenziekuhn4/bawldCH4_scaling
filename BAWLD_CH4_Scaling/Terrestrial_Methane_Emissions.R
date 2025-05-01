#Terrestrial emissions and uncertainties

#Code creator- McKenzie Kuhn 
#BAWLD-CH4 scaling, March 2025

#loading the required packages


library(ggplot2)   # for general plotting

library(matrixStats)

library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(sf)        #for simple features 
library(tidyverse)
library(matrixStats)

require(rgdal)
library(ggpubr)

#
library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")

library(maps)


#reading in the  BAWLD Landcover model with land cover distributions from Olefeldt et al. 2021 ESSD

shape = read.csv("bawld_full.csv", header = TRUE)
head(shape)

#optional
#loading in a base map layer to be used in figure development later on
#Change path to your directory
map <- read_sf('C:\\Users\\kuhnm\\Dropbox\\2 University of Alberta\\1.Phd Chapters\\Ch 4 Methane Synthesis\\GCB Models\\Upscaling Product\\Final_models_april_2022\\Manuscript\\Figures and Code\\Spatial_emissions_maps\\crnt_ftr_emissions.shp')

head(map)
map$GSD = shape$gs_days1


#Scaling wetland and terrestrial methane emissions

#calculating the % cover for each land cover type per cell 
PermBog_P=  shape$PermBog/100
PermWet_P = shape$PermWet/100
Marsh_P = shape$Marsh/100
Bog_P = shape$Bog/100
Fen_P = shape$Fen/100
UpTundra_P = shape$UpTundra/100
Boreal_P = shape$Boreal/100


#calculating the total area for each landcover type per cell 
PermBog_area= shape$Shape_Area*PermBog_P
PermWet_area= shape$Shape_Area*PermWet_P
Marsh_area= shape$Shape_Area*Marsh_P
Bog_area= shape$Shape_Area*Bog_P
Fen_area= shape$Shape_Area*Fen_P
UpTundra_area= shape$Shape_Area*UpTundra_P
Boreal_area= shape$Shape_Area*Boreal_P


######### Terrestrial scaling code ##########

# Setting up the class water table characteristics used in the temperature model 
#values are mean water table position from Kuhn et al. 2021 ESSD

#change a to see how water table impacts output
a = 0

MarshWT = 2 + a
PermWetWT = 0 + a
FenWT = -6 + a
BogWT = -12.65 + a
PermBogWT = -22.15+ a

#### annual temperature term

#change b to see how temperature impacts output
b = 0

shape$CD_Tmp_AnX = shape$CD_Tmp_An + b

shape$CD_Tmp_AnX

#changing name of file to condense text
df = as.data.frame(shape)

## running the wetlands model 

#Calculating an average soil temperature for each landcover class as individual columns 
# SoiT = Annual_Temp + WTAv + C 

#intercept 
I = 1.65
#temp constant
TC = 0.0023
#water table constant 
WC = 0.011
eFEN =  0.29
eMAR = 0.58
ePEB = -0.23
ePEW = 0.30

# The following code does not need to be run, the output is included in cvs files
# #establishing the uncertainty bounds (95CI from the model output)
# 
# # creating a dataframe for random number for monte-carlo assessment
# df_out <- data.frame(matrix(0, ncol = 1, nrow = 200))
# 
# 
# #randomly selecting a number between 0 and 1
# 
# df_coefs <- data.frame(matrix(0, ncol = 1, nrow = 200))
# 
# set.seed(4)
# 
# #
# df_coefs$I = runif(200)
# df_coefs$IT = runif(200)
# df_coefs$IW = runif(200)
# df_coefs$IF = runif(200)
# df_coefs$IMar = runif(200)
# df_coefs$IPeb = runif(200)
# df_coefs$IPew = runif(200)
# 
# #random variations for water tables 
# 
# df_coefs$IFWtd = runif(200)
# df_coefs$IMarWtd = runif(200)
# df_coefs$IPebWtd= runif(200)
# df_coefs$IPewWtd = runif(200)
# 
# #For intercept
# x_I = df_coefs$I
# 
# m = NULL;
# for (I in 1:length(x_I)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,1.65,0.01))
#    break
# 
#   } else { print (rnorm(1,1.65, 0.01))
#   }
#   m = rbind(m, coef_I)
# }
# 
# df_coefs$I_coef = m[,1]
# head(df_coefs)
# 
# ##Temperature
# 
# #For intercept
# Tx_I = df_coefs$IT
# 
# Tm = NULL;
# for (I in 1:length(Tx_I)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,0.0023,0.0001))
#     break
# 
#   } else { print (rnorm(1,0.0022,0.0006))
#   }
#   Tm = rbind(Tm, coef_I)
# }
# 
# df_coefs$TI_coef = Tm[,1]
# head(df_coefs)
# 
# #For water table
# Wx_I = df_coefs$IW
# 
# Wm = NULL;
# for (I in 1:length(Wx_I)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,0.011,0.002))
#     break
# 
#   } else { print (rnorm(1,0.011,0.002))
#   }
#   Wm = rbind(Wm, coef_I)
# }
# 
# df_coefs$WI_coef = Wm[,1]
# head(df_coefs)
# 
# 
# #Fen coefs
# 
# Fenx_I = df_coefs$IF
# 
# Fenm = NULL;
# for (I in 1:length(Fenx_I)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,0.287,0.002))
#     break
# 
#   } else { print (rnorm(1,0.287,0.007))
#   }
#   Fenm = rbind(Fenm, coef_I)
# }
# 
# df_coefs$FenI_coef = Fenm[,1]
# head(df_coefs)
# 
# #Marshes
# 
# Marx_I = df_coefs$IMar
# 
# Marm = NULL;
# for (I in 1:length(Marx_I)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,0.58,0.003))
#     break
# 
#   } else { print (rnorm(1,0.58,0.007))
#   }
#   Marm = rbind(Marm, coef_I)
# }
# 
# df_coefs$MarI_coef = Marm[,1]
# head(df_coefs)
# 
# #PermafrostBogs
# 
# Pebx_I = df_coefs$IPeb
# 
# Pebm = NULL;
# for (I in 1:length(Pebx_I)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,-0.23,0.004))
#     break
# 
#   } else { print (rnorm(1,-0.23,0.013))
#   }
#   Pebm = rbind(Pebm, coef_I)
# }
# 
# df_coefs$PebI_coef = Pebm[,1]
# head(df_coefs)
# 
# #PEW
# 
# Pewx_I = df_coefs$IPew
# 
# Pewm = NULL;
# for (I in 1:length(Pewx_I)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,0.30,0.001))
#     break
# 
#   } else { print (rnorm(1,0.30,0.08))
#   }
#   Pewm = rbind(Pewm, coef_I)
# }
# 
# df_coefs$PewI_coef = Pewm[,1]
# head(df_coefs)
# 
# #now water table depths
# 
# df_coefs$IFWtd = runif(200)
# df_coefs$IMarWtd = runif(200)
# df_coefs$IPebWtd= runif(200)
# df_coefs$IPewWtd = runif(200)
# df_coefs$IBogWtd = runif(200)
# 
# #permafrost wetlands
# 
# Pewx_IW = df_coefs$IPewWtd
# 
# PewmW = NULL;
# for (I in 1:length(Pewx_IW)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,-0.4,7.4))
#     break
#     
#   } else { print (rnorm(1,-0.4,7.4))
#   }
#   PewmW = rbind(PewmW, coef_I)
# }
# 
# df_coefs$PewI_coefW = PewmW[,1]
# head(df_coefs)
# 
# ##fens
# 
# Fenx_IW = df_coefs$IFWtd
# 
# FenmW = NULL;
# for (I in 1:length(Fenx_IW)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,-6,8.85))
#     break
#     
#   } else { print (rnorm(1,-6,8.85))
#   }
#   FenmW = rbind(FenmW, coef_I)
# }
# 
# df_coefs$FenI_coefW = FenmW[,1]
# head(df_coefs)
# 
# ##Marshes
# 
# Marx_IW = df_coefs$IMarWtd
# 
# MarmW = NULL;
# for (I in 1:length(Marx_IW)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,2,10.6))
#     break
#     
#   } else { print (rnorm(1,2, 10.6))
#   }
#   MarmW = rbind(MarmW, coef_I)
# }
# 
# df_coefs$MarI_coefW = MarmW[,1]
# head(df_coefs)
# 
# #Permafrost Bogs
# 
# Pebx_IW = df_coefs$IPebWtd
# 
# PebmW = NULL;
# for (I in 1:length(Pebx_IW)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,-22,15))
#     break
#     
#   } else { print (rnorm(1,-22,15))
#   }
#   PebmW = rbind(PebmW, coef_I)
# }
# 
# df_coefs$PebI_coefW = PebmW[,1]
# head(df_coefs)
# 
# ###Bogs
# 
# Bogx_IW = df_coefs$IBogWtd
# 
# BogmW = NULL;
# for (I in 1:length(Bogx_IW)) {
#   coef_I = if (I <0.5){
#     print(rnorm(1,-12.7,11.2))
#     break
#     
#   } else { print (rnorm(1,-12.7,11.2))
#   }
#   BogmW = rbind(BogmW, coef_I)
# }
# 
# df_coefs$BogI_coefW = BogmW[,1]
# head(df_coefs)
# 
# #df_coefs = df_coefs[,c(15,16,17,18,19,20,21,22,23,24, 25, 26)]
# 
# #dataframe with just the coefficients. These will be used to create fluxes now. 
# 
# 
# #Writing a csv for the coefficients to use for later 
# 
# write.csv(df_coefs, "wetland_coefs.csv")


#uncertainty bounds for coefficients
df_coefs = read.csv("wetland_coefs.csv", header = TRUE)

head(df_coefs)



# Loop to create 200 matrices
# Standard deviation for the uncertainties



################# for Marshes #############

#subset
for (i in 1:200) {
  df[[paste0("MarD_CH4_", i)]] <- 10^(df_coefs$I_coef[i] + 
                                        df_coefs$TI_coef[i] * df$CD_Tmp_AnX + 
                                        df_coefs$WI_coef[i] * df_coefs$MarI_coefW[i] +
                                        df_coefs$MarI_coef[i]) - 10
}


match("MarD_CH4_1",names(df)) #173
match("MarD_CH4_200",names(df)) #372

MARSH_df = df[, c(173:372)]

#head(MARSH_df)

# fen ############

for (i in 1:200) {
  df[[paste0("FenD_CH4_", i)]] <- 10^(df_coefs$I_coef[i] + 
                                        df_coefs$TI_coef[i] * df$CD_Tmp_AnX + 
                                        df_coefs$WI_coef[i] * df_coefs$FenI_coefW[i] +
                                        df_coefs$FenI_coef[i]) - 10
}



match("FenD_CH4_1",names(df)) #293
match("FenD_CH4_200",names(df)) #492

Fen_df = df[, c(373:572)]

#head(Fen_df)


#PEB

for (i in 1:200) {
  df[[paste0("PebD_CH4_", i)]] <- 10^(df_coefs$I_coef[i] + 
                                        df_coefs$TI_coef[i] * df$CD_Tmp_AnX + 
                                        df_coefs$WI_coef[i] * df_coefs$PebI_coefW[i] +
                                        df_coefs$PebI_coef[i]) - 10
}



match("PebD_CH4_1",names(df)) #493
match("PebD_CH4_200",names(df)) #692

Peb_df = df[, c(573:772)]


#PEW


for (i in 1:200) {
  df[[paste0("PewD_CH4_", i)]] <- 10^(df_coefs$I_coef[i] + 
                                        df_coefs$TI_coef[i] * df$CD_Tmp_AnX + 
                                        df_coefs$WI_coef[i] * df_coefs$PewI_coefW[i] +
                                        df_coefs$PewI_coef[i]) - 10
}



match("PewD_CH4_1",names(df)) #774
match("PewD_CH4_200",names(df)) #937

Pew_df = df[, c(773:972)]

#head(Pew_df)

# Bogs


for (i in 1:200) {
  df[[paste0("BogD_CH4_", i)]] <- 10^(df_coefs$I_coef[i] + 
                                        df_coefs$TI_coef[i] * df$CD_Tmp_AnX + 
                                        df_coefs$WI_coef[i] * df_coefs$BogI_coefW[i]  ) - 10
}



match("BogD_CH4_1",names(df)) #896
match("BogD_CH4_200",names(df)) #1095

Bog_df = df[, c(973:1172)]

#head(Bog_df)

#turn all of the dataframes into matrices to add them together

Mar_matrix = as.matrix(MARSH_df)
Fen_matrix = as.matrix(Fen_df)
Pew_matrix = as.matrix(Pew_df)
Peb_matrix = as.matrix(Peb_df)
Bog_matrix = as.matrix(Bog_df)

Sum_Matrix <- Mar_matrix + Fen_matrix + Pew_matrix + Peb_matrix + Bog_matrix

head(Sum_Matrix)


#calculating daily emissions in each cell based on percent of each class present in the grid (mg CH4 m-2 d-1)

############# need to take means across the matrix with CI



shape$fenflux = rowMeans(Fen_matrix)
shape$bogflux = rowMeans(Bog_matrix)
shape$marshflux = rowMeans(Mar_matrix)

shape$pebflux = rowMeans(Peb_matrix)
shape$pewflux = rowMeans(Pew_matrix)

#standard deviations

shape$fenfluxsd = rowSdDiffs(Fen_matrix)
shape$bogfluxsd = rowSdDiffs(Bog_matrix)
shape$marshfluxsd = rowSdDiffs(Mar_matrix)

head(shape$marshfluxsd)

shape$pebfluxsd = rowSdDiffs(Peb_matrix)
shape$pewfluxsd = rowSdDiffs(Pew_matrix)

#setting growing season day term

gs_vec = as.vector(df$gs_days1 + 0)



#calculating the means and upper/lower bounds for growing season emissions 

fen_an = (sum(shape$fenflux*Fen_P*gs_vec*shape$Shape_Area))*1e-15
fen_an
fen_an_hs = (sum(((shape$fenflux+(1.96*(shape$fenfluxsd/sqrt(200))))*Fen_P*gs_vec*shape$Shape_Area)))*1e-15
fen_an_hs
fen_an_ls = (sum(((shape$fenflux-(1.96*(shape$fenfluxsd/sqrt(200))))*Fen_P*gs_vec*shape$Shape_Area)))*1e-15
fen_an_ls


bog_an = (sum(shape$bogflux*Bog_P*gs_vec*shape$Shape_Area))*1e-15
bog_an
bog_an_hs = (sum(((shape$bogflux+(1.96*(shape$bogfluxsd/sqrt(200))))*Bog_P*gs_vec*shape$Shape_Area)))*1e-15
bog_an_hs
bog_an_ls = (sum(((shape$bogflux-(1.96*(shape$bogfluxsd/sqrt(200))))*Bog_P*gs_vec*shape$Shape_Area)))*1e-15
bog_an_ls


mar_an = (sum(shape$marshflux*Marsh_P*gs_vec*shape$Shape_Area))*1e-15
mar_an
mar_an_hs = (sum(((shape$marshflux+(1.96*(shape$marshfluxsd/14.14)))*Marsh_area*gs_vec)))*1e-15
mar_an_hs
mar_an_ls = (sum(((shape$marshflux-(1.96*(shape$marshfluxsd/14.14)))*Marsh_area*gs_vec)))*1e-15
mar_an_ls

shape$marshfluxsd

pew_an = (sum(shape$pewflux*PermWet_P*gs_vec*shape$Shape_Area))*1e-15
pew_an
pew_an_hs = (sum(((shape$pewflux+(1.96*(shape$pewfluxsd/14.14)))*PermWet_area*gs_vec)))*1e-15
pew_an_hs
pew_an_ls = (sum(((shape$pewflux-(1.96*(shape$pewfluxsd/14.14)))*PermWet_area*gs_vec)))*1e-15
pew_an_ls


peb_an = (sum(shape$pebflux*PermBog_P*gs_vec*shape$Shape_Area))*1e-15
peb_an
peb_an_hs = (sum(((shape$pebflux+(1.96*(shape$pebfluxsd/14.14)))*PermBog_area*gs_vec)))*1e-15
peb_an_hs
peb_an_ls = (sum(((shape$pebflux-(1.96*(shape$pebfluxsd/14.14)))*PermBog_area*gs_vec)))*1e-15
peb_an_ls


mean_an = sum(fen_an, bog_an, mar_an, pew_an, peb_an)
mean_an

mean_an_ls = sum(fen_an_ls , bog_an_ls , mar_an_ls , pew_an_ls , peb_an_ls )
mean_an_ls 

mean_an_hs = sum(fen_an_hs, bog_an_hs, mar_an_hs, pew_an_hs, peb_an_hs)
mean_an_hs

#### Low and High area uncertainty for BAWLD

names(shape)[names(shape) == "PEB_L"] <- "PermBog_L"
names(shape)[names(shape) == "WTU_L"] <- "PermWet_L"
names(shape)[names(shape) == "MAR_L"] <- "Marsh_L"
names(shape)[names(shape) == "BOG_L"] <- "Bog_L"
names(shape)[names(shape) == "FEN_L"] <- "Fen_L"


#calculating the % cover for each land cover type per cell 
PermBog_P_L=  shape$PermBog_L/100
PermWet_P_L = shape$PermWet_L/100
Marsh_P_L = shape$Marsh_L/100
Bog_P_L = shape$Bog_L/100
Fen_P_L = shape$Fen_L/100


#calculating the total area for each landcover type per cell 
PermBog_area_L= shape$Shape_Area*PermBog_P_L
PermWet_area_L= shape$Shape_Area*PermWet_P_L
Marsh_area_L= shape$Shape_Area*Marsh_P_L
Bog_area_L= shape$Shape_Area*Bog_P_L
Fen_area_L= shape$Shape_Area*Fen_P_L

##Higher areas ##

names(shape)[names(shape) == "PEB_H"] <- "PermBog_H"
names(shape)[names(shape) == "WTU_H"] <- "PermWet_H"
names(shape)[names(shape) == "MAR_H"] <- "Marsh_H"
names(shape)[names(shape) == "BOG_H"] <- "Bog_H"
names(shape)[names(shape) == "FEN_H"] <- "Fen_H"


#calculating the % cover for each land cover type per cell 
PermBog_P_H=  shape$PermBog_H/100
PermWet_P_H = shape$PermWet_H/100
Marsh_P_H = shape$Marsh_H/100
Bog_P_H = shape$Bog_H/100
Fen_P_H = shape$Fen_H/100


#calculating the total area for each landcover type per cell 
PermBog_area_H= shape$Shape_Area*PermBog_P_H
PermWet_area_H= shape$Shape_Area*PermWet_P_H
Marsh_area_H= shape$Shape_Area*Marsh_P_H
Bog_area_H= shape$Shape_Area*Bog_P_H
Fen_area_H= shape$Shape_Area*Fen_P_H

### Low and high area bounds based on 95% confidence intervals from BAWLD (Olefeldt et al. 2021)


fen_an_H = (sum(((shape$fenflux+(1.96*(shape$fenfluxsd/sqrt(200))))*Fen_P_H*gs_vec*shape$Shape_Area)))*1e-15
fen_an_H
fen_an_L = (sum(((shape$fenflux-(1.96*(shape$fenfluxsd/sqrt(200))))*Fen_P_L*gs_vec*shape$Shape_Area)))*1e-15
fen_an_L



bog_an_H = (sum(((shape$bogflux+(1.96*(shape$bogfluxsd/sqrt(200))))*Bog_P_H*gs_vec*shape$Shape_Area)))*1e-15
bog_an_H
bog_an_L = (sum(((shape$bogflux-(1.96*(shape$bogfluxsd/sqrt(200))))*Bog_P_L*gs_vec*shape$Shape_Area)))*1e-15
bog_an_L



mar_an_H = (sum(((shape$marshflux+(1.96*(shape$marshfluxsd/14.14)))*Marsh_P_H*gs_vec*shape$Shape_Area)))*1e-15
mar_an_H
mar_an_L = (sum(((shape$marshflux-(1.96*(shape$marshfluxsd/14.14)))*Marsh_P_L*gs_vec*shape$Shape_Area)))*1e-15
mar_an_L


pew_an_H = (sum(((shape$pewflux+(1.96*(shape$pewfluxsd/14.14)))*PermWet_P_H*gs_vec*shape$Shape_Area)))*1e-15
pew_an_H
pew_an_L = (sum(((shape$pewflux-(1.96*(shape$pewfluxsd/14.14)))*PermWet_P_L*gs_vec*shape$Shape_Area)))*1e-15
pew_an_L



peb_an_H = (sum(((shape$pebflux+(1.96*(shape$pebfluxsd/14.14)))*PermBog_P_H*gs_vec*shape$Shape_Area)))*1e-15
peb_an_H
peb_an_L = (sum(((shape$pebflux-(1.96*(shape$pebfluxsd/14.14)))*PermBog_P_L*gs_vec*shape$Shape_Area)))*1e-15
peb_an_L


mean_an_L = sum(fen_an_L, bog_an_L, mar_an_L, pew_an_L, peb_an_L)
mean_an_L

mean_an_H = sum(fen_an_H, bog_an_H, mar_an_H, pew_an_H, peb_an_H)
mean_an_H


########### Boreal and tundra uplands. Simple scaling ####

### individual classes 
### Simple flux statistics from Kuhn et al. 2021 ESSD

#boreal

#sd = 2.3
#mean = 1.1
#95% CI = 0.58
#n=62

Bor_an = (sum(-1.1*Boreal_P*gs_vec*shape$Shape_Area))*1e-15
Bor_an
Bor_an_l = (sum((-1.1-0.58)*Boreal_P*gs_vec*shape$Shape_Area))*1e-15
Bor_an_l
Bor_an_h = (sum((-1.1+0.58)*Boreal_P*gs_vec*shape$Shape_Area))*1e-15
Bor_an_h

# Upland tundra

#sd = 15.7
#mean = 3.8
#95% CI = 3.91
#n=62

Tun_an = (sum(3.8*UpTundra_P*gs_vec*shape$Shape_Area))*1e-15
Tun_an
Tun_an_l = (sum((3.8-3.91)*UpTundra_P*gs_vec*shape$Shape_Area))*1e-15
Tun_an_l
Tun_an_h = (sum((3.8+3.91)*UpTundra_P*gs_vec*shape$Shape_Area))*1e-15
Tun_an_h


#calculating the % cover for each land cover type per cell 
#Finding area uncertainty 

UpTundra_P_L = shape$TUN_L/100
Boreal_P_L = shape$BOR_L/100
UpTundra_P_H = shape$TUN_H/100
Boreal_P_H = shape$BOR_H/100

Bor_an_L = (sum(-1.1*Boreal_P_L*gs_vec*shape$Shape_Area))*1e-15
Bor_an_L 
Bor_an_H = (sum(-1.1*Boreal_P_H*gs_vec*shape$Shape_Area))*1e-15
Bor_an_H

Tun_an_L  = (sum(3.8*UpTundra_P_L*gs_vec*shape$Shape_Area))*1e-15
Tun_an_L 
Tun_an_H = (sum(3.8*UpTundra_P_H*gs_vec*shape$Shape_Area))*1e-15
Tun_an_H
