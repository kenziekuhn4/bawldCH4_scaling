
####### cmip6 temperature scenarios (low, moderate, high)
# estimate uncertainty for wetland emissions including temp uncertainty

#Code creator- McKenzie Kuhn 
#PCN Methane synthesis, terrestrial and aquatic class upscaling 
#This code reads in a landcover shapefile 

#loading the required packages



library(ggplot2)   # for general plotting

library(matrixStats)

library(tidyverse)


df_cmip6 = read.csv("bawld_cmip6.csv", header = TRUE, na.strings  = "NA")
head(df_cmip6)

shape = df_cmip6

head(shape)

shape2 = read.csv("bawld_full.csv", header = TRUE)
head(shape2)

#calculating the % cover for each land cover type per cell 
PermBog_P=  shape$PEB/100
PermWet_P = shape$WTU/100
Marsh_P = shape$MAR/100
Bog_P = shape$BOG/100
Fen_P = shape$FEN/100
UpTundra_P = shape$TUN/100
Boreal_P = shape$BOR/100


#calculating the total area for each landcover type per cell 
PermBog_area= shape$Shp_Area*PermBog_P
PermWet_area= shape$Shp_Area*PermWet_P
Marsh_area= shape$Shp_Area*Marsh_P
Bog_area= shape$Shp_Area*Bog_P
Fen_area= shape$Shp_Area*Fen_P
UpTundra_area= shape$Shp_Area*UpTundra_P
Boreal_area= shape$Shp_Area*Boreal_P

#Renaming temperatures column 

names(shape)[names(shape) == "DN_2"] <- "lowTemp"
names(shape)[names(shape) == "DN"] <- "modTemp"
names(shape)[names(shape) == "upper"] <- "highTemp"

######### Terrestrial scaling code - future emissions ##########

#### annual temperature term

shape$CD_Tmp_AnX = shape$modTemp

shape$CD_Tmp_An_24 = shape2$CD_Tmp_An + 3.6

shape$CD_Tmp_An_24

# Replace NAs in column A with values from column B

shape <- shape %>%
  mutate(CD_Tmp_AnX = coalesce(CD_Tmp_AnX, CD_Tmp_An_24))

shape$CD_Tmp_AnX

head(shape)


df = as.data.frame(shape)

## running the wetlands model 


#establishing the uncertainty bounds (95CI from the model output)

df_coefs = read.csv("wetland_coefs.csv", header = TRUE)

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

MARSH_df = df[, c(92:291)]

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

Fen_df = df[, c(292:491)]

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

Peb_df = df[, c(492:691)]


#PEW


for (i in 1:200) {
  df[[paste0("PewD_CH4_", i)]] <- 10^(df_coefs$I_coef[i] + 
                                        df_coefs$TI_coef[i] * df$CD_Tmp_AnX + 
                                        df_coefs$WI_coef[i] * df_coefs$PewI_coefW[i] +
                                        df_coefs$PewI_coef[i]) - 10
}



match("PewD_CH4_1",names(df)) #774
match("PewD_CH4_200",names(df)) #937

Pew_df = df[, c(692:891)]

#head(Pew_df)

# Bogs


for (i in 1:200) {
  df[[paste0("BogD_CH4_", i)]] <- 10^(df_coefs$I_coef[i] + 
                                        df_coefs$TI_coef[i] * df$CD_Tmp_AnX + 
                                        df_coefs$WI_coef[i] * df_coefs$BogI_coefW[i]  ) - 10
}



match("BogD_CH4_1",names(df)) #896
match("BogD_CH4_200",names(df)) #1095

Bog_df = df[, c(892:1091)]

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

shape$fenflux = rowMeans(Fen_matrix)
shape$bogflux = rowMeans(Bog_matrix)
shape$marshflux = rowMeans(Mar_matrix)

shape$pebflux = rowMeans(Peb_matrix)
shape$pewflux = rowMeans(Pew_matrix)



#Moderate thaw scenarios areas

#calculating the % cover for each land cover type per cell in the moderate thaw scenario
PermBog_P_ls=  shape2$PEB_m/100
PermWet_P_ls = shape2$WTU_m/100
Marsh_P_ls = shape2$MAR_m/100
Bog_P_ls = shape2$BOG_m/100
Fen_P_ls = shape2$FEN_m/100


#calculating the total area for each landcover type per cell 
PermBog_area_ls= shape$Shp_Area*PermBog_P_ls
PermWet_area_ls= shape$Shp_Area*PermWet_P_ls
Marsh_area_ls= shape$Shp_Area*Marsh_P_ls
Bog_area_ls= shape$Shp_Area*Bog_P_ls
Fen_area_ls= shape$Shp_Area*Fen_P_ls

### 

### Adding 20 growing season days to each cell under central warming scenario
### Add 10 for low scenario and 30 for high scenario
gs_vec = as.vector(shape2$gs_days1 + 20)

head(gs_vec)


#calculating the total annual growing season emissions for central emission scenario

fen_an_ls  = (sum(shape$fenflux*Fen_P_ls*gs_vec*shape2$Shape_Area))*1e-15
fen_an_ls 

bog_an_ls  = (sum(shape$bogflux*Bog_P_ls*gs_vec*shape2$Shape_Area))*1e-15
bog_an_ls 

mar_an_ls  = (sum(shape$marshflux*Marsh_P_ls*gs_vec*shape2$Shape_Area))*1e-15
mar_an_ls 

pew_an_ls  = (sum(shape$pewflux*PermWet_P_ls*gs_vec*shape2$Shape_Area))*1e-15
pew_an_ls 

peb_an_ls  = (sum(shape$pebflux*PermBog_P_ls*gs_vec*shape2$Shape_Area))*1e-15
peb_an_ls 

mean_an_ls = sum(fen_an_ls, bog_an_ls, mar_an_ls, pew_an_ls, peb_an_ls)
mean_an_ls