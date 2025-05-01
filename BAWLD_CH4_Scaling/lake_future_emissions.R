######


###Testing the lake temperature scenarios 


shape = read.csv("bawld_full.csv", header = TRUE)
head(shape)

df= read.csv("bawld_full_lakes.csv", header = TRUE)

df_cmip6 = read.csv("bawld_cmip6.csv", header = TRUE, na.strings  = "NA")
head(df_cmip6)


df_joined <- df_cmip6 %>%
  left_join(df %>% select(Cell_ID, IFD), by = "Cell_ID")


df_cmip6$CD_Tmp_An = ifelse(df_cmip6$CD_Tmp_An == -9999, NA, df_cmip6$CD_Tmp_An)



shape = df_joined

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


### loading in the coefficients

df_coefsA1 = read.csv("df_coefs_lake.csv", header = TRUE)

#setting shape to df

head(shape)


df = shape

#full daily CH4 equation for aquatic systems. Full equation is being exponentially transformed to unlog the result

#daily aquatic emissions per class per cell (mg CH4 m-2 d-1). Not linked to area or percent cover per cell yet 

#example of terrestrial equation
#df$MarD_CH4_1 = 10^(df_coefs$FI_coef[1] +df_coefs$FTC_coef[1]*dfT1$MarTs + df_coefs$MAR_coef[1])-10

#midsize glacial lake 
for (i in 1:200) {
  df[[paste0("MGLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$modTemp + df_coefsA1$MGL_coef[i])
}

match("MGLD_CH4_1",names(df)) #176
match("MGLD_CH4_200",names(df)) #375

MGL_df = df[, c(92:291)]

head(MGL_df)

#MPL 

for (i in 1:200) {
  df[[paste0("MPLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$modTemp + df_coefsA1$MPL_coef[i])
}

match("MPLD_CH4_1",names(df)) #376
match("MPLD_CH4_200",names(df)) #575

MPL_df = df[, c(292:491)]

head(MPL_df)

#MYL 

for (i in 1:200) {
  df[[paste0("MYLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$modTemp + df_coefsA1$MYL_coef[i])
}

match("MYLD_CH4_1",names(df)) #576
match("MYLD_CH4_200",names(df)) #775

MYL_df = df[, c(492:691)]

head(MYL_df)

##small glacial lakes

for (i in 1:200) {
  df[[paste0("SGLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$modTemp + df_coefsA1$SGL_coef[i])
}


match("SGLD_CH4_1",names(df)) #776
match("SGLD_CH4_200",names(df)) #975

SGL_df = df[, c(692:891)]

head(SGL_df)

#small peatland lakes 

for (i in 1:200) {
  df[[paste0("SPLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$modTemp + df_coefsA1$SPL_coef[i])
}


match("SPLD_CH4_1",names(df)) #976
match("SPLD_CH4_200",names(df)) #10275

SPL_df = df[, c(892:1091)]

head(SPL_df)

## Small yedoma lakes ##

for (i in 1:200) {
  df[[paste0("SYLD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$modTemp + df_coefsA1$SYL_coef[i])
}

match("SYLD_CH4_1",names(df)) #1176
match("SYLD_CH4_200",names(df)) #1375

SYL_df = df[, c(1092:1291)]

head(SYL_df)

## large lakes 

for (i in 1:200) {
  df[[paste0("LALD_CH4_", i)]] <- 10^(df_coefsA1$FIA_coef[i] + df_coefsA1$FTCA_coef[i] * df$modTemp)
}

match("LALD_CH4_1",names(df)) #1376
match("LALD_CH4_200",names(df)) #1575

LAL_df = df[, c(1292:1491)]

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

head(shape$IFD)


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


##

shape$IFD20 = shape4$IFD + 20

head(shape4)

#### thaw areas 
#calculating lake percent and areas similar to terrestrial class above. 
LAL_m_P = (shape2$LAL_m/100)
MPL_m_P = (shape2$MPL_m/100)
MYL_m_P = (shape2$MYL_m/100)
MGL_m_P = (shape2$MGL_m/100)
SPL_m_P = (shape2$SPL_m/100)
SYL_m_P = (shape2$SYL_m/100)
SGL_m_P = (shape2$SGL_m/100)

NMPL_m_P = (shape2$NMPL_m/100)
NMYL_m_P = (shape2$NMYL_m/100)
NSPL_m_P = (shape2$NSPL_m/100)
NSYL_m_P = (shape2$NSYL_m/100)

MPL_m_P

head(shape)
# totals for small lakes, yedoma lakes and peatland lakes

MPL_an = (sum(shape$MPLfluxt*MPL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))*1e-15 + 
  (sum(111*NMPL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))*1e-15
MPL_an
MYL_an = (sum(shape$MYLfluxt*MYL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))*1e-15+ 
  (sum(139*NMYL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))*1e-15
MYL_an
MGL_an = ((sum(shape$MGLflux*MGL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))+
            (sum((shape$MGLflux*2.63)*shape$IFD20*shape$LL_shore_area, na.rm = TRUE)))*1e-15
MGL_an
SPL_an = (sum(shape$SPLfluxt*SPL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))*1e-15+ 
  (sum(111*NSPL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))*1e-15
SPL_an
SGL_an = (sum(shape$SGLfluxt*SGL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))*1e-15
SGL_an
SYL_an = (sum(shape$SYLfluxt*SYL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))*1e-15+ 
  (sum(139*NSYL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))*1e-15
SYL_an
LAL_an = ((sum(shape$LALflux*LAL_m_P*shape$IFD20*shape$Shp_Area, na.rm = TRUE))+
            (sum((shape$LALflux*2.63)*shape$IFD20*shape$LL_shore_area, na.rm = TRUE)))*1e-15
LAL_an


#open water emissions for central emission scenario
total_an = sum(MPL_an, MYL_an, MGL_an, SPL_an, SGL_an, SYL_an, LAL_an)
total_an

### map
