## Read the data
library(data.table)
library(ggplot2)
library(MASS)
library(pscl)
library(knitr)
library(systemfit)
library(VGAM)
library(multcomp)

setwd("/Users/lijun/Dropbox/2016Fall/TA_Consulting/Complete_Projects/2_Evan_butterfly_project/jun_Nov10_fix/")
fc  <- fread("ButterflyR.csv")
fi  <- fread("idaho_for_R.csv")
cfc <- dcast(fc, Site~Treatment,value.var ="Attacked", fun.aggregate = sum)
cfi <- dcast(fi, Site~Treatment,value.var ="Attacked", fun.aggregate = sum)
cfc_bird <- subset(cfc,rowSums(cfc[,-1,with=FALSE])>0)
cfi_bird <- subset(cfi,rowSums(cfi[,-1,with=FALSE])>0)
cfc_bird$location <- "California"
cfi_bird$location <- "Idaho"
cf_comb <- rbind(cfc_bird,cfi_bird)
dt_comb <- melt(cf_comb,id.vars=c("Site","location"))
setnames(dt_comb,colnames(dt_comb),c("site","location","butterfly","count"))
dt_comb$n <- 20
dt_comb$location  <- factor(dt_comb$location,levels=c("Idaho","California"))
dt_comb$butterfly <- factor(dt_comb$butterfly,levels=c("J","W","L","A"))
dt_comb$not_attacked <- dt_comb$n-dt_comb$count
dt_comb$poison <- dt_comb$butterfly%in% c("L","A")
dt_cal<-subset(dt_comb,dt_comb$location=="California")
dt_idh<-subset(dt_comb,dt_comb$location=="Idaho")

head(dt_comb)
head(dt_cal)
head(dt_idh)
head(cf_comb)

## Section 5.1  Binomial
mat = matrix(c(   -1/2, -1/2, 1/2, 1/2,
                  -1/2, -1/2, 0, 1,
                  -1/2, -1/2, 1, 0,
                  -1,1,0,0), ncol = 4)
colnames(mat)<-c("JW-LA","JW-A ","JW-L "," J-W ")
loglm_fit_1_c_a  <- glm(cbind(count,not_attacked)~butterfly-1,data=dt_cal ,family=quasibinomial)
loglm_fit_1_i_a  <- glm(cbind(count,not_attacked)~butterfly-1,data=dt_idh ,family=quasibinomial)

# California
summary(glht(loglm_fit_1_c_a, linfct=mcp(butterfly=t(mat[,-4])),alternative="less"))
mat_col4 <- as.matrix(mat[,4]); colnames(mat_col4) <- "J-W"
summary(glht(loglm_fit_1_c_a, linfct=mcp(butterfly=t(mat_col4)),alternative="two.sided"))

# Idaho
summary(glht(loglm_fit_1_i_a, linfct=mcp(butterfly=t(mat[,-4])),alternative="less"))
summary(glht(loglm_fit_1_i_a, linfct=mcp(butterfly=t(mat_col4)),alternative="two.sided"))

## Section 6.1
butterfly_data <- read.csv("/Users/lijun/Dropbox/2016Fall/TA_Consulting/Complete_Projects/2_Evan_butterfly_project/jun_Nov10_fix/for_R_sheet.csv")
par(mfrow = c(2, 2))
hist(butterfly_data[butterfly_data$Treatment == "A", "Attacked"], breaks = 2, xlim = c(0,5),
     xlab = "Attacks", main = "Butterfly A")
hist(butterfly_data[butterfly_data$Treatment == "J", "Attacked"], breaks = 3, xlim = c(0,5),
     xlab = "Attacks", main = "Butterfly J")
hist(butterfly_data[butterfly_data$Treatment == "L", "Attacked"], breaks = 2, xlim = c(0,5),
     xlab = "Attacks", main = "Butterfly L")
hist(butterfly_data[butterfly_data$Treatment == "W", "Attacked"], breaks = 3, xlim = c(0,5),
     xlab = "Attacks", main = "Butterfly W")

## Section 6.2
# 6.2.1 & 6.2.2; the code is in the report

## Section 6.3: Quasipoisson
loglm_fit_1_c_b  <- glm(count~butterfly-1,data=dt_cal ,family=quasipoisson)
loglm_fit_1_i_b  <- glm(count~butterfly-1,data=dt_idh ,family=quasipoisson)
# California
summary(glht(loglm_fit_1_c_b, linfct=mcp(butterfly=t(mat[,-4])),alternative="less"))
summary(glht(loglm_fit_1_c_b, linfct=mcp(butterfly=t(mat_col4)),alternative="two.sided"))
# Idaho
summary(glht(loglm_fit_1_i_b, linfct=mcp(butterfly=t(mat[,-4])),alternative="less"))
summary(glht(loglm_fit_1_c_b, linfct=mcp(butterfly=t(mat_col4)),alternative="two.sided"))




























































