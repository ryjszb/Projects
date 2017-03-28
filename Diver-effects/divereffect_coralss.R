getwd() #this shows you where the working directory is at this time


#USING FORWARD MODEL SELECTION APPORACH; TRATMENT=DIVER PRESENCE-ABSENCE
#INDIPENDENT VARIABLE:TRATMENT=DIVER PRESENCE-ABSENCE
mydata <- read.csv("/Users/lijun/Dropbox/2016Fall/TA_Consulting/4_Rebecca_project/DIVER EFFECT, CORAL S&S, GROUP S.csv", header=TRUE, sep = ";")
attach(mydata)
head(mydata)
library(nlme)
colnames(mydata)

library(lme4)
library(glmmADMB)

subdata <- mydata[mydata$TOT..AGGRESSIVE.B.<=40, ]
subsubdata <- subdata[,c(2, 3, 4, 5, 6)]

lm1<- glm(TOT..AGGRESSIVE.B.~as.factor(TREATMENT) + CORAL.SPECIES + CORAL.SIZE + GROUP.SIZE, family= poisson (link= "log"), data = subdata)
summary(lm1)
anova(lm1)



lm1<- glmer(TOT..AGGRESSIVE.B.~as.factor(TREATMENT) + CORAL.SPECIES + (1|GROUP.ID), family= poisson (link= "log"))
summary(lm1)
anova(lm1)


lm1<- glmer(TOT..AGGRESSIVE.B.~as.factor(TREATMENT) + CORAL.SPECIES + CORAL.SIZE+ 
              GROUP.SIZE  + (1|GROUP.ID), family= poisson (link= "log"), data = mydata)
lm2<- glm(TOT..AGGRESSIVE.B.~as.factor(TREATMENT) + CORAL.SPECIES + CORAL.SIZE+ 
              GROUP.SIZE, family= poisson (link= "log"), data = mydata)
summary(lm1)
anova(lm1)




# Behavior 1: TOTAL AGGRESSIVE BEHAVIORS
# UNIVARIABLE

lm1<-lme(TOT..AGGRESSIVE.B.~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm1)
anova(lm1)

lm2<-lme(TOT..AGGRESSIVE.B.~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm2)
anova(lm2)

lm3<-lme(TOT..AGGRESSIVE.B.~CORAL.SIZE,random=~1|GROUP.ID,data=mydata)
summary(lm3)
anova(lm3)

lm4<-lme(TOT..AGGRESSIVE.B.~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) # SIG.DIFF.
summary(lm4)
anova(lm4)

# BIVARIABLE

lm5<-lme(TOT..AGGRESSIVE.B.~as.factor(GROUP.SIZE)+as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm5)
anova(lm5)

lm6<-lme(TOT..AGGRESSIVE.B.~as.factor(GROUP.SIZE)+(CORAL.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm6)
anova(lm6)

lm7<-lme(TOT..AGGRESSIVE.B.~as.factor(GROUP.SIZE)+as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm7)
anova(lm7)
# Behavior 2: CHASING (C)
# UNIVARIABLE

lm8<-lme(C~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm8)
anova(lm8)

lm9<-lme(C~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm9)
anova(lm9)

lm10<-lme(C~CORAL.SIZE,random=~1|GROUP.ID,data=mydata)
summary(lm10)
anova(lm10)

lm11<-lme(C~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) # SIG.DIFF.
summary(lm11)
anova(lm11)

# BIVARIABLE

lm12<-lme(C~as.factor(GROUP.SIZE)+as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm12)
anova(lm12)

lm13<-lme(C~as.factor(GROUP.SIZE)+(CORAL.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm13)
anova(lm13)

lm14<-lme(C~as.factor(GROUP.SIZE)+as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm14)
anova(lm14)


# Behavior 3: CHASING HETEROSPECIFICS (CHET)
# UNIVARIABLE

lm15<-lme(CHET~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm15)
anova(lm15)

lm16<-lme(CHET~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm16)
anova(lm16)

lm17<-lme(CHET~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) # SIG.DIFF.
summary(lm17)
anova(lm17)

lm18<-lme(CHET~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm18)
anova(lm18)

# BIVARIABLE

lm19<-lme(CHET~CORAL.SIZE+as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm19)
anova(lm19)

lm20<-lme(CHET~CORAL.SIZE+as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm20)
anova(lm20)

lm21<-lme(CHET~CORAL.SIZE+as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm21)
anova(lm21)


# Behavior 4: BITING (B)
# UNIVARIABLE

lm22<-lme(B~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm22)
anova(lm22)

lm23<-lme(B~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm23)
anova(lm23)

lm24<-lme(B~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm24)
anova(lm24)

lm25<-lme(B~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm25)
anova(lm25)

# Behavior 5: BITING HETEROSPECIFICS (BHET) NONE

# Behavior 6: AGGRESSIVE DISPLAY (AD)
# UNIVARIABLE

lm26<-lme(AD~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm26)
anova(lm26)

lm27<-lme(AD~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm27)
anova(lm27)

lm28<-lme(AD~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm28)
anova(lm28)

lm29<-lme(AD~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm29)
anova(lm29)

# Behavior 7: RAM (R) NONE
# Behavior 8: RAM AGAINST HETEROSPECIFICS (RHET) NONE

# Behavior 9: AGGRESSIVE DISPLAY AGAINST HETEROSPECIFICS (ADHET)
# UNIVARIABLE

lm30<-lme(ADHET~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm30)
anova(lm30)

lm31<-lme(ADHET~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm31)
anova(lm31)

lm32<-lme(ADHET~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm32)
anova(lm32)

lm33<-lme(ADHET~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm33)
anova(lm33)

# Behavior 10:TOTAL AGGRESSIVE BEHAVIORS AGAINST HETEROSPECIFICS  (TOT..AGGRESS..HET)
# UNIVARIABLE

lm34<-lme(TOT..AGGRESS..HET~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm34)
anova(lm34)

lm35<-lme(TOT..AGGRESS..HET~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm35)
anova(lm35)

lm36<-lme(TOT..AGGRESS..HET~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm36)
anova(lm36)

lm37<-lme(TOT..AGGRESS..HET~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm37)
anova(lm37)

# Behavior 11:TOTAL AGGRESSIVE BEHAVIORS BY HETEROSPECIFICS  (TOT.AGGR..BY.HET)
# UNIVARIABLE

lm38<-lme(TOT.AGGR..BY.HET~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm38)
anova(lm38)

lm39<-lme(TOT.AGGR..BY.HET~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm39)
anova(lm39)

lm40<-lme(TOT.AGGR..BY.HET~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm40)
anova(lm40)

lm41<-lme(TOT.AGGR..BY.HET~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm41)
anova(lm41)

# Behavior 12:TOTAL SOCIAL BEHAVIORS (TOT..SOCIAL.B.)
# UNIVARIABLE

lm42<-lme(TOT..SOCIAL.B.~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm42)
anova(lm42)

lm43<-lme(TOT..SOCIAL.B.~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm43)
anova(lm43)

lm44<-lme(TOT..SOCIAL.B.~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm44)
anova(lm44)

lm45<-lme(TOT..SOCIAL.B.~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm45)
anova(lm45)

# Behavior 13: SIGNAL JUMPING (SJ)
# UNIVARIABLE

lm46<-lme(SJ~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm46)
anova(lm46)

lm47<-lme(SJ~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm47)
anova(lm47)

lm48<-lme(SJ~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm48)
anova(lm48)

lm49<-lme(SJ~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm49)
anova(lm49)

# Behavior 14: FOLLOWING (FO)
# UNIVARIABLE

lm50<-lme(FO~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm50)
anova(lm50)

lm51<-lme(FO~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm51)
anova(lm51)

lm52<-lme(FO~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm52)
anova(lm52)

lm53<-lme(FO~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm53)
anova(lm53)

# Behavior 15: FINS RISING (FR)
# UNIVARIABLE

lm54<-lme(FR~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm54)
anova(lm54)

lm55<-lme(FR~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm55)
anova(lm55)

lm56<-lme(FR~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm56)
anova(lm56)

lm57<-lme(FR~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm57)
anova(lm57)

# Behavior 16: MEETINGS (M) NONE

# Behavior 17: (ST) NONE

# Behavior 18: HETEROSPECIFIC AFFILIATION (HA) 
# UNIVARIABLE

lm58<-lme(HA~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm58)
anova(lm58)

lm59<-lme(HA~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) # SIGN. DIFF.
summary(lm59)
anova(lm59)

lm60<-lme(HA~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) # ALMOST SIGNIFICANT
summary(lm60)
anova(lm60)

lm61<-lme(HA~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm61)
anova(lm61)

# BIVARIABLE

lm62<-lme(HA~as.factor(CORAL.SPECIES)+as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm62)
anova(lm62)

lm63<-lme(HA~as.factor(CORAL.SPECIES)+(CORAL.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm63)
anova(lm63)

lm64<-lme(HA~as.factor(CORAL.SPECIES)+as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm64)
anova(lm64)

lm65<-lme(HA~CORAL.SIZE+as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm65)
anova(lm65)

lm66<-lme(HA~CORAL.SIZE+as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) # SIGN. DIFF.
summary(lm66)
anova(lm66)

lm67<-lme(HA~CORAL.SIZE+as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm67)
anova(lm67)

# TRIVARIABLE

lm68<-lme(HA~CORAL.SIZE+as.factor(CORAL.SPECIES)+as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm68)
anova(lm68)

lm69<-lme(HA~CORAL.SIZE+as.factor(CORAL.SPECIES)+as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm69)
anova(lm69)


# Behavior 19: TOTAL MAINTENANCE BEHAVIORS (TOT..MAINTENANCE.B.) 
# UNIVARIABLE

lm70<-lme(TOT..MAINTENANCE.B.~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm70)
anova(lm70)

lm71<-lme(TOT..MAINTENANCE.B.~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) 
summary(lm71)
anova(lm71)

lm72<-lme(TOT..MAINTENANCE.B.~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm72)
anova(lm72)

lm73<-lme(TOT..MAINTENANCE.B.~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) # SIGN. DIFF.
summary(lm73)
anova(lm73)

# BIVARIABLE

lm74<-lme(TOT..MAINTENANCE.B.~as.factor(GROUP.SIZE)+as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm74)
anova(lm74)

lm75<-lme(TOT..MAINTENANCE.B.~as.factor(GROUP.SIZE)+(CORAL.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm75)
anova(lm75)

lm76<-lme(TOT..MAINTENANCE.B.~as.factor(GROUP.SIZE)+as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm76)
anova(lm76)

# Behavior 20: FEEDING IN THE WATER COLUMN(FWC) 
# UNIVARIABLE

lm77<-lme(FWC~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm77)
anova(lm77)

lm78<-lme(FWC~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) 
summary(lm78)
anova(lm78)

lm79<-lme(FWC~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm79)
anova(lm79)

lm80<-lme(FWC~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) # SIGN. DIFF.
summary(lm80)
anova(lm80)

# BIVARIABLE

lm81<-lme(FWC~as.factor(GROUP.SIZE)+as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm81)
anova(lm81)

lm82<-lme(FWC~as.factor(GROUP.SIZE)+(CORAL.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm82)
anova(lm82)

lm83<-lme(FWC~as.factor(GROUP.SIZE)+as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm83)
anova(lm83)

# Behavior 21: FEEDING FROM THE BOTTOM (FB) 
# UNIVARIABLE

lm84<-lme(FB~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm84)
anova(lm84)

lm85<-lme(FB~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  # ALMOST SIGNIFICANT
summary(lm85)
anova(lm85)

lm86<-lme(FB~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm86)
anova(lm86)

lm87<-lme(FB~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm87)
anova(lm87)

# BIVARIABLE

lm88<-lme(FB~as.factor(CORAL.SPECIES)+as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm88)
anova(lm88)

lm89<-lme(FB~as.factor(CORAL.SPECIES)+(CORAL.SIZE),random=~1|GROUP.ID,data=mydata) # ALMOST SIG.
summary(lm89)
anova(lm89)

lm90<-lme(FB~as.factor(CORAL.SPECIES)+as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm90)
anova(lm90)


# Behavior 22: FEEDING FROM THE CORAL (FC) 
# UNIVARIABLE

lm93<-lme(FC~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm93)
anova(lm93)

lm94<-lme(FC~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  
summary(lm94)
anova(lm94)

lm95<-lme(FC~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm95)
anova(lm95)

lm96<-lme(FC~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)  # ALMOST SIG.
summary(lm96)
anova(lm96)

# Behavior 23: SHAKING (SH) 
# UNIVARIABLE

lm100<-lme(SH~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm100)
anova(lm100)

lm101<-lme(SH~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  
summary(lm101)
anova(lm101)

lm102<-lme(SH~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm102)
anova(lm102)

lm103<-lme(SH~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)  
summary(lm103)
anova(lm103)

# Behavior 24: POINTING (P) 
# UNIVARIABLE

lm104<-lme(P~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm104)
anova(lm104)

lm105<-lme(P~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  
summary(lm105)
anova(lm105)

lm106<-lme(P~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm106)
anova(lm106)

lm107<-lme(P~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)  
summary(lm107)
anova(lm107)

# Behavior 25: SCRATCHING (SC)
# UNIVARIABLE

lm108<-lme(SC~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm108)
anova(lm108)

lm109<-lme(SC~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  #ALMOST SIGNIFICANT
summary(lm109)
anova(lm109)

lm110<-lme(SC~CORAL.SIZE,random=~1|GROUP.ID,data=mydata)  #ALMOST SIGNIFICANT
summary(lm110)
anova(lm110)

lm111<-lme(SC~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm111)
anova(lm111)

# Behavior 26: DART (DA) 
# UNIVARIABLE

lm112<-lme(DA~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm112)
anova(lm112)

lm113<-lme(DA~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  #ALMOST SIGNIFICANT
summary(lm113)
anova(lm113)

lm114<-lme(DA~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm114)
anova(lm114)
 
 lm115<-lme(DA~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm115)
anova(lm115)

# Behavior 27: RETREAT (RT) 
# UNIVARIABLE

lm116<-lme(RT~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm116)
anova(lm116)

lm117<-lme(RT~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  
summary(lm117)
anova(lm117)

lm118<-lme(RT~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm118)
anova(lm118)

lm119<-lme(RT~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm119)
anova(lm119)

# Behavior 28: YAWNING (Y) 
# UNIVARIABLE

lm120<-lme(Y~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) # SIGN. DIFF.
summary(lm120)
anova(lm120)

lm121<-lme(Y~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  #ALMOST SIGNIFICANT
summary(lm121)
anova(lm121)

lm122<-lme(Y~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm122)
anova(lm122)

lm123<-lme(Y~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm123)
anova(lm123)

# BIVARIABLE

lm124<-lme(Y~as.factor(TREATMENT)+as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) #ALMOST SIGNIFICANT
summary(lm124)
anova(lm124)

lm125<-lme(Y~as.factor(TREATMENT)+(CORAL.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm125)
anova(lm125)

lm126<-lme(Y~as.factor(TREATMENT)+as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm126)
anova(lm126)

# Behavior 29: DOCK (DK) 
# UNIVARIABLE

lm127<-lme(DK~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm127)
anova(lm127)

lm128<-lme(DK~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  
summary(lm128)
anova(lm128)

lm129<-lme(DK~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm129)
anova(lm129)

lm130<-lme(DK~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm130)
anova(lm130)

# Behavior 29: TOTAL REPRODUCTIVE BEHAVIORS (TOT..REPRODUCTIVE.B.) 
# UNIVARIABLE

lm131<-lme(TOT..REPRODUCTIVE.B.~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm131)
anova(lm131)

lm132<-lme(TOT..REPRODUCTIVE.B.~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  
summary(lm132)
anova(lm132)

lm133<-lme(TOT..REPRODUCTIVE.B.~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm133)
anova(lm133)

lm134<-lme(TOT..REPRODUCTIVE.B.~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm134)
anova(lm134)

# Behavior 30: LEADING TO SPAWN (LS.FS) 
# UNIVARIABLE

lm135<-lme(LS.FS~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm135)
anova(lm135)

lm136<-lme(LS.FS~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  
summary(lm136)
anova(lm136)

lm137<-lme(LS.FS~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm137)
anova(lm137)

lm138<-lme(LS.FS~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm138)
anova(lm138)

# Behavior 30: FANNING (FA) 
# UNIVARIABLE

lm139<-lme(FA~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm139)
anova(lm139)

lm140<-lme(FA~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  
summary(lm140)
anova(lm140)

lm141<-lme(FA~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm141)
anova(lm141)

lm142<-lme(FA~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm142)
anova(lm141)

# Behavior 31: MOUTHING (MO) 
# UNIVARIABLE

lm143<-lme(MO~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm143)
anova(lm143)

lm144<-lme(MO~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)  
summary(lm144)
anova(lm144)

lm145<-lme(MO~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm145)
anova(lm145)

lm146<-lme(MO~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm146)
anova(lm146)

# Behavior 32: NEST PREPARING (NP) 
# UNIVARIABLE

lm147<-lme(NP~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm147)
anova(lm147)

lm148<-lme(NP~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) 
summary(lm148)
anova(lm148)

lm149<-lme(NP~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm149)
anova(lm149)

lm150<-lme(NP~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm150)
anova(lm150)

# Behavior 33: NEST CONTROLLING (NC) 
# UNIVARIABLE

lm151<-lme(NC~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm151)
anova(lm151)

lm152<-lme(NC~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm152)
anova(lm152)

lm153<-lme(NC~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm153)
anova(lm153)

lm154<-lme(NC~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) # SIGN. DIFF.
summary(lm154)
anova(lm154)

# BIVARIABLE

lm155<-lme(NC~as.factor(GROUP.SIZE)+as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata)
summary(lm155)
anova(lm155)

lm156<-lme(NC~as.factor(GROUP.SIZE)+(CORAL.SIZE),random=~1|GROUP.ID,data=mydata)
summary(lm156)
anova(lm156)

lm157<-lme(NC~as.factor(GROUP.SIZE)+as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm157)
anova(lm157)

# Behavior 34: REPRODUCTIVE SCRATCHING (SCR) 
# UNIVARIABLE

lm158<-lme(SCR~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm158)
anova(lm158)

lm159<-lme(SCR~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata)
summary(lm159)
anova(lm159)

lm160<-lme(SCR~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm160)
anova(lm160)

lm161<-lme(SCR~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm161)
anova(lm161)

# Behavior 35: PSEUDO SPAWNING (PS) 
# UNIVARIABLE

lm162<-lme(PS~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm162)
anova(lm162)

lm163<-lme(PS~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) # ALMOST SIGNIFICANT
summary(lm163)
anova(lm163)

lm164<-lme(PS~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm164)
anova(lm164)

lm165<-lme(PS~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm165)
anova(lm165)

# Behavior 36: TOTAL SUBMISSIVE BEHAVIORS (TOT.SUBMISSIVE.B) 
# UNIVARIABLE

lm166<-lme(TOT.SUBMISSIVE.B~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm166)
anova(lm166)

lm167<-lme(TOT.SUBMISSIVE.B~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) 
summary(lm167)
anova(lm167)

lm168<-lme(TOT.SUBMISSIVE.B~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm168)
anova(lm168)

lm169<-lme(TOT.SUBMISSIVE.B~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm169)
anova(lm169)

# Behavior 37: SUBMISSIVE DISPLAY (SD) :NONE

# Behavior 38: FLEE (FL) 
# UNIVARIABLE

lm170<-lme(FL~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm170)
anova(lm170)

lm171<-lme(FL~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) 
summary(lm171)
anova(lm171)

lm172<-lme(FL~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm172)
anova(lm172)

lm173<-lme(FL~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm173)
anova(lm173)

# Behavior 38: HIDING (H) 
# UNIVARIABLE

lm174<-lme(H~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm174)
anova(lm174)

lm175<-lme(H~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) 
summary(lm175)
anova(lm175)

lm176<-lme(H~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm176)
anova(lm176)

lm177<-lme(H~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) # ALMOST SIGNIFICANT
summary(lm177)
anova(lm177)

# Behavior 39: HEAD DOWN (HD) :
# UNIVARIABLE

lm178<-lme(HD~as.factor(TREATMENT),random=~1|GROUP.ID,data=mydata) 
summary(lm178)
anova(lm178)

lm179<-lme(HD~as.factor(CORAL.SPECIES),random=~1|GROUP.ID,data=mydata) 
summary(lm179)
anova(lm179)

lm180<-lme(HD~CORAL.SIZE,random=~1|GROUP.ID,data=mydata) 
summary(lm180)
anova(lm180)

lm181<-lme(HD~as.factor(GROUP.SIZE),random=~1|GROUP.ID,data=mydata) 
summary(lm181)
anova(lm181)





head(mydata)













