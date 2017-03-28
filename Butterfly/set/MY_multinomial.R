Masanao Quick 
------------------------------

```{r}
library(data.table)
library(ggplot2)
library(MASS)
library(pscl)
library(knitr)

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
```

```{r,echo=FALSE, fig.width=8,fig.height=4,message=FALSE}
# plot
ggplot(dt_comb[,list(counts=sum(count)),by=c("location","butterfly")])+
  geom_bar(stat="identity")+
  aes(x=butterfly,y=counts)+
  facet_grid(.~location)
 dcast(dt_comb,location~butterfly,value.var="count",sum)

```

```{r}
dt_cal<-subset(dt_comb,dt_comb$location=="California")
dt_idh<-subset(dt_comb,dt_comb$location=="Idaho")

# using log link
loglm_fit_1_b <- glm(count~location+butterfly*butterfly:location,data=dt_comb,family=poisson)
loglm_fit_1_c <- glm(count~ butterfly,data=dt_cal,family=poisson)
loglm_fit_1_i <- glm(count~ butterfly,data=dt_idh,family=poisson)
summary(loglm_fit_1_b)
summary(loglm_fit_1_c)
summary(loglm_fit_1_i)

# different link
loglm_fit_2_b <- glm(count~location*butterfly+butterfly:location,data=dt_comb,family=poisson(link="sqrt"))
loglm_fit_2_c <- glm(count~butterfly ,data=dt_cal,family=poisson(link="sqrt"))
loglm_fit_2_i <- glm(count~butterfly ,data=dt_idh,family=poisson(link="sqrt"))
summary(loglm_fit_2_b)
summary(loglm_fit_2_c)
summary(loglm_fit_2_i)


# treating it as binomial with success out of 20 trials
loglm_fit_3_b <- glm(cbind(count,not_attacked)~butterfly*location,data=dt_comb,family=binomial)
loglm_fit_3_c <- glm(cbind(count,not_attacked)~butterfly,data=dt_cal ,family=binomial)
loglm_fit_3_i <- glm(cbind(count,not_attacked)~butterfly,data=dt_idh ,family=binomial)
summary(loglm_fit_3_b)
summary(loglm_fit_3_c)
summary(loglm_fit_3_i)

# negative binomial 
loglm_fit_4_b<-glm.nb(count~location*butterfly,data=dt_comb)
loglm_fit_4_c<-glm.nb(count~butterfly,data=dt_cal )
loglm_fit_4_i<-glm.nb(count~butterfly,data=dt_idh )
summary(loglm_fit_4_b)
summary(loglm_fit_4_c)
summary(loglm_fit_4_i)

# zero inflated negative binomial
loglm_fit_5_b<-zeroinfl(count~location*butterfly,dist="negbin",data=dt_comb)
loglm_fit_5_c<-zeroinfl(count~butterfly,dist="negbin",data=dt_cal )
loglm_fit_5_i<-zeroinfl(count~butterfly,dist="negbin",data=dt_idh )
summary(loglm_fit_5_b)
summary(loglm_fit_5_c)
summary(loglm_fit_5_i)

# hurdle negative binomial
loglm_fit_6_b<-hurdle(count~location*butterfly,dist="negbin",data=dt_comb)
loglm_fit_6_c<-hurdle(count~butterfly,dist="negbin",data=dt_cal )
loglm_fit_6_i<-hurdle(count~butterfly,dist="negbin",data=dt_idh )
summary(loglm_fit_6_b)
summary(loglm_fit_6_c)
summary(loglm_fit_6_i)



# correcting for multiple comparison
library(multcomp)
#summary(glht(loglm_fit_1_b,mcp(butterfly="Tukey")))
summary(glht(loglm_fit_1_c,mcp(butterfly="Tukey")))
summary(glht(loglm_fit_1_i,mcp(butterfly="Tukey")))

#summary(glht(loglm_fit_2_b,mcp(butterfly="Tukey")))
summary(glht(loglm_fit_2_c,mcp(butterfly="Tukey")))
summary(glht(loglm_fit_2_i,mcp(butterfly="Tukey")))

#summary(glht(loglm_fit_3_b,mcp(butterfly="Tukey")))
summary(glht(loglm_fit_3_c,mcp(butterfly="Tukey")))
summary(glht(loglm_fit_3_i,mcp(butterfly="Tukey")))

#	summary(glht(loglm_fit_4_b,mcp(butterfly="Tukey")))
summary(glht(loglm_fit_4_c,mcp(butterfly="Tukey")))
summary(glht(loglm_fit_4_i,mcp(butterfly="Tukey")))
```

```{r,echo=FALSE, fig.width=18,fig.height=4 ,message=FALSE}


Model_p    = c("Poisson (log)","Poisson (sqrt)","Binomial","Negative Binomial")

p_table_Combined  =cbind(summary(loglm_fit_1_b)$coef[,4],summary(loglm_fit_2_b)$coef[,4],summary(loglm_fit_3_b)$coef[,4],summary(loglm_fit_4_b)$coef[,4])

p_table_California=cbind(summary(loglm_fit_1_c)$coef[,4],summary(loglm_fit_2_c)$coef[,4],summary(loglm_fit_3_c)$coef[,4],summary(loglm_fit_4_c)$coef[,4])
p_table_Idaho     =cbind(summary(loglm_fit_1_i)$coef[,4],summary(loglm_fit_2_i)$coef[,4],summary(loglm_fit_3_i)$coef[,4],summary(loglm_fit_4_i)$coef[,4])
colnames(p_table_Combined)<-Model_p
colnames(p_table_California)<-Model_p
colnames(p_table_Idaho)<-Model_p

p1<-ggplot(melt(p_table_Combined[-1,]))+aes(x=Var2,y=Var1,fill=log10(value))+
geom_tile()+scale_fill_gradient2(midpoint=log10(0.05),limits=c(log10(0.001),log10(1)))

p2<-ggplot(melt(p_table_California[-1,]))+aes(x=Var2,y=Var1,fill=log10(value))+geom_tile()+scale_fill_gradient2(midpoint=log10(0.05),limits=c(log10(0.001),log10(1)))

p3<-ggplot(melt(p_table_Idaho[-1,]))+aes(x=Var2,y=Var1,fill=log10(value))+geom_tile()+scale_fill_gradient2(midpoint=log10(0.05),limits=c(log10(0.001),log10(1)))
library(gridExtra)
grid.arrange(p1,p2,p3,ncol=3)
```

# AIC
```{r,echo=FALSE, fig.width=6,fig.height=5 ,message=FALSE}
aic_table<-data.frame(
Model    = c("Poisson (log)","Poisson (sqrt)","Binomial","Negative Binomial","ZINB","HurdleNB"),
Combined  =c(AIC(loglm_fit_1_b),AIC(loglm_fit_2_b),AIC(loglm_fit_3_b),AIC(loglm_fit_4_b),AIC(loglm_fit_5_b),AIC(loglm_fit_6_b)),
California=c(AIC(loglm_fit_1_c),AIC(loglm_fit_2_c),AIC(loglm_fit_3_c),AIC(loglm_fit_4_c),AIC(loglm_fit_5_c),AIC(loglm_fit_6_c)),
Idaho     =c(AIC(loglm_fit_1_i),AIC(loglm_fit_2_i),AIC(loglm_fit_3_i),AIC(loglm_fit_4_i),AIC(loglm_fit_5_i),AIC(loglm_fit_6_i))
)

kable(aic_table,digits=2)
```
# Deviance
```{r,echo=FALSE, fig.width=6,fig.height=5 ,message=FALSE}
deviance_table<-data.frame(
Model    = c("Poisson (log)","Poisson (sqrt)","Binomial","Negative Binomial"),
Combined  =c(deviance(loglm_fit_1_b),deviance(loglm_fit_2_b),deviance(loglm_fit_3_b),deviance(loglm_fit_4_b)),
California=c(deviance(loglm_fit_1_c),deviance(loglm_fit_2_c),deviance(loglm_fit_3_c),deviance(loglm_fit_4_c)),
Idaho     =c(deviance(loglm_fit_1_i),deviance(loglm_fit_2_i),deviance(loglm_fit_3_i),deviance(loglm_fit_4_i))
)

kable(deviance_table,digits=2)
```

# Multinomial model

```{r,echo=FALSE, fig.width=6,fig.height=5 ,message=FALSE}
head(cf_comb)

# Treating it as difference in food choice for butterflies
library(VGAM)
fit_multinom<- vglm(formula=cbind(W,L,A,J)~location,family=multinomial,data=cf_comb)
summary(fit_multinom)
fit_multinom_0<- vglm(formula=cbind(W,L,A,J)~1,family=multinomial,data=cf_comb)
anova(fit_multinom_0,fit_multinom)

AIC(fit_multinom)
AIC(fit_multinom_0)
```

# ANOVA with quasipoisson

```{r,echo=FALSE, fig.width=6,fig.height=5 ,message=FALSE}
loglm_fit_2_b  <- glm(count~location*poison,data=dt_comb, family=quasipoisson )
loglm_fit_2_b_0<- glm(count~location       ,data=dt_comb, family=quasipoisson )
loglm_fit_2_c  <- glm(count~poison         ,data=dt_cal , family=quasipoisson )
loglm_fit_2_c_0<- glm(count~1              ,data=dt_cal , family=quasipoisson )
loglm_fit_2_i  <- glm(count~poison         ,data=dt_idh , family=quasipoisson )
loglm_fit_2_i_0<- glm(count~1              ,data=dt_idh , family=quasipoisson )
anova(loglm_fit_2_b_0,loglm_fit_2_b,test="Chisq")
anova(loglm_fit_2_c_0,loglm_fit_2_c,test="Chisq")
anova(loglm_fit_2_i_0,loglm_fit_2_i,test="Chisq")

```



