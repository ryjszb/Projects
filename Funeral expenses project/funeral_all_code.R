# Dec 13, 2016
library(data.table)
library(pscl)
library(lme4)

### Step 1: Data Cleaning
funeral <- read.csv("/Users/lijun/Dropbox/2016Fall/TA_Consulting/8_Funeral_project/Funerals_PositiveVallueExpenses.csv")
funeral <- as.data.frame(funeral)
funeral_sub <- funeral[,c(27, 29, 28, 26, 21, 30, 1)]
funeral_sub <- funeral_sub[as.character(funeral_sub$Category)=="1"|as.character(funeral_sub$Category)=="2", ] # 4233 entries
# all the Expense > 0, only includes Category 1 & 2
funeral_sub$Category <- as.factor(funeral_sub$Category)
funeral_sub$Expense_Type <- as.factor(funeral_sub$Expense_Type)

### Step 2: check variability
boxplot(log(funeral_sub$Expense) ~ funeral_sub$Expense_Type, main = "Expense vs Expense Type Boxplot", xlab = "Expense Type", ylab = "log scale Expense")

fit1<-lmer(log(Expense) ~ Other_Planned + Public + Other_Planned:Public + (1|ID) + (1|Expense_Type), data=funeral_sub)
round(summary(fit1)$coefficient ,2)
#                      Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)              4.55       0.29   25.26   15.52     0.00
# Other_Planned            0.00       0.01  258.50    0.14     0.89
# Public                   1.17       0.51   25.13    2.30     0.03
# Other_Planned:Public     0.04       0.02 3646.92    2.25     0.02

fit1a<-lmer(Expense ~ Other_Planned + Public + Other_Planned:Public + (1|ID) + (1|Expense_Type), data=funeral_sub)
round(summary(fit1a)$coefficient ,2)
#                      Estimate Std. Error t value
# (Intercept)            259.67     125.82    2.06
# Other_Planned            1.73      10.17    0.17
# Public                 225.27     217.08    1.04
# Other_Planned:Public    52.50      14.92    3.52

# alternatives, model comparison
fit2 <- lmer(log(Expense) ~ Other_Planned + Public + (1|ID) + (1|Expense_Type), data=funeral_sub)
fit3 <- lmer(log(Expense) ~ Other_Planned  + (1|ID) + (1|Expense_Type), data=funeral_sub)

anova(fit1, fit2)
anova(fit2, fit3)

### Step 3: z score the data within Expense Type, don't substract the mean
nExpenseType <- length(unique(funeral_sub$Expense_Type))
unique_vec <- as.character(unique(funeral_sub$Expense_Type))
ZExpense <- rep(NA, dim(funeral_sub)[1])
ZlogExpense <- rep(NA, dim(funeral_sub)[1])

for (j in unique_vec){
  curr_id <- (as.character(funeral_sub$Expense_Type)==j)
  ori_scale <- funeral_sub$Expense
  log_scale <- log(funeral_sub$Expense)
  ZExpense[curr_id] <- scale(ori_scale[curr_id], center = FALSE)
  ZlogExpense[curr_id] <- scale(log_scale[curr_id], center = FALSE)
}
funeral_sub <- within(funeral_sub, {
  ZExpense <- ZExpense
  ZlogExpense <- ZlogExpense
})

fit4 <- lmer(ZlogExpense ~ Other_Planned + Public + Other_Planned:Public + (1|ID) + (1|Expense_Type), data=funeral_sub)
round(summary(fit4)$coefficient ,2)
#                      Estimate Std. Error t value
# (Intercept)              0.98       0.01   82.02
# Other_Planned            0.00       0.00   -0.02
# Public                   0.00       0.02   -0.15
# Other_Planned:Public     0.01       0.00    2.20

## all the coefficients are not significant
fit5 <- lmer(ZExpense ~ Other_Planned + Public + Other_Planned:Public + (1|ID) + (1|Expense_Type), data=funeral_sub)
round(summary(fit5)$coefficient ,2)
#                      Estimate Std. Error t value
# (Intercept)              0.94       0.02   44.43
# Other_Planned            0.01       0.01    1.21
# Public                  -0.06       0.04   -1.76
# Other_Planned:Public     0.05       0.02    2.56















