
## Let us first import our dataset for modeling
setwd("D:/K2Analytics/Datafile")

LR_DF <- read.table("LR_DF.csv",sep = ",", header = T)
View(LR_DF)

summary(LR_DF)


## See the percentile distribution
quantile(LR_DF$Balance, 
         c(0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1))


quantile(LR_DF$Age, 
         c(0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1))

## What if I want the percentile distribution for all the fields
apply(LR_DF[,sapply(LR_DF, is.numeric)], 
      2, quantile, 
      probs=c(0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
      na.rm=T)



boxplot(LR_DF$Balance , 
             main= "Balance Box Plot" ,
             xlab = "Overall Base",
            )



## Typically we floor and cap the variables at P1 and P99. 
## Let us cap the Balance variable at P99.
LR_DF$BAL_CAP <- 
  ifelse(LR_DF$Balance > 723000, 723000, LR_DF$Balance)

summary(LR_DF$BAL_CAP)
sd(LR_DF$BAL_CAP)

quantile(LR_DF$BAL_CAP, 
         c(0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1))


## Missing Value Imputation for Holding Period
#### Creating a function to decile the records function
decile <- function(x){
deciles <- vector(length=10)
for (i in seq(0.1,1,.1)){
deciles[i*10] <- quantile(x, i, na.rm=T)
}
return (
ifelse(x<deciles[1], 1,
ifelse(x<deciles[2], 2,
ifelse(x<deciles[3], 3,
ifelse(x<deciles[4], 4,
ifelse(x<deciles[5], 5,
ifelse(x<deciles[6], 6,
ifelse(x<deciles[7], 7,
ifelse(x<deciles[8], 8,
ifelse(x<deciles[9], 9, 10
))))))))))
}



tmp <- LR_DF
tmp$deciles <- decile(tmp$Holding_Period)

library(data.table)
tmp_DT = data.table(tmp)
RRate <- tmp_DT[, list(
  min_hp = min(Holding_Period), 
  max_hp = max(Holding_Period), 
  avg_hp = mean(Holding_Period),
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(deciles)]
RRate$rrate <- RRate$cnt_resp * 100 / RRate$cnt;
View(RRate)

rm(tmp)

LR_DF$HP_Imputed <- ifelse(is.na(LR_DF$Holding_Period), 
                           18, LR_DF$Holding_Period)


## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
##m = table(LR_DF$Target ,LR_DF$Occupation)
##prop.table(m) * 100 # Relative frequency scaled up to a percentage
##prop.table(m,1) # Scale cells by the row sum
##prop.table(m,2) # Scale cells by the column sum


ctab <- xtabs(~Target + Occupation, data = LR_DF)
ctab
class(LR_DF$Occupation)
LR_DF$Occupation <- as.character(LR_DF$Occupation)
LR_DF$OCC_Imputed <- ifelse(LR_DF$Occupation=="", 
                            "MISSING", LR_DF$Occupation)
table(LR_DF$OCC_Imputed)



## Let us find the variables Information Value
##install.packages("devtools")
library(devtools)
##install_github("riv","tomasgreif")
library(woe)

iv.plot.summary(iv.mult(LR_DF[,!names(LR_DF) %in% c("Cust_ID")],
                        "Target",TRUE))

iv <- iv.mult(LR_DF[,!names(LR_DF) %in% c("Cust_ID")],
        "Target",TRUE)

iv
## Pattern Detection

source("D:/K2Analytics/Logistic_Regression/RScripts/Visualization.R")
output_folder = "D:/K2Analytics/Logistic_Regression/VISUALIZATIONS1/"
Target_var_name = "Target"

col_list = colnames(LR_DF)[
  lapply(LR_DF, class) %in% c("numeric", "integer")
  ]
col_list
for (i in 1 : length(col_list)) {
  fn_biz_viz(df = LR_DF, target = Target_var_name, var = col_list[i])
}


LR_DF$DV_Age <- ifelse(LR_DF$Age > 43, 43 - (LR_DF$Age - 43), LR_DF$Age)


##LR_DF$No_OF_CR_TXNS_ln <- log(LR_DF$No_OF_CR_TXNS + 1)



mydata <- LR_DF

mydata$random <- runif(nrow(mydata), 0, 1)
mydata.dev <- mydata[which(mydata$random <= 0.5),]
mydata.val <- mydata[which(mydata$random > 0.5 
                           & mydata$random <= 0.8 ),]
mydata.hold <- mydata[which(mydata$random > 0.8),]
nrow(mydata)
nrow(mydata.dev)
nrow(mydata.val)
nrow(mydata.hold)

sum(mydata$Target) / nrow(mydata)
sum(mydata.dev$Target)/ nrow(mydata.dev)
sum(mydata.val$Target)/ nrow(mydata.val)
sum(mydata.hold$Target)/ nrow(mydata.hold)


##install.packages("aod")
##install.packages("ggplot2")
##library(aod)
##library(ggplot2)
## Running Regression Process
mylogit <- glm(
  Target ~  DV_Age + Gender + OCC_Imputed 
  + SCR + Balance + No_OF_CR_TXNS + HP_Imputed , 
  data = mydata.dev, family = "binomial"
)
summary(mylogit)


## After dropping Gender Variable
mylogit <- glm(
  Target ~  DV_Age +  OCC_Imputed 
  + SCR + Balance + No_OF_CR_TXNS + HP_Imputed , 
  data = mydata.dev, family = "binomial"
)
summary(mylogit)




## We need to treat Occupation Variable

pp <- as.data.frame.matrix(table(mydata.dev$OCC_Imputed, mydata.dev$Target))
pp$total <- (pp$`0` + pp$`1`)
pp$rrate <- round(pp$`1` * 100 / (pp$`0` + pp$`1`), 3)
pp

mydata.dev$DV_OCC = ifelse ( mydata.dev$OCC_Imputed %in% c("SAL", "SENP"), "SAL-SENP",
                              ifelse (
                                mydata.dev$OCC_Imputed %in% c("MISSING", "PROF"), "MISSING-PROF",
                                mydata.dev$OCC_Imputed
                                )
                            )
table(mydata.dev$DV_OCC)

## After creating new Derived Occupation Categories
mylogit <- glm(
  Target ~  DV_Age +  DV_OCC 
  + SCR + Balance + No_OF_CR_TXNS + HP_Imputed , 
  data = mydata.dev, family = "binomial"
)
summary(mylogit)


##install.packages("car")
library(car)
vif(mylogit)



## Rank Ordering Test
## Calculating the probabilities and create deciles
mydata.dev$prob <- predict(mylogit, mydata.dev, type="response")
mydata.dev$deciles <- decile(mydata.dev$prob)

##install.packages("data.table")
##install.packages("scales")
library(data.table)
library(scales)

tmp_DT = data.table(mydata.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,3);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),3);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),3);
rank$ks <- percent(abs(rank$cum_rel_resp - rank$cum_rel_non_resp));
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)



############ Goodness of Fit: ##############
# A function to do the Hosmer-Lemeshow test in R.
# R Function is due to Peter D. M. Macdonald, McMaster University.
 


hosmerlem <-
  function (y, yhat, g = 10) 
  {
    cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 
                                                             1, 1/g)), include.lowest = T)
    obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
    expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
    chisq <- sum((obs - expect)^2/expect)
    P <- 1 - pchisq(chisq, g - 2)
    c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
  }

hl_gof = hosmerlem(mydata.dev$Target, mydata.dev$prob )
hl_gof


#***FUNCTION TO CALCULATE CONCORDANCE AND DISCORDANCE***#
concordance=function(y, yhat)
{
  Con_Dis_Data = cbind(y, yhat) 
  ones = Con_Dis_Data[Con_Dis_Data[,1] == 1,]
  zeros = Con_Dis_Data[Con_Dis_Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}
#***FUNCTION TO CALCULATE CONCORDANCE AND DISCORDANCE ENDS***#


concordance_output = concordance(mydata.dev$Target, mydata.dev$prob)
concordance_output


##library(sqldf)
##sqldf('select deciles, count(1) as cnt, 
  ##    sum(Target) as Obs_Resp, 
  ##    count(Target==0) as Obs_Non_Resp, 
  ##    sum(prob) as Exp_Resp,
  ##    sum(1-prob) as Exp_Non_Resp 
  ##    from test
  ##    group by deciles
  ##    order by deciles desc')



############ GINI Index ##############
##install.packages("ineq")
library(ineq)
gini = ineq(mydata.dev$prob, type="Gini")
gini



### Calculating AUC using ROC Curve and KS for the model
##install.packages("ROCR")
library(ROCR)
pred <- prediction(mydata.dev$prob, mydata.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf, col="green", lwd=2, main="ROC Curve")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
KS
auc




####### MODEL VALIDATION #########


mydata.val$DV_OCC = ifelse ( 
  mydata.val$OCC_Imputed %in% c("SAL", "SENP"), "SAL-SENP",
  ifelse (
   mydata.val$OCC_Imputed %in% c("MISSING", "PROF"), "MISSING-PROF",
   mydata.val$OCC_Imputed
  )
)


mylogit_val <- glm(
  Target ~  DV_Age +  DV_OCC 
  + SCR + Balance + No_OF_CR_TXNS + HP_Imputed , 
  data = mydata.val, family = "binomial"
)
summary(mylogit_val)
