#Question 1
bankruptcy.data = as.data.frame(read.csv("C:/Users/Cassie Gill/OneDrive/SCMA 851/CGill HW4/SCMA 851 HW4/BankruptcyData.csv", header = TRUE, stringsAsFactors=T,
sep = ","))
head(bankruptcy.data)
summary(bankruptcy.data)
bankruptcy.data <- subset(bankruptcy.data, select = -fyear)
bankruptcy.data <- subset(bankruptcy.data, select = -oiadp)
bankruptcy.data <- subset(bankruptcy.data, select = -revt)
summary(bankruptcy.data)

#Question 2
set.seed(1)
train.index = sample(c(1:dim(bankruptcy.data)[1]), dim(bankruptcy.data)[1]*0.7)
train.data = bankruptcy.data[train.index, ]
test.data = bankruptcy.data[-train.index, ]
logit.reg = glm(formula = Bankrupt ~., data = train.data, family = "binomial")
options(scipen=999)
summary(logit.reg)

logit.reg.pred = predict(logit.reg, test.data, type = "response")
data.frame(actual = test.data$Bankrupt, predicted = logit.reg.pred)
exp(coef(logit.reg))
exp(cbind(OR = coef(logit.reg), confint(logit.reg)))

#Question 3
library(car)
vif(logit.reg)
logit.reg = glm(formula = Bankrupt ~ bkvlps + invt + Lt + rectr +
cogs + dvt + gp + dvpsx_f + mkvalt , data = train.data, family = "binomial")

#Question 4
confusionmatrix=table(test.data$Bankrupt, logit.reg.pred>0.5)
confusionmatrix
write.csv(confusionmatrix,"confusion_matrix.csv")
sum(diag(confusionmatrix))/sum(sum(confusionmatrix))
confusionmatrix[2, 2]/(confusionmatrix[2, 2] + confusionmatrix[1, 2])
confusionmatrix[2, 2]/(confusionmatrix[2, 2] + confusionmatrix[2, 1])
(2*confusionmatrix[2, 2])/((2*confusionmatrix[2, 2]) + confusionmatrix[2, 1]+confusionmatrix[1, 2])

#Question 5
library(pROC)
log.reg.pred <- predict(logit.reg, test.data, decision.values=TRUE, type="response")  
par(pty="s")
test.data_prob = as.numeric(predict(logit.reg, test.data, decision.values=TRUE,type = "response"))-1
test.data_roc = roc(test.data$Bankrupt ~ test.data_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)
auc(test.data_roc)
par(pty="m")

#question 6
library(rpart) 
library(party) 
library(partykit)
library(caret)
library(caTools)

set.seed(1)
train2 = sample(c(1:dim(bankruptcy.data)[1]), dim(bankruptcy.data)[1]*0.7)
DTtrain = bankruptcy.data[train.index, ]
DTtest = bankruptcy.data[-train.index, ]
DT.1 <- rpart(Bankrupt ~., method="class", data=DTtest)
plot(DT.1)
text(DT.1)
plot(as.party(DT.1))
printcp(DT.1)
plotcp(DT.1)

#Question 7
DT.1pred <- predict(DT.1, DTtest, decision.values=TRUE, type="class")
DT.1CM=table(DT.1pred = DT.1pred, true = DTtest$Bankrupt)
DT.1CM
sum(diag(DT.1CM))/sum(sum(DT.1CM))
DT.1CM[2, 2]/(DT.1CM[2, 2] + DT.1CM[1, 2])
DT.1CM[2, 2]/(DT.1CM[2, 2] + DT.1CM[2, 1])
(2*DT.1CM[2, 2])/((2*DT.1CM[2, 2]) + DT.1CM[2, 1]+DT.1CM[1, 2])

library(pROC)
DT.1pred <- predict(DT.1, DTtest, decision.values=TRUE, type="class")  
par(pty="s")
DTtest_prob = as.numeric(predict(DT.1, DTtest, decision.values=TRUE,type = "class"))-1
DTtest_roc = roc(DTtest$Bankrupt ~ DTtest_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)
auc(DTtest_roc)
par(pty="m")

#Question 8
m <- which.min(DT.1$cptable[, "xerror"])
DT.1$cptable[m, "CP"]
DT.2 <- prune(DT.1, cp = DT.1$cptable[which.min(DT.1$cptable[, "xerror"]), "CP"])
plot(as.party(DT.2))
printcp(DT.2)
plotcp(DT.2)
DT.2pred <- predict(DT.2, DTtest, decision.values=TRUE, type="class") 

DT.2CM=table(DT.2pred = DT.2pred, true = DTtest$Bankrupt)
DT.2CM
sum(diag(DT.2CM))/sum(sum(DT.2CM))
DT.2CM[2, 2]/(DT.2CM[2, 2] + DT.2CM[1, 2])
DT.2CM[2, 2]/(DT.2CM[2, 2] + DT.2CM[2, 1])
(2*DT.2CM[2, 2])/((2*DT.2CM[2, 2]) + DT.2CM[2, 1]+DT.2CM[1, 2])

library(pROC)
DT.2pred <- predict(DT.2, DTtest, decision.values=TRUE, type="class")  
par(pty="s")
DTtest_prob = as.numeric(predict(DT.2, DTtest, decision.values=TRUE,type = "class"))-1
DTtest_roc = roc(DTtest$Bankrupt ~ DTtest_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)
auc(DTtest_roc)
par(pty="m")

#Question 10
bankruptcynew.data = as.data.frame(read.csv("C:/Users/Cassie Gill/OneDrive/SCMA 851/CGill HW4/SCMA 851 HW4/BankruptcyNew.csv", header= TRUE, stringsAsFactors=T, sep = ","))
head(bankruptcynew.data)
summary(bankruptcynew.data)
bankruptcynew.data <- subset(bankruptcynew.data, select = -fyear)
bankruptcynew.data <- subset(bankruptcynew.data, select = -oiadp)
bankruptcynew.data <- subset(bankruptcynew.data, select = -revt)
summary(bankruptcynew.data)
Final.Pred = predict(DT.1, bankruptcynew.data, type = "class") 
Final.Pred
