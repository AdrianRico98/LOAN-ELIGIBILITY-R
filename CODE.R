### OVERVIEW.
#loading or installing the libraries.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggthemr)) install.packages("ggthemr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(gtools)) install.packages("gtools", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(gridExtra)
library(ggthemr)
library(readxl)
library(missForest)
library(corrplot)
library(gtools)
library(ggrepel)
library(e1071)

#setting working directory and getting the data into R.
setwd("C:/Users/adria/Desktop/CERTIFICADOS Y APUNTES RELACIONADOS A LOS MISMOS/HARVARD DATA SCIENCE PROFESSIONAL CERTIFICATE/9- CAPSTONE PROJECT/LOAN EGILIBILITY/data")
data <- as.data.frame(read_excel("data.xlsx"))

#getting the first view of the data
str(data)
summary(data)[1:4,1:7] %>% knitr::kable()
summary(data)[,8:13] %>% knitr::kable()

### EXPLORATORY ANALYSIS.

#solving the first problem.
data <- data %>% #correct type of data.
  mutate (Gender = as.factor(Gender),
          Married = as.factor(Married),
          Dependents = as.factor(Dependents),
          Education = as.factor(Education),
          Self_Employed = as.factor(Self_Employed),
          Credit_History = as.factor(Credit_History),
          Property_Area = as.factor(Property_Area),
          Loan_Status = as.factor(Loan_Status))
data <- data %>% select(-Loan_ID) #removing loan_id from the data.
str(data)

#solving the second problem.
ggthemr("flat")
p1 <- data %>%
  ggplot(aes(CoapplicantIncome)) + 
  geom_boxplot() +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Coapplicant income before") 

index <- which(rank(data$CoapplicantIncome) %in% c(613,614)) #keeping only the two biggest numbers, the outliers
outlier_1 <- max(data$CoapplicantIncome[index])
outlier_2 <- min(data$CoapplicantIncome[index])
outliers <- c(outlier_1,outlier_2)

data <-  data %>% mutate(CoapplicantIncome = ifelse(CoapplicantIncome %in% outliers, data$CoapplicantIncome[rank(data$CoapplicantIncome) %in% c(612)], CoapplicantIncome))

p2 <- data %>%
  ggplot(aes(CoapplicantIncome)) + 
  geom_boxplot() +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Coapplicant income after") 

grid.arrange(p1,p2,nrow = 1)

#solving the final problem.
imputation <- missForest(data,variablewise = FALSE)
imputation$OOBerror
names <- c("observations","variables")
dimensions <- dim(data)
data <- imputation$ximp
data.frame(names,dimensions) %>% knitr::kable()

#checking all the changes of the variables.
summary(data)[,1:7] %>% knitr::kable()
summary(data)[,8:12] %>% knitr::kable()

p1 <- data %>%
  ggplot(aes(Gender)) + 
  geom_bar(aes(fill = Gender)) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("Count") + 
  ggtitle("Gender") 

p2 <- data %>%
  ggplot(aes(Married)) + 
  geom_bar(aes(fill = Married)) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Married") 

p3 <- data %>%
  ggplot(aes(Dependents)) + 
  geom_bar(aes(fill = Dependents)) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Dependents") 

p4 <- data %>%
  ggplot(aes(Education)) + 
  geom_bar(aes(fill = Education)) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("Count") + 
  ggtitle("Education") 

p5 <- data %>%
  ggplot(aes(Self_Employed)) + 
  geom_bar(aes(fill = Self_Employed)) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Self Employed") 

p6 <- data %>%
  ggplot(aes(ApplicantIncome)) + 
  geom_histogram(binwidth = 500) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Applicant Income") 

p7 <- data %>%
  ggplot(aes(CoapplicantIncome)) + 
  geom_histogram() +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Coapplicant Income") 

p8 <- data %>%
  ggplot(aes(LoanAmount)) + 
  geom_histogram(binwidth = 20) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Loan Amount") 

p9 <- data %>%
  ggplot(aes(Loan_Amount_Term)) + 
  geom_histogram(bins = 15) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Loan Amount Term") 

p10 <- data %>%
  ggplot(aes(Credit_History)) + 
  geom_bar(aes(fill = Credit_History)) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("Count") + 
  ggtitle("Credit History") 

p11 <- data %>%
  ggplot(aes(Property_Area)) + 
  geom_bar(aes(fill = Property_Area)) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Property Area") 

p12 <- data %>%
  ggplot(aes(Loan_Status)) + 
  geom_bar(aes(fill = Loan_Status)) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Loan Status") 

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, nrow = 4)

#generating the train and test data
set.seed(2021, sample.kind = "Rounding") 
ind <- createDataPartition(data$Loan_Status, times = 1, p = 0.2, list = FALSE)
train_data <- data[-ind,]
test_data <- data[ind,]
x <- dim(train_data)
z <- dim(test_data)
data.frame("Dimensions of Train Data" = x, "Dimensions of Test Data" = z) %>% knitr::kable()

#Watching the correlation between variables
#continous
corr <- cor(train_data %>% select(ApplicantIncome,CoapplicantIncome,LoanAmount,Loan_Amount_Term))
round(corr,2) %>% knitr::kable()
corrplot(corr,
         method = "shade",
         order = "hclust", 
         tl.col='black', 
         tl.cex=0.6,
         tl.srt = 20)
train_data %>% ggplot(aes(LoanAmount,ApplicantIncome)) + 
  geom_point() + 
  xlab("Loan amount") + 
  ylab("Applicant income") + 
  ggtitle("Linear relationship between loan amount and applicant income") +
  annotate("text", x = 350, y = 50000, label = "r = 0.60")
#categorical
combinations <- combinations(7,2, v = c(1:5,10,11)) #all possible combinations of the factors.

##creating the function that performs chi tests. It only needs one argument, one of the 21 pairs of variables contained in the vector "combinations".

chi_test <- function(x){ 
  n <- combinations[x,1]
  z <- combinations[x,2]
  table <- table(train_data[,n],train_data[,z])
  test <- chisq.test(table)
  p_value <- round(test$p.value,3)
  variables <- colnames(train_data[,c(n,z)]) 
  c(variables[1],variables[2],p_value)
}

##applying the function to the 21 combinations and plotting results.
d <- data.frame(sapply(c(1:21),chi_test))
d <- transpose(d)
v1 <- as.character(d[[1]])
v2 <- as.character(d[[2]])
p_value <- as.numeric(d[[3]])
d <- data.frame(v1,v2,p_value)
d %>%
  ggplot(aes(v1,v2,col = p_value, size = p_value)) + 
  geom_point() + 
  geom_text_repel(aes(v1,v2, label = p_value), nudge_y = 0.5, size = 5)  +
  xlab("") + 
  ylab("") + 
  ggtitle("Association test results",
          subtitle = "P-values for the chi-squared tests of association for each pair of variables")

#removing variables based in their correlation.
train_data <- train_data %>% select(-Gender,-Married,-Self_Employed)
test_data <- test_data %>% select(-Gender,-Married,-Self_Employed)

#bivariate analysis.
p1 <- data %>%
  ggplot(aes(Dependents)) + 
  geom_bar(aes(fill = Loan_Status)) +
  xlab("") + 
  ylab("") + 
  ggtitle("Dependents vs Loan status") +
  theme(plot.title = element_text(size=rel(0.7))) 

p2 <- data %>%
  ggplot(aes(Education)) + 
  geom_bar(aes(fill = Loan_Status)) +
  theme(legend.position = "none") +
  xlab("") + 
  ylab("Count") + 
  ggtitle("Education vs Loan status") +
  theme(plot.title = element_text(size=rel(0.7)))

p3 <- data %>%
  ggplot(aes(ApplicantIncome, fill = Loan_Status), colour = "Black", alpha = 0.5) + 
  geom_histogram(binwidth = 500) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Applicant Income vs Loan status") +
  theme(plot.title = element_text(size=rel(0.7)))

p4 <- data %>%
  ggplot(aes(CoapplicantIncome, fill = Loan_Status), colour = "Black", alpha = 0.5) + 
  geom_histogram() +
  theme(legend.position = "none") +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Coapplicant Income vs Loan status") +
  theme(plot.title = element_text(size=rel(0.7)))

p5 <- data %>%
  ggplot(aes(LoanAmount, fill = Loan_Status), colour = "Black", alpha = 0.5) + 
  geom_histogram(binwidth = 20) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Loan Amount vs Loan status") +
  theme(plot.title = element_text(size=rel(0.7)))

p6 <- data %>%
  ggplot(aes(Loan_Amount_Term, fill = Loan_Status), colour = "Black", alpha = 0.5) + 
  geom_histogram(bins = 15) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Loan Amount Term vs Loan status") +
  theme(plot.title = element_text(size=rel(0.7)))

p7 <- data %>%
  ggplot(aes(Credit_History)) + 
  geom_bar(aes(fill = Loan_Status)) +
  theme(legend.position = "none") +
  xlab("") + 
  ylab("Count") + 
  ggtitle("Credit History vs Loan status") +
  theme(plot.title = element_text(size=rel(0.7)))

p8 <- data %>%
  ggplot(aes(Property_Area)) + 
  geom_bar(aes(fill = Loan_Status)) +
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Property Area vs Loan status") +
  theme(plot.title = element_text(size=rel(0.7))) 

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2)

###ALGORITHM TRAINING

#KNN
set.seed(2021, sample.kind = "Rounding") 

knn <- train(Loan_Status ~ ., #algorithm training
             data = train_data, 
             method = "knn",
             tuneGrid = data.frame(k = c(3:10)))

ggplot(knn, highlight = TRUE) +
  ggtitle("Choosing K - Bootstrap Cross Validation") +
  xlab("Neighbors (K)") +
  annotate("text", x = 8 , y = 0.62, label = "K which maximize accuracy = 10")
knn_preds <- predict(knn,test_data[,1:8]) %>% as.factor()

#LR
set.seed(2021, sample.kind = "Rounding") 
lr <- glm(Loan_Status ~ ., data = train_data, family = "binomial")
lr_preds <- predict(lr,test_data[,1:8], type = "response")
lr_preds <- ifelse(lr_preds > 0.7, "Y", "N") %>% as.factor()
round(summary(lr)$coefficients,3) %>% knitr::kable() #coefficients
round(exp(lr$coefficients[-1]),3) %>% knitr::kable() #odd ratios

#RF
rf <- train(Loan_Status ~ .,
            method = "rf",
            tuneGrid = data.frame(mtry = c(1:8)),
            data = train_data)

ggplot(rf,highlight = TRUE) + 
  ggtitle("Choosing mtry - Bootstrap Cross Validation") +
  xlab("Randomly selected predictors") +
  annotate("text", x = 3.5 , y = 0.75, label = "mtry which maximize accuracy = 2")
rf_preds <- predict(rf,test_data[,1:8]) %>% as.factor()
round(varImp(rf)$importance,2) %>% knitr::kable()

#SVM
set.seed(2021, sample.kind = "Rounding") 
validation <- tune.svm(Loan_Status ~ ., #choosing cost with kfold cross validation
                       data = train_data,
                       cost = seq(0.1,5,0.1),
                       kernel = "radial",
                       class.weights = c(Y = 0.3, N = 0.7))

cost <- validation[["performances"]][["cost"]]
error <- validation[["performances"]][["error"]]
tibble(cost,error) %>%
  ggplot(aes(cost,error)) +
  geom_line() +
  xlab("Cost") +
  ylab("Error") + 
  ggtitle("Choosing cost - K fold Cross Validation") +
  annotate("text", x = 2 , y = 0.5, label = "Cost which minimize error = 0.6")

cost <- validation[["best.parameters"]][["cost"]]
svm <- svm(Loan_Status ~ .,
           data = train_data, 
           kernel = "radial",
           cost = cost,
           class.weights = c(Y = 0.3, N = 0.7))
svm_preds <- predict(svm,test_data[,1:8]) %>% as.factor()

#ENSEMBLE
##generating the predictions on train data to compute F1 scores.
lr_p <- predict(lr,train_data[,1:8]) 
lr_p <- ifelse(lr_p > 0.7, "Y", "N") %>% as.factor()
knn_p <- predict(knn,train_data[,1:8]) %>% as.factor()
rf_p <- predict(rf,train_data[,1:8]) %>% as.factor()
svm_p <- predict(svm,train_data[,1:8]) %>% as.factor()

f1_total <- F_meas(svm_p,train_data$Loan_Status) + 
  F_meas(lr_p,train_data$Loan_Status) +
  F_meas(rf_p,train_data$Loan_Status) +
  F_meas(knn_p,train_data$Loan_Status)

f1_svm <- F_meas(svm_p,train_data$Loan_Status)/f1_total
f1_rf <-  F_meas(rf_p,train_data$Loan_Status)/f1_total
f1_knn <- F_meas(knn_p,train_data$Loan_Status)/f1_total
f1_lr <- F_meas(lr_p,train_data$Loan_Status)/f1_total

defining_n <- function(n){
  ensembling_pred <- data.frame(svm_ = ifelse(svm_p == "Y",1,2),
                                lr_ = ifelse(lr_p == "Y",1,2),
                                knn_ = ifelse(knn_p == "Y",1,2),
                                rf_ = ifelse(rf_p == "Y",1,2)) %>% 
    mutate(svm_w = svm_ * f1_svm,
           lr_w = lr_ * f1_lr,
           rf_w = rf_ * f1_rf,
           knn_w = knn_ * f1_knn,
           ensemble_sum = svm_w + rf_w + lr_w + knn_w,
           ensemble_preds = as.factor(ifelse(ensemble_sum > n, "N","Y")))
  
  cm <- confusionMatrix(ensembling_pred$ensemble_preds,train_data$Loan_Status) 
  accuracy <- cm[["overall"]][["Accuracy"]]
  sensitivity <- cm[["byClass"]][["Sensitivity"]]
  specificity <- cm[["byClass"]][["Specificity"]]
  c(accuracy,sensitivity,specificity)
}

results <- as.data.frame(sapply(seq(1,1.5,0.05),defining_n))
results <- `colnames<-` (results, c("1","1.05","1.10","1.15","1.20","1.25","1.30","1.35","1.40","1.45","1.50"))
results <- round(results,4) %>%
  mutate(Measures = c("Accuracy","Specificity","Sensibility")) %>%
  select("Measures","1","1.05","1.10","1.15","1.20","1.25","1.30","1.35","1.40","1.45","1.50")
results %>%
  knitr::kable()
chosen_n <- 1.1

ensembling_pred <- data.frame(svm_ = ifelse(svm_preds == "Y",1,2),
                              lr_ = ifelse(lr_preds == "Y",1,2),
                              knn_ = ifelse(knn_preds == "Y",1,2),
                              rf_ = ifelse(rf_preds == "Y",1,2)) %>% 
  mutate(svm_w = svm_ * f1_svm,
         lr_w = lr_ * f1_lr,
         rf_w = rf_ * f1_rf,
         knn_w = knn_ * f1_knn,
         ensemble_sum = svm_w + rf_w + lr_w + knn_w,
         ensemble_preds = as.factor(ifelse(ensemble_sum > chosen_n, "N","Y")))

ensemble_preds <- ensembling_pred$ensemble_preds

###RESULTS
#confussion matrix
#Obtaining confusion matrix of the models on test data                     
c_ensemble <- as.data.frame(confusionMatrix(ensemble_preds,test_data$Loan_Status)$table)                
c_lr <- as.data.frame(confusionMatrix(lr_preds,test_data$Loan_Status)$table)    
c_knn <- as.data.frame(confusionMatrix(knn_preds,test_data$Loan_Status)$table)    
c_svm <- as.data.frame(confusionMatrix(svm_preds,test_data$Loan_Status)$table)    
c_rf <- as.data.frame(confusionMatrix(rf_preds,test_data$Loan_Status)$table)    



#Plotting them.
#knn
plotTable_1 <- c_knn %>%
  mutate(goodbad = ifelse(c_knn$Prediction == c_knn$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))


c_1 <- ggplot(data = plotTable_1, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  xlim(rev(levels(c_knn$Reference))) +
  ggtitle("Confusion Matrix - KNN",
          subtitle = "Overall accuracy = 69.35%") +
  theme(plot.title = element_text(size=rel(0.8)))

#logistic regression
plotTable_2 <- c_lr %>%
  mutate(goodbad = ifelse(c_lr$Prediction == c_lr$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))


c_2 <- ggplot(data = plotTable_2, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  xlim(rev(levels(c_lr$Reference))) +
  ggtitle("Confusion Matrix - Logistic Regression",
          subtitle = "Overall accuracy = 76.61%") +
  theme(plot.title = element_text(size=rel(0.8)))

#random forest
plotTable_3 <- c_rf %>%
  mutate(goodbad = ifelse(c_rf$Prediction == c_rf$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))


c_3 <- ggplot(data = plotTable_3, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  xlim(rev(levels(c_rf$Reference))) +
  ggtitle("Confusion Matrix - Random Forest",
          subtitle = "Overall accuracy = 83.06%") +
  theme(plot.title = element_text(size=rel(0.8)))

#supporting vector machine
plotTable_4 <- c_svm %>%
  mutate(goodbad = ifelse(c_svm$Prediction == c_svm$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))


c_4 <- ggplot(data = plotTable_4, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  xlim(rev(levels(c_svm$Reference))) +
  ggtitle("Confusion Matrix - SVM",
          subtitle = "Overall accuracy = 81.45%") +
  theme(plot.title = element_text(size=rel(0.8)))

#supporting vector machine
plotTable_5 <- c_ensemble %>%
  mutate(goodbad = ifelse(c_ensemble$Prediction == c_ensemble$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))


c_5 <- ggplot(data = plotTable_5, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  xlim(rev(levels(c_ensemble$Reference))) +
  ggtitle("Confusion Matrix - Ensemble model",
          subtitle = "Overall accuracy = 73.39%") +
  theme(plot.title = element_text(size=rel(0.8)))

grid.arrange(c_1,c_2,c_3,c_4,c_5,nrow = 2)

#hypothetical situation
c_svm <- c_svm %>% mutate(error = ifelse(Prediction == "N" & Reference == "Y",
                                         "risk",
                                         ifelse(Prediction == "Y" & Reference == "N", "oportunity loss", "good")),
                          loss = ifelse(error == "risk",
                                        100000 * Freq,
                                        ifelse(error == "oportunity loss",15000 * Freq,0)),
                          model = rep("svm",4))

c_knn <- c_knn %>% mutate(error = ifelse(Prediction == "N" & Reference == "Y",
                                         "risk",
                                         ifelse(Prediction == "Y" & Reference == "N", "oportunity loss", "good")),
                          loss = ifelse(error == "risk",
                                        100000 * Freq,
                                        ifelse(error == "oportunity loss",15000 * Freq,0)),
                          model = rep("knn",4))  
c_lr <- c_lr %>% mutate(error = ifelse(Prediction == "N" & Reference == "Y",
                                       "risk",
                                       ifelse(Prediction == "Y" & Reference == "N", "oportunity loss", "good")),
                        loss = ifelse(error == "risk",
                                      100000 * Freq,
                                      ifelse(error == "oportunity loss",15000 * Freq,0)),
                        model = rep("lr",4))
c_rf <- c_rf %>% mutate(error = ifelse(Prediction == "N" & Reference == "Y",
                                       "risk",
                                       ifelse(Prediction == "Y" & Reference == "N", "oportunity loss", "good")),
                        loss = ifelse(error == "risk",
                                      100000 * Freq,
                                      ifelse(error == "oportunity loss",15000 * Freq,0)),
                        model = rep("rf",4))  
c_ensemble <- c_ensemble %>% mutate(error = ifelse(Prediction == "N" & Reference == "Y",
                                                   "risk",
                                                   ifelse(Prediction == "Y" & Reference == "N", "oportunity loss", "good")),
                                    loss = ifelse(error == "risk",
                                                  100000 * Freq,
                                                  ifelse(error == "oportunity loss",15000 * Freq,0)),
                                    model = rep("ensemble",4)) 

economic_loss <- union(c_knn,c_lr) %>% union(c_rf) %>% union(c_svm) %>% union(c_ensemble)

economic_loss %>%
  group_by(model) %>%
  summarise(sum_loss = sum(loss)) %>%
  ggplot(aes(model, sum_loss, fill = model)) + 
  geom_col(size = 2) +
  xlab("Model") +
  ylab("Loss") +
  ggtitle("Presumed loss generated by each model on the test data") +
  theme(legend.position = "none")

### CONCLUSIONS
prediction_reality <- tibble(knn = knn_preds,
                             lr = lr_preds,
                             rf = rf_preds,
                             svm = svm_preds,
                             reality = test_data$Loan_Status,
                             credit_history = test_data$Credit_History) %>%
  mutate(prediction_caos = ifelse(knn == lr &
                                    lr == rf &
                                    rf == svm &
                                    reality != knn &
                                    reality != lr &
                                    reality != rf &
                                    reality != svm, "TRUE","FALSE"))  %>%
  filter(prediction_caos == "TRUE")

prediction_reality %>%
  select(-prediction_caos) %>%
  knitr::kable()