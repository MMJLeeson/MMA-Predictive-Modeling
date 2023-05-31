#####################################
### MMA867 Project - Team Dunning ###
#####################################

## Load Libraries
library(mice)
library(fastDummies)
library(dplyr)
library(caTools)
library(ROSE)
library(glmnet)
library(caret)
library(corrplot)
library(randomForest)
library(ggplot2)
library(ggcorrplot)
library(pROC)
library(rpart)
library(MASS)
library(ROCR)

#################### Load Data #################### 
HS_data <- read.csv(file.choose()) #load data
HS_data_plot <- HS_data #for plotting


#################### Data Cleaning #################### 

# Data type transformation
HS_data$gender<-as.factor(HS_data$gender)
HS_data$ever_married <-as.factor(HS_data$ever_married)
HS_data$work_type <-as.factor(HS_data$work_type)
HS_data$Residence_type <-as.factor(HS_data$Residence_type)
HS_data$smoking_status <-as.factor(HS_data$smoking_status)
HS_data$stroke <-as.factor(ifelse(HS_data$stroke=="0","No","Yes"))
HS_data$hypertension <-as.factor(ifelse(HS_data$hypertension=="0","No","Yes"))
HS_data$heart_disease <-as.factor(ifelse(HS_data$heart_disease=="0","No","Yes"))
HS_data$bmi <- as.double(HS_data$bmi)  

#### missing data identification ####
md.pattern(HS_data, rotate.names = TRUE)
# Drop NAs
HS_data_DropNA <- na.omit(HS_data)
md.pattern(HS_data_DropNA, rotate.names = TRUE)
HS_data <-HS_data_DropNA

# Drop Outliers
HS_data <- filter(HS_data,HS_data$id != 59993 & HS_data$id != 3205) #bmi <12
HS_data <- filter(HS_data,HS_data$gender != 'Other') #'other' gender
HS_data <- HS_data[ , !(names(HS_data) %in% 'id') ] #remove id

# Convert dummy variables
HS_data <- dummy_cols(HS_data, select_columns = 'work_type')
HS_data <- dummy_cols(HS_data, select_columns = "smoking_status")
HS_data <- subset(HS_data, select = -c(work_type, work_type_Never_worked, smoking_status, `smoking_status_never smoked`))

# Rename columns to remove spaces
HS_data <- rename(HS_data, smoking_status_formerly_smoke = `smoking_status_formerly smoked`)
HS_data <- rename(HS_data, work_type_Self_employed = `work_type_Self-employed`)


#################### Data Exploration ####################

## -------------  Correlation ------------- 
model.matrix(~0+., data=HS_data) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE)

## -------------  Histograms ------------- 
ggplot(data.frame(HS_data_plot), aes(x=gender)) + geom_bar() #gender
hist(HS_data_plot$age, main = "Histogram") #age
hist(HS_data_plot$avg_glucose_level, main = "Histogram") #avg glucose level
hist(HS_data_plot$bmi, main = "Histogram") #bmi
ggplot(data.frame(HS_data_plot), aes(x=ever_married)) + geom_bar() #marriage
ggplot(data.frame(HS_data_plot), aes(x=heart_disease)) + geom_bar() #heart disease
ggplot(data.frame(HS_data_plot), aes(x=hypertension)) + geom_bar() #hypertension
ggplot(data.frame(HS_data_plot), aes(x=Residence_type)) + geom_bar() #residence type
ggplot(data.frame(HS_data_plot), aes(x=smoking_status)) + geom_bar() #smoking status
ggplot(data.frame(HS_data_plot), aes(x=work_type)) + geom_bar() #work type


## -------------  Proportion graphs ------------- 

#Stroke Distribution
barplot(prop.table(table(HS_data$stroke)),
        col = c("blue", "orange"),
        ylim = c(0,1),
        main="Stroke Distribution")


#Gender Distribution#
ggplot(HS_data %>% group_by(stroke)%>%count(stroke, gender) %>%    # Group by stroke and gender, then count number in each group
         mutate(pct=n/sum(n)),                 # Calculate percent within stroke
       aes(stroke, n, fill=gender)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.3))

# age
hist(HS_data$age,
     main = "Age Distribution", xlim =c(0,100), col = "lightblue")

ggplot(HS_data,
       aes(x = age, fill = stroke)) +
  scale_y_continuous(labels = scales::percent) +
  geom_density(alpha = 0.3) +
  labs(title = 'Proportion of Age Breakdown by Stroke vs. No-Stroke',
       y = 'Proportion of Sub-Group',
       x = 'age')


#Hypertension#

ggplot(HS_data %>% group_by(stroke)%>%count(stroke, hypertension) %>%    # Group by stroke and hypertension, then count number in each group
         mutate(pct=n/sum(n)),              # Calculate percent within stroke
       aes(stroke, n, fill=hypertension)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.3))

#Heart Disease#

ggplot(HS_data %>% group_by(stroke)%>%count(stroke, heart_disease) %>%    # Group by stroke and heart_disease, then count number in each group
         mutate(pct=n/sum(n)),              # Calculate percent within stroke
       aes(stroke, n, fill=heart_disease)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.3))

#ever_married# 

ggplot(HS_data %>% group_by(stroke)%>%count(stroke, ever_married) %>%    # Group by stroke and ever_married, then count number in each group
         mutate(pct=n/sum(n)),              # Calculate percent within stroke
       aes(stroke, n, fill=ever_married)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.3))

#Residence_type#### No need to plot as low corrected 

ggplot(HS_data %>% group_by(stroke)%>%count(stroke, Residence_type) %>%    # Group by stroke and Residence_type, then count number in each group
         mutate(pct=n/sum(n)),              # Calculate percent within stroke
       aes(stroke, n, fill=Residence_type)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.3))

#bmi
ggplot(HS_data,
       aes(x = bmi, fill = stroke)) +
  scale_y_continuous(labels = scales::percent) +
  geom_density(alpha = 0.3) +
  labs(title = 'Proportion of bmi Breakdown by Stroke  vs. No-Stroke',
       y = 'Proportion of Sub-Group',
       x = 'bmi')


#avg_glucose_level
ggplot(HS_data,
       aes(x = avg_glucose_level, fill = stroke)) +
  scale_y_continuous(labels = scales::percent) +
  geom_density(alpha = 0.3) +
  labs(title = 'Proportion of avg_glucose_level Breakdown by Stroke vs. No-Stroke',
       y = 'Proportion of Sub-Group',
       x = 'avg_glucose_level')




#################### Predictive Modelling #################### 

# Split train/test set with ratio 75:25
set.seed(123)
trainset = sample.split(HS_data, SplitRatio = .75)

Stroke_Train = subset(HS_data, trainset == TRUE)
Stroke_Test = subset(HS_data, trainset == FALSE)

##------------- BASE MODEL ------------- 
# base glm: AUC=0.856
reg_glm <- glm(stroke~ ., data=Stroke_Train, family=binomial)
pred_glm = predict(reg_glm, newdata = Stroke_Test, type = "response")
test_glm = roc(Stroke_Test$stroke ~ pred_glm, plot = TRUE, print.auc = TRUE)

confusion.matrix<-table(Stroke_Test$stroke, pred_glm >= 0.5)
confusion.matrix  

# imbalanced data detected
confusion.matrix[2,2]/(confusion.matrix[2,2]+confusion.matrix[2,1]) # Sensitivity
confusion.matrix[1,1]/(confusion.matrix[1,1]+confusion.matrix[1,2]) # Specificity

summary(reg_glm)

##------------- Feature Engineering ------------- 

# Balance the unbalanced data using "both" methods (over-sampling + under sampling)
both <- ovun.sample(stroke~., data=Stroke_Train, method="both", p=0.5, N=3860, seed=1)$data

# Find the significant coefficients using Lasso
x <- model.matrix(stroke ~ ., both)[,-1]
x1 <- model.matrix(stroke ~., Stroke_Test)[,-1] 
y <- both$stroke

# Fit regression on training set
cv.fit <- cv.glmnet(x, y, alpha = 1, nfolds = 10, family = "binomial") 

# Get the lambda for the lowest cross-validation error
optlambda <- cv.fit$lambda.min
optlambda

# Create the vector of lambdas and fit the lasso model again
w <- seq(10, -3, length=100)
lvalues <- 10 ^ w

fit <- glmnet(x, y, alpha = 1, lambda = lvalues, family = "binomial")

# Model with optimal lambda
predict(fit, s = optlambda, type = "coefficients")
pred_probs1 <- predict(fit, s = optlambda, type = "response", newx = x1) # Creating a vector of probabilities to have a stroke or not 

head(pred_probs1) # for the 1st observation in the test set the probability to have an stroke is 0.8326, for the second is 0.6752
estclass <- ifelse(pred_probs1 <0.5, 0,1) #we assume that the person will have an stroke if the probability is more than 0.5
head(estclass)
table(estclass, Stroke_Test$stroke)

pr1 <- prediction(pred_probs1, Stroke_Test$stroke) ### create the performance object to analyze the AUC
perf1 <- performance(pr1, x.measure = "fpr", measure = "tpr")
plot(perf1)   ###AUC plot
auc1 <- performance(pr1, measure = "auc")  ### compute the area under curve (auc)
auc1@y.values # AUC = 0.848


##------------- Re-Modeling with Improvement -------------
# base glm: AUC=0.846, 
reg_glm_opt <- glm(stroke~ .
                   -gender
                   -ever_married
                   -Residence_type
                   -work_type_children
                   -smoking_status_formerly_smoke
                   , data=both, family=binomial)
pred_glm_opt = predict(reg_glm_opt, newdata = Stroke_Test, type = "response")
test_glm_opt = roc(Stroke_Test$stroke ~ pred_glm_opt, plot = TRUE, print.auc = TRUE)

summary(reg_glm_lasso)

# Confusion Matrix with Threshold = 0.5
confusion.matrix_lasso <- table(Stroke_Test$stroke, pred_glm_lasso >= 0.5)
confusion.matrix_lasso

TP <- confusion.matrix_lasso[2,2]
FN <- confusion.matrix_lasso[2,1]
FP <- confusion.matrix_lasso[1,2]
TN <- confusion.matrix_lasso[1,1]

P <- TP + FN
N <- FP + TN
TPR <- TP/P
FPR <- FP/N

sens <- TP/(TP+FN);sens # sensitivity = 0.75
spec <- TN/(TN+FP);spec # specificity = 0.746

PT <- (sqrt(TPR*FPR)-FPR)/(TPR-FPR);PT # optimal threshold = 0.3678

# Confusion Matrix with optimal threshold = 0.3678
confusion.matrix_lasso_opt <- table(Stroke_Test$stroke, pred_glm_lasso >= 0.3678)
confusion.matrix_lasso_opt

TP <- confusion.matrix_lasso_opt[2,2]
FN <- confusion.matrix_lasso_opt[2,1]
FP <- confusion.matrix_lasso_opt[1,2]
TN <- confusion.matrix_lasso_opt[1,1]

sens_opt <- TP/(TP+FN);sens_opt # sensitivity = 0.942
spec_opt <- TN/(TN+FP);spec_opt # specificity = 0.643


##------------- Try other modelling methods -------------

# Decision tree, AUC=0.795
tree.both <- rpart(stroke~ .
                   -gender
                   -ever_married
                   -Residence_type
                   -work_type_Private
                   -work_type_Self_employed
                   -work_type_Children, data = both)
pred.tree.both <- predict(tree.both, newdata = Stroke_Test)
roc.curve(Stroke_Test$stroke, pred.tree.both[,2])

# Random forest, AUC=0.672
rf.both <- randomForest(stroke~ .
                        -gender
                        -ever_married
                        -Residence_type
                        -work_type_Private
                        -work_type_Self_employed
                        -work_type_Children, data = both)
pred.rf.both <- predict(rf.both, newdata = Stroke_Test)
roc.curve(Stroke_Test$stroke, pred.rf.both)

# Over-sampling, AUC=0.851
over <- ovun.sample(stroke~., data=Stroke_Train, method="over", N=6832, seed=1)$data
reg_glm_over <- glm(stroke~ ., data=over, family=binomial)
pred_glm_over = predict(reg_glm_over, newdata = Stroke_Test, type = "response")
test_glm_over = roc(Stroke_Test$stroke ~ pred_glm_over, plot = TRUE, print.auc = TRUE)

# Down-sampling, AUC=0.837
under <- ovun.sample(stroke~., data=Stroke_Train, method="under", N=304, seed=1)$data
reg_glm_under <- glm(stroke~ ., data=under, family=binomial)
pred_glm_under = predict(reg_glm_under, newdata = Stroke_Test, type = "response")
test_glm_under = roc(Stroke_Test$stroke ~ pred_glm_under, plot = TRUE, print.auc = TRUE)






