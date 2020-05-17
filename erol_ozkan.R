##### 3	EXPLORATORY DATA ANALYSIS #####
### 3.1	Load Libraries ###
library('ggplot2') 
library('ggthemes') 
library('scales') 
library('mice') 
library('Hmisc')
library('knitr')
library('dplyr')
library('caret')
library('randomForest')
library('gridExtra')
library('ROCR')
library('corrplot')

### 3.2	Read Data ###
train <- read.csv('dataset/train.csv', stringsAsFactors = F, na.strings = c("NA", "")) # Read train dataset.
test  <- read.csv('dataset/test.csv', stringsAsFactors = F, na.strings = c("NA", "")) # Read test dataset.
full  <- bind_rows(train, test) # Concatinate train and test dataset

### 3.3	Check Data ###
str(full)

### 3.5	Investigate the Data ###
sapply(full, function(x) {sum(is.na(x))}) 

### 3.6	Explore the Data ###
full$Sex <- as.factor(full$Sex)
full$Survived <- as.factor(full$Survived)
full$Pclass <- as.factor(full$Pclass) 
full$Embarked <- as.factor(full$Embarked)

# 3.6.1	Survived #
ggplot(full[!is.na(full$Survived),], aes(x = Survived, fill = Survived)) +
  geom_bar(stat='count') +
  labs(x = 'How many people died and survived on the Titanic?') +
  geom_label(stat='count',aes(label=..count..), size=7) 

# 3.6.2	Sex #
p1 <- ggplot(full, aes(x = Sex, fill = Sex)) +
  geom_bar(stat='count', position='dodge') + theme_grey() +
  labs(x = 'All data') +
  geom_label(stat='count', aes(label=..count..)) +
  scale_fill_manual("legend", values = c("female" = "pink", "male" = "green"))

p2 <- ggplot(full[!is.na(full$Survived),], aes(x = Sex, fill = Survived)) +
  geom_bar(stat='count', position='dodge') + theme_grey() +
  labs(x = 'Training data only') +
  geom_label(stat='count', aes(label=..count..))

grid.arrange(p1,p2, nrow=1)

# 3.6.3	PClass #
p3 <- ggplot(full, aes(x = Pclass, fill = Pclass)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Pclass, All data') + geom_label(stat='count', aes(label=..count..)) +
  theme(legend.position="none") + theme_grey()     
p4 <- ggplot(full[!is.na(full$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='dodge') + labs(x = 'Training data only') +
  theme(legend.position="none") + theme_grey()
p5 <- ggplot(full[!is.na(full$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Training data only', y= "Count") + facet_grid(.~Sex) +
  theme(legend.position="none") + theme_grey()
p6 <- ggplot(full[!is.na(full$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~Sex) +
  theme(legend.position="none") + theme_grey()
grid.arrange(p3, p4, p5, p6, ncol=2)

##### 4	FEATURE ENGINEERING 1 #####
### 4.1	Title Variable ###
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name) # Create Title variable
table(full$Sex, full$Title) # Show Title counts

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',  'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
table(full$Sex, full$Title)

ggplot(full[!is.na(full$Survived),], aes(x = Title, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Title') +theme_grey()

### 4.2	Family Size ###
full$Fsize <- full$SibSp + full$Parch + 1

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')

full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size - Survival', shade=TRUE)

### 4.3	Cabin ###
full$Cabin
strsplit(full$Cabin[2], NULL)[[1]] 
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1])) 
full$Deck

##### 5	MISSING VALUE IMPUTATION #####
### 5.1	Embarked ###
full[c(62, 830), 'Embarked']

embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format())

full$Embarked[c(62, 830)] <- 'C'
full[c(62, 830), 'Embarked']

### 5.2	Fare ###
full[1044, ]

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format())


full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
full[1044, ]

### 5.3	Age ###
sum(is.na(full$Age))

set.seed(129) # Set a random seed
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod) # Save the output 

par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Original Data', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='MICE Output Data', ylim=c(0,0.04))

full$Age <- mice_output$Age
sum(is.na(full$Age))

##### 6	FEATURE ENGINEERING 2 #####
### 6.1	Child ###
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  facet_grid(.~Sex)

full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
table(full$Child, full$Survived)
full$Child  <- factor(full$Child)

### 6.2	Mother ###
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
table(full$Mother, full$Survived)
full$Mother <- factor(full$Mother)

##### 7	PREDICTION #####
sapply(full, function(x) {sum(is.na(x))})
full$Sex <- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)
full$Title<- as.factor(full$Title) 
full$FsizeD<- as.factor(full$FsizeD)

### 7.1	Split Dataset ###
train <- full[1:891,]
test <- full[892:1309,]

### 7.2	Random Forrest Model ###
set.seed(2017)
random_forrest_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                       Fare + Embarked + Title + 
                                       FsizeD + Child + Mother, data = train)

random_forrest_predictions <- predict(random_forrest_model, test)
random_forrest_solutions <- data.frame(PassengerID = test$PassengerId, Survived = random_forrest_predictions)
write.csv(random_forrest_solutions, file = 'output/random_forrest_solutions.csv', row.names = F)

### 7.2.1	Visualize the Model ###
plot(random_forrest_model, ylim=c(0,0.36))
legend('topright', colnames(random_forrest_model$err.rate), col=1:3, fill=1:3)

random_forrest_importance <- varImp(random_forrest_model, scale = FALSE)
importance_scores <- data.frame(Variables = row.names(random_forrest_importance), MeanDecreaseGini = random_forrest_importance$Overall)

ggplot(importance_scores, aes(x=reorder(Variables, MeanDecreaseGini), y=MeanDecreaseGini, fill=MeanDecreaseGini)) +
  geom_bar(stat='identity') + coord_flip() + theme(legend.position="none") + labs(x="") +
  ggtitle('Variable Importance Random Forest') + theme(plot.title = element_text(hjust = 0.5))

### 7.3	SVM Model ###
set.seed(2017)
svm_model <- train(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + 
                     FsizeD + Child + Mother, data=train, method='svmRadial', preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number=5))
svm_model
svm_model$results

svm_predictions <- predict(svm_model, test)
svm_solutions <- data.frame(PassengerID = test$PassengerId, Survived = svm_predictions)
write.csv(svm_solutions, file = 'output/svm_solutions.csv', row.names = F)

### 7.4	GBM Model ###
set.seed(2017)
gbm_model <- train(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + 
                       FsizeD + Child + Mother, data=train, method='gbm', preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number=7), verbose=FALSE)
print(gbm_model)

gbm_predictions <- predict(gbm_model, test)
gbm_solutions <- data.frame(PassengerID = test$PassengerId, Survived = gbm_predictions)
write.csv(gbm_solutions, file = 'output/gbm_solutions.csv', row.names = F)

### 8 Results ###
random_forrest_model_result <- 0.78947
svm_model_result <- 0.80382
gbm_model_result <- 0.76555

# Compare Results
barplot(c(random_forrest_model_result, svm_model_result, gbm_model_result), main="Results", names.arg=c("Random Forrest","SVM","GBM"),col = rainbow(3), xlab="Model")

