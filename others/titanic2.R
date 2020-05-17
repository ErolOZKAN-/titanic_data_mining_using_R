library(Hmisc)
library(knitr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(gridExtra)
library(ROCR)
library(corrplot)

train <- read.csv("train.csv", stringsAsFactors = F, na.strings = c("NA", ""))
test <- read.csv("test.csv", stringsAsFactors = F, na.strings = c("NA", ""))
str(train) # describe the dataset

test$Survived <- NA # add “Survived” column to test dataset.
all <- rbind(train, test) # merge train and test datasets.

sapply(all, function(x) {sum(is.na(x))}) # check which columns contains missing values.

all$Sex <- as.factor(all$Sex)
all$Survived <- as.factor(all$Survived)
all$Pclass <- as.ordered(all$Pclass) #because Pclass is ordinal

ggplot(all[!is.na(all$Survived),], aes(x = Survived, fill = Survived)) +
  geom_bar(stat='count') +
  labs(x = 'How many people died and survived on the Titanic?') +
  geom_label(stat='count',aes(label=..count..), size=7) +
  theme_grey(base_size = 18)

p1 <- ggplot(all, aes(x = Sex, fill = Sex)) +
  geom_bar(stat='count', position='dodge') + theme_grey() +
  labs(x = 'All data') +
  geom_label(stat='count', aes(label=..count..)) +
  scale_fill_manual("legend", values = c("female" = "pink", "male" = "green"))

p2 <- ggplot(all[!is.na(all$Survived),], aes(x = Sex, fill = Survived)) +
  geom_bar(stat='count', position='dodge') + theme_grey() +
  labs(x = 'Training data only') +
  geom_label(stat='count', aes(label=..count..))

grid.arrange(p1,p2, nrow=1)

###############################################################################
p3 <- ggplot(all, aes(x = Pclass, fill = Pclass)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Pclass, All data') + geom_label(stat='count', aes(label=..count..)) +
  theme(legend.position="none") + theme_grey()     

p4 <- ggplot(all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='dodge') + labs(x = 'Training data only') +
  theme(legend.position="none") + theme_grey()

p5 <- ggplot(all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Training data only', y= "Count") + facet_grid(.~Sex) +
  theme(legend.position="none") + theme_grey()

p6 <- ggplot(all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~Sex) +
  theme(legend.position="none") + theme_grey()

grid.arrange(p3, p4, p5, p6, ncol=2)

##############################################################################

all$PclassSex[all$Pclass=='1' & all$Sex=='male'] <- 'P1Male'
all$PclassSex[all$Pclass=='2' & all$Sex=='male'] <- 'P2Male'
all$PclassSex[all$Pclass=='3' & all$Sex=='male'] <- 'P3Male'
all$PclassSex[all$Pclass=='1' & all$Sex=='female'] <- 'P1Female'
all$PclassSex[all$Pclass=='2' & all$Sex=='female'] <- 'P2Female'
all$PclassSex[all$Pclass=='3' & all$Sex=='female'] <- 'P3Female'
all$PclassSex <- as.factor(all$PclassSex)

#########################################################
###########Feature Engineering###########################
#########################################################
all$Surname <- sapply(all$Name, function(x) {strsplit(x, split='[,.]')[[1]][1]})
#correcting some surnames that also include a maiden name
all$Surname <- sapply(all$Surname, function(x) {strsplit(x, split='[-]')[[1]][1]})
all$Title <- sapply(all$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})
all$Title <- sub(' ', '', all$Title) #removing spaces before title
kable(table(all$Sex, all$Title))
########################################################
all$Title[all$Title %in% c("Mlle", "Ms")] <- "Miss"
all$Title[all$Title== "Mme"] <- "Mrs"
all$Title[!(all$Title %in% c('Master', 'Miss', 'Mr', 'Mrs'))] <- "Rare Title"
all$Title <- as.factor(all$Title)
kable(table(all$Sex, all$Title))

ggplot(all[!is.na(all$Survived),], aes(x = Title, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Title') +theme_grey()
########################################################
all$Fsize <- all$SibSp+all$Parch +1 #creating family size variable (Fsize)
ggplot(all[!is.na(all$Survived),], aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') + theme_grey()
########################################################
all$FsizeName <- paste(as.character(all$Fsize), all$Surname, sep="") #  combine Fsize and Surname
SizeCheck <- all %>%
  group_by(FsizeName, Fsize) %>%
  summarise(NumObs=n())
SizeCheck$NumFam <- SizeCheck$NumObs/SizeCheck$Fsize
SizeCheck$modulo <- SizeCheck$NumObs %% SizeCheck$Fsize
SizeCheck <- SizeCheck[SizeCheck$modulo !=0,]
sum(SizeCheck$NumObs) #total number of Observations with inconsistencies
kable(SizeCheck[SizeCheck$FsizeName %in% c('3Davies', '5Hocking', '6Richards', '2Wilkes', '3Richards', '4Hocking'),]) #only display some inconsistencies that are discussed in the text
########################################################
kable(all[all$FsizeName=='3Davies',c(2,3,14,5,6,7,8,17,9,15)])
all$FsizeName[c(550, 1222)] <- '2Davies'
all$SibSp[550] <- 0
all$Parch[1222] <- 1
all$Fsize[c(550, 1222)] <- 2
kable(all[all$FsizeName=='2Davies',c(2,3,14,5,6,7,8,17,9,15)])
########################################################
NC <- all[all$FsizeName %in% SizeCheck$FsizeName,] #create data frame with only relevant Fsizenames

#extracting maiden names
NC$Name <- sub("\\s$", "", NC$Name) #removing spaces at end Name
NC$Maiden <- sub(".*[^\\)]$", "", NC$Name) #remove when not ending with ')'
NC$Maiden <- sub(".*\\s(.*)\\)$", "\\1", NC$Maiden)
NC$Maiden[NC$Title!='Mrs'] <- "" #cleaning up other stuff between brackets (including Nickname of a Mr)
NC$Maiden <- sub("^\\(", '', NC$Maiden) #removing opening brackets (sometimes single name, no spaces between brackets)
#making an exceptions match
NC$Maiden[NC$Name=='Andersen-Jensen, Miss. Carla Christine Nielsine'] <- 'Jensen'

#take only Maiden names that also exist as surname in other Observations
NC$Maiden2[NC$Maiden %in% NC$Surname] <- NC$Maiden[NC$Maiden %in% NC$Surname] 
#create surname+maiden name combinations
NC$Combi[!is.na(NC$Maiden2)] <- paste(NC$Surname[!is.na(NC$Maiden2)], NC$Maiden[!is.na(NC$Maiden2)])

#create labels dataframe with surname and maiden merged into one column
labels1 <- NC[!is.na(NC$Combi), c('Surname','Combi')]
labels2 <- NC[!is.na(NC$Combi), c('Maiden','Combi')]
colnames(labels2) <- c('Surname', 'Combi')
labels1 <- rbind(labels1, labels2)

NC$Combi <- NULL
NC <- left_join(NC, labels1, by='Surname')

#Find the maximum Fsize within each newly found 'second degree' family
CombiMaxF <- NC[!is.na(NC$Combi),] %>%
  group_by(Combi) %>%
  summarise(MaxF=max(Fsize)) #summarise(MaxF=n())
NC <- left_join(NC, CombiMaxF, by = "Combi")

#create family names for those larger families
NC$FsizeCombi[!is.na(NC$Combi)] <- paste(as.character(NC$Fsize[!is.na(NC$Combi)]), NC$Combi[!is.na(NC$Combi)], sep="")

#find the ones in which not all Fsizes are the same
FamMaid <- NC[!is.na(NC$FsizeCombi),] %>%
  group_by(FsizeCombi, MaxF, Fsize) %>%
  summarise(NumObs=n())
FamMaidWrong <- FamMaid[FamMaid$MaxF!=FamMaid$NumObs,]

kable(unique(NC[!is.na(NC$Combi) & NC$FsizeCombi %in% FamMaidWrong$FsizeCombi, c('Combi', 'MaxF')]))
#####################
NC$MaxF <- NULL #erasing MaxF column maiden combi's

#Find the maximum Fsize within remaining families (no maiden combi's)
FamMale <- NC[is.na(NC$Combi),] %>%
  group_by(Surname) %>%
  summarise(MaxF=max(Fsize))
NC <- left_join(NC, FamMale, by = "Surname")

NCMale <- NC[is.na(NC$Combi),] %>%
  group_by(Surname, FsizeName, MaxF) %>%
  summarise(count=n()) %>%
  group_by(Surname, MaxF) %>%
  filter(n()>1) %>%
  summarise(NumFsizes=n())

NC$Combi[NC$Surname %in% NCMale$Surname] <- NC$Surname[NC$Surname %in% NCMale$Surname]

kable(NCMale[, c(1,2)])
kable(all[all$Surname=='Vander Planke', c(2,3,4,5,6,7,8,9,15)])
#selecting those 37 passengers In Not Correct dataframe
NC <- NC[(NC$FsizeCombi %in% FamMaidWrong$FsizeCombi)|(NC$Surname %in% NCMale$Surname),]

#calculating the average Fsize for those 9 families
NC1 <- NC %>%
  group_by(Combi) %>%
  summarise(Favg=mean(Fsize))
kable(NC1)

NC <- left_join(NC, NC1, by = "Combi") #adding Favg to NC dataframe 
NC$Favg <- round(NC$Favg) #rounding those averages to integers
NC <- NC[, c('PassengerId', 'Favg')]
all <- left_join(all, NC, by='PassengerId')

#replacing Fsize by Favg
all$Fsize[!is.na(all$Favg)] <- all$Favg[!is.na(all$Favg)]

#####################################################
#taking the max of family and ticket size as the group size
all$Group <- all$Fsize
for (i in 1:nrow(all)){
  all$Group[i] <- max(all$Group[i], all$Tsize[i])
}

#Creating final group categories
all$GroupSize[all$Group==1] <- 'solo'
all$GroupSize[all$Group==2] <- 'duo'
all$GroupSize[all$Group>=3 & all$Group<=4] <- 'group'
all$GroupSize[all$Group>=5] <- 'large group'
all$GroupSize <- as.factor(all$GroupSize)
g1 <- ggplot(all[!is.na(all$Survived),], aes(x = Group, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Final Group Sizes') + theme_grey()

g2 <- ggplot(all[!is.na(all$Survived),], aes(x = GroupSize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Final Group Categories') + theme_grey() +
  scale_x_discrete (limits = c('solo', 'duo', 'group', 'large group'))
grid.arrange(g2, g1)

##########################################################################
all$FarePP <- all$Fare/all$Tsize #creating the Fare Per Person variable

tab2 <- all[(!is.na(all$Embarked) & !is.na(all$Fare)),] %>%
  group_by(Embarked, Pclass) %>%
  summarise(FarePP=median(FarePP))
kable(tab2)


#imputing missing Embarked values
all$Embarked[all$Ticket=='113572'] <- 'C'
#converting Embarked into a factor
all$Embarked <- as.factor(all$Embarked)
I can actually use the same table to find a sensible fare for Mr Story. As you can see below, he traveled 3rd class and embarked at Southampton.

#display passengers with missing Fare
kable(all[which(is.na(all$Fare)), c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group')])

#imputing FarePP (as the Fare will be dropped later on anyway)
all$FarePP[1044] <- 7.8

