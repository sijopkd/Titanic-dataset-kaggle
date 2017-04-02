library(caret)
library(dplyr)
library(mRMRe)
library(Metrics)
library(randomForest)
library(DT)
library('ggthemes')
library('scales')
library(RColorBrewer)
library('mice') #imputation

#Read the data
train <- read.csv("D:/Sijo_data/Datasets/Titanic/train.csv", header = T, stringsAsFactors = F)
test <- read.csv("D:/Sijo_data/Datasets/Titanic/test.csv", header = T, stringsAsFactors = F)

dataset <- bind_rows(train, test)

#Check for NA
colSums(is.na(dataset))



#Classify based on the type
"""
classes <- sapply(names(dataset),function(x){class(train[[x]])})
dataset$PassengerId <- as.factor(dataset$PassengerId)
dataset$Survived <- as.factor(dataset$Survived)
dataset$Pclass <- as.factor(dataset$Pclass)
dataset$Sex <- as.factor(dataset$Sex)
dataset$SibSp <- as.integer(dataset$SibSp)
dataset$Parch <- as.integer(dataset$Parch)"""

#Feature engineering
dataset$Title <- gsub('(.*, )|(\\..*)', '', dataset$Name)

# Show title counts by sex
table(dataset$Sex, dataset$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
dataset$Title[dataset$Title == 'Mlle']        <- 'Miss' 
dataset$Title[dataset$Title == 'Ms']          <- 'Miss'
dataset$Title[dataset$Title == 'Mme']         <- 'Mrs' 
dataset$Title[dataset$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(dataset$Sex, dataset$Title)


# Finally, grab surname from passenger name
dataset$Surname <- gsub("^(.*?),.*", "\\1", dataset$Name)

#familysize

dataset$Familysize <- dataset$SibSp + dataset$Parch + 1 

ggplot(dataset[1:891,], aes(x = Familysize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
   theme_bw()

#family type
dataset$Familysizegroup[dataset$Familysize == 1] <- 'single'
dataset$Familysizegroup[dataset$Familysize > 1 &
                          dataset$Familysize < 5] <- 'small'
dataset$Familysizegroup[dataset$Familysize > 4] <- 'large'

#Survivde by family size 
ggplot(dataset[1:891,], aes(x = as.factor(Familysizegroup), fill = factor(Survived))) + 
  geom_bar(stat =  'count', position = 'dodge') +
  labs(x = "Family Size", y = "Number", fill = "Survived") +
  theme_wsj() + scale_colour_wsj("colors6", "")

#do it after treating for embarkment 
#emabarked
ggplot(dataset[1:891,], aes(x = as.factor(Embarked), fill = factor(Survived))) + 
  geom_bar(stat =  'count', position = 'dodge') +
  labs(x = "Family Size", y = "Number", fill = "Survived") +
  theme_wsj() + scale_colour_wsj("colors6", "")

#Treat passenger cabin variable
str(dataset$Cabin)
dataset$Deck <- substr(dataset$Cabin, 1,1)

dataset$Embarked <- as.factor(dataset$Embarked)
levels(dataset$Embarked)
dataset$PassengerId[dataset$Embarked == ""]
#passengers with id 62 and 830 have no embarked place. So let us
#look at their fare and Passenger class
dataset$Pclass[dataset$Embarked == ""]
dataset$Fare[dataset$Embarked == ""]
#both have same Pclass and fare 
dataset$Pclass <- as.factor(dataset$Pclass)
classwise<-as.data.frame(aggregate(Fare ~ Pclass, data = dataset, FUN = mean))
embarkedwise<-as.data.frame(aggregate(Fare ~ Embarked, data = dataset, FUN =mean))

embark_fare <- dataset %>%
  filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) + theme_classic()

#the fare of the missing passengerID values, matches with Pclass 
#of 1 and Embarked from C, which is Charbourg  

dataset$Embarked[c(62, 830)] <- 'C'
dataset$Embarked <-factor(dataset$Embarked)
levels(dataset$Embarked)

#more exploration 
colSums(is.na(dataset))
#let's look at the one missing value in Fare
dataset$Fare <-fac
dataset$PassengerId[is.na(dataset$Fare)]
#Passenger 1044 has missing value in fare
cat(paste("Passenger with ID" ,dataset$PassengerId[is.na(dataset$Fare)], 
          "has Fare column as NA. Let us look at the 
          Embarked station and Pclass"))

dataset$Pclass[is.na(dataset$Fare)]
dataset$Embarked[is.na(dataset$Fare)]
mean(dataset$Fare[dataset$Pclass == "3" & dataset$Embarked == "S"])

agg_mean<- aggregate(Fare~ Pclass + Embarked , dataset, FUN = mean )
agg_median <- aggregate(Fare~ Pclass + Embarked , dataset, FUN = median )
#since we have not removed the outliers, I am replacing the
#empty fare coulmn with the median of S and 3 combination 
dataset$Fare[is.na(dataset$Fare)] <- 8.05

colSums(is.na(dataset))
#We need to take care of the Age variable now
#Since we have a lot of NA values, 
#let us look at the distribution 

ggplot(data = dataset, aes(x = Age))  + 
geom_histogram(aes(y = ..density..) , col = "red" ,fill = "green")+ geom_density() +
  scale_fill_excel()  + theme_igray()

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Familysizegroup', 'Deck')

dataset[factor_vars] <- lapply(dataset[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)


# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(dataset[, !names(dataset) %in% 
                        c('PassengerId','Name','Ticket','Cabin','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)
# Plot age distributions
par(mfrow=c(1,2))
p1<- ggplot(data = dataset, aes(x = Age))  + 
  geom_histogram(aes(y = ..density..) , col = "red" ,fill = "green")+ geom_density() +
  scale_fill_excel()  + theme_igray() + 
  ggtitle("Original Age distribution")

p2<-ggplot(data = mice_output, aes(x = Age))  + 
  geom_histogram(aes(y = ..density..) , col = "red" ,fill = "green")+ geom_density() +
  scale_fill_excel()  + theme_igray()  +
  ggtitle("Imputed Age distribution")


source("http://peterhaschke.com/Code/multiplot.R")

multiplot(p1,p2, cols= 2)

#the distribution has not changed. So let's add the ouput of 
#mice imputation to the dataset

dataset$Age <- mice_output$Age
colSums(is.na(dataset))
#there seems that there are empty values in Deck
#deck has 1048 empty values. So I think that variable can be
#rejected. 
dataset$Deck <- NULL

summary(dataset)
str(dataset)
#let's begin our modelling

#split the datasets again.
train <- dataset[1:891,]
test <- dataset[892:nrow(dataset), ]
summary(train$Survived)




# Set a random seed
set.seed(677)

# Build the model (note: not all possible variables are used)
RF_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + Familysize + 
                           Fare*Age ,
                         data = train)


# Show model error
plot(RF_model, ylim=c(0,0.36))
legend('topright', colnames(RF_model$err.rate), col=1:3, fill=1:3)


# Get importance
importance    <- importance(RF_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(RF_model, test)


# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

#Try new
target <- "Survived"
out <- c("PassengerId", "Familysizegroup", "Name", "Ticket", "Cabin") 

charVars <- which(classes == "character")
integVars <- which(classes == "integer")


model_RF_New <- randomForest(factor(Survived) ~ Familysize + 
                               Pclass + Sex +Age+Fare + Title , data = train)
prediction <- predict(model_RF_New, test)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_new_mod_Solution.csv', row.names = F)

