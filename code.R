library(readxl)
library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(factoextra)
library(FactoMineR)


socioecon <- read_excel("electionsProject.xlsx", sheet="county_facts")
elections <- read_excel("electionsProject.xlsx", sheet="votes")


# ------------------------
# PART 1 - CLASSIFICATION
# ------------------------


# compute total votes and find percentage for Trump
# merge the two datasets to be able to perform classification
# create binary variable for whether Trump got more than 50% of the votes

totalvotestrumpbyfips = cbind(elections$votes[elections$candidate=="Donald Trump"],
                              elections$fips[elections$candidate=="Donald Trump"])
fractions = cbind(elections$fraction_votes[elections$candidate=="Donald Trump"],
                  elections$fips[elections$candidate=="Donald Trump"])
totalvotesfips = as.data.frame(tapply(elections$votes,elections$fips,sum))
totalvotesfips = data.frame(rownames(totalvotesfips),totalvotesfips)
totalvotesfips$rownames.totalvotesfips. = as.numeric(totalvotesfips$rownames.totalvotesfips.)
colnames(totalvotestrumpbyfips) = c("Donald Trump","fips")
totalvotestrumpbyfips = as.data.frame(totalvotestrumpbyfips)
colnames(fractions) = c("fractions","fips")
fractions = as.data.frame(fractions)
rownames(totalvotesfips) = NULL
colnames(totalvotesfips) = c("fips","Total Votes")
head(totalvotestrumpbyfips)
head(totalvotesfips)
head(fractions)

totalvotesfips = left_join(totalvotesfips,totalvotestrumpbyfips,by = "fips") #merge on common fips

totalvotesfractions = left_join(totalvotesfips,fractions,by = "fips")

totalvotesfractions$`Donald Trump Percentage` = (totalvotesfractions$`Donald Trump`/totalvotesfractions$`Total Votes`)+
  totalvotesfractions$fractions
totalvotesfractions$`Donald Trump Percentage` = round(totalvotesfractions$`Donald Trump Percentage`,2)
totalvotesfractions$lead = 0
totalvotesfractions$lead[totalvotesfractions$`Donald Trump Percentage`>=0.50] = 1

final = left_join(socioecon,totalvotesfractions, by = "fips")

final = as.data.frame(final)
head(final)

# remove all NAs
final = na.omit(final)

# make the variable binary
final$lead = as.factor(final$lead)

#change any column in dataset based on % of the population to true percentage 
final[,c(6,8:24, 28:29, 35, 38, 41:46)] = final[,c(6,8:24, 28:29, 35, 38, 41:46)]/100

# remove a few unneccesary columns
final = final[,-c(1:3,5:8,55:58)]





# split into train and test sets
set.seed(20)
trainingIndex <- createDataPartition(final$lead, p=0.8, list = FALSE)
train <- final[trainingIndex,-c(1:3)]
test <- final[-trainingIndex,-c(1:3)]
testWinner = final$lead[-trainingIndex]
dim(train)
dim(test)



# ----------
# KNN 

set.seed(123)
modelKNN <- train(
  lead ~., data = train, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 25
)
# Plot model accuracy vs different values of k
plot(modelKNN)

modelKNN$bestTune  # k=5

predKNN <- modelKNN %>% predict(test)
head(predKNN)

table(predKNN,testWinner)
mean(predKNN == testWinner)   # accuracy 0.855

442/(442+57) # precision rate 0.885

confusionMatrix(predKNN,testWinner)



# ---------------
# DECISION TREE

tree = rpart(lead ~ ., data=final, parms=list(split="information"))
print(tree)
rpart.plot(tree, type=1, box.palette="Blues", cex=0.6)

# check predictions
tree = rpart(lead ~ . , data=train, parms=list(split="information"))
summary(tree)
treepred = predict(tree, test, type="class")
table(treepred, testWinner)
mean(treepred==testWinner) #0.844 


# comparison of final results
models = c("KNN", "DecisionTree", "KNN", "DecisionTree")
measure = c("Accuracy", "Accuracy", "Precision", "Precision") 
percentage = c(85.5, 84.4, 88.5, 86.3)
finalResults = data.frame(models,measure,percentage)
ggplot(finalResults, aes(x = models, y = percentage, fill = measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = .6), alpha = .8) +
  ggtitle("Model Comparison") +
  scale_y_continuous(limits = c(0, 100)) + 
  geom_hline(yintercept = 87.8, linetype = "dashed", size = 1, color = "#E54445") + 
  geom_hline(yintercept = 88.7, linetype = "dashed", size = 1, color = "#5B94C2") +
  scale_fill_brewer(palette = "Set1")



# ---------------------
# PART 2 - CLUSTERING
# ---------------------


demographics = as.data.frame(socioecon[,c(1:25)])
economics = as.data.frame(socioecon[,-c(4:25)])


# remove NAs
demographics = demographics[complete.cases(demographics),]
economics = economics[complete.cases(economics),]

# change rownames of demographics to get the fips code
rownames(demographics) = demographics[,1]
demographics[,1] = NULL

rownames(economics) = economics[,1]
economics[,1] = NULL

# remove county and state names
demographics = demographics[,-c(1:2)]

economics = economics[,-c(1:2)]



# scale data
demoScale = data.frame(scale(demographics))
apply(demoScale,2,sd)
plot(apply(demoScale,2,sd))




# optimal number for k
wssplot = function(data, nc=15, seed=1234){
  wss = (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(demoScale, nc=10) 


# second way to visualize it
fviz_nbclust(demoScale, kmeans, method = "wss") +
  geom_vline(xintercept = 9, linetype = 2)

# perform k-means
km = kmeans(demoScale, 9, nstart=25)

head(cbind(demographics, cluster=km$cluster))
fviz_cluster(km, data = demoScale,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#023047", "#3c096c", "#fb5607", "#8c2f39", "#adc178"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



# save clusters in economics data set to do classification
economics$cluster = as.factor(km$cluster)



# split into train and test sets
set.seed(43)
trainingIndex2 <- createDataPartition(economics$cluster, p=0.8, list = FALSE)
train2 <- economics[trainingIndex2,]
test2 <- economics[-trainingIndex2,]
testCluster = economics$cluster[-trainingIndex2]
dim(train2)
dim(test2)


# perform KNN classification
set.seed(12)
econKNN <- train(
  cluster ~., data = train2, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 25
)
# Plot model accuracy vs different values of k
plot(econKNN)

econKNN$bestTune  # k=9

pred <- econKNN %>% predict(test2)
head(pred)

table(pred,testCluster)
mean(pred == testCluster) 

confusionMatrix(pred,testCluster)


