data(iris)

dataset <- iris

View(dataset)


write.csv(dataset, "iris_09-07-2018.csv")

dataset <- na.omit(dataset)


library("caret")



#Cretaing training and validation dataset
#We will split the loaded dataset into two, 80% of which 
#we will use to train our models and 20% that we will hold back as a validation dataset.


# use the remaining 80% of data to training and testing the models


train_data <- createDataPartition(dataset$Species, p=0.80, list=FALSE)

# select 20% of the data for validation
test_data <- dataset[-train_data,]

#added 80% data into project_iris
dataset <- dataset[train_data,]

dim(dataset)

#types to attributes

# list types for each attribute

sapply(dataset, class)

#Peek at the Data

head(dataset)


#Levels of the Class
levels(dataset$Species)


#class distribution

percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$species), percentage=percentage)

#Now we can look at the interactions between the variables.

#We can also look at box and whisker plots of each input variable again,
#but this time broken down into separate plots for each class. 
#This can help to tease out obvious linear separations between the classes.


x <- dataset[,1:4]
y <- dataset[,5]



featurePlot(x=x, y=y, plot="box")


#Boxplot
boxplot(x, dataset,varwidth =  TRUE,notch = T, main=names(dataset))



# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


#Apply k-means clustering algorithm

#Process the data
iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new)

result<- kmeans(iris.new,3) #aplly k-means algorithm with no. of centroids(k)=3
result$size # gives no. of records in each cluster


# gives value of cluster center datapoint value(3 centers for k=3)
result$centers 



#gives cluster vector showing the custer where each record falls
result$cluster

#Verify results of clustering
par(mfrow=c(2,2), mar=c(5,4,2,2))

# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.new[c(1,2)], col=result$cluster)

# Plot to see how Sepal.Length and Sepal.Width 
#data points have been distributed originally as per "class" attribute in dataset
plot(iris.new[c(1,2)], col=iris.class)

# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters 
plot(iris.new[c(3,4)], col=result$cluster)

plot(iris.new[c(3,4)], col=iris.class)


#Build Models
#We don't know which algorithms would work on this dataset or what configurations 
#to use. We get an idea from the plots that some of the classes are partially 
#linearly separable in some dimensions, so we are expecting generally good results.

#We have evaluate 3 different algorithms:
#Linear Discriminant Analysis (LDA) Classification, Support Vector Machines (SVM) with a linear kernel and Random Forest (RF)

#This is a good mixture of simple linear (LDA) and 
#complex nonlinear methods (SVM, RF). 
#We reset the random number seed before reach run to ensure 
#that the evaluation of each algorithm is performed using exactly 
#the same data splits. It ensures the results are directly comparable.


# Linear discriminant Analysis(LDA)

# to find the colration between the variables and present scatter plot graph between them

library(psych)
pairs.panels(dataset[1:4],
             gap = 0,
             bg = c("red", "green", "blue")[dataset$species],
             pch =21)


library(MASS)

#split the data into two (traning_data , testing_data)
z <- sample(2, nrow(iris),
              replace = T,
              prob = c(0.8,0.2))
training_data <- iris[z==1,]
testing_data <- iris[z==2,]

linear <- lda(Species~.,training_data)
linear

#First discriminant function is a linear combination of the four variables:
#1.252207(Sepel.Length)
#1.115823(Sepel.Width)
#-2.626277(Petal.length)
#-2.156489(Petal.Width)


# Percentage seperations achived by the first discriminant function is 99.37%



#to check other outputs

attributes(linear)
linear$scaling
linear$xlevels
linear$counts


#histogram

p<- predict(linear, training_data)
p


#histogram1 is based on LD1
ldahist(data = p$x[,1],g = training_data$Species)

#histogram2 is based on LD2

ldahist(data = p$x[,2],g = training_data$Species)



#bi-plot
install.packages("devtools")

library(devtools)
install_github("fawda123/ggord")
library(ggord)

ggord(linear, training_data$Species,ylim=c(-10,10))


#partitioning plot

install.packages("CRAN")

install.packages("klaR")
library(klaR)

partimat(Species~.,data = training_data, method = "lda")

#Partition plot on the bases of Quadratic Discriminent Analysis(QDA)
partimat(Species~.,data = training_data, method = "qda")


# confusion matrix and accuracy with training data
p1 <- predict(linear, training_data)$class
p1
tab <- table(Predicted = p1, Actual = training_data$Species)
tab 

#Check Accuracy
sum(diag(tab)/sum(tab))


#confusion matrix and accuracy with testing data

p2 <- predict(linear, testing_data)$class
tab1 <- table(predicted = p2, Actual = testing_data$Species)
tab1
sum(diag(tab1)/sum(tab1))





#Analysis by Using Support vector Machine

set.seed(7)
fit.svm <- train(Species~., data=iris, method="svmRadial", metric=metric, trControl=control)

fit.svm


#Analysis by Using Random Forest
set.seed(7)
fit.rf <- train(Species~., data=iris, method="rf", metric=metric, trControl=control)

fit.rf









