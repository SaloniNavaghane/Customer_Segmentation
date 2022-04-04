getwd()

install.packages("dyplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("purrr")
install.packages("factoextra")
install.packages("Nbclust")

library(ggplot2)
library(dplyr)
library(purrr)
library(NbClust)
library(factoextra)


#improt csv file to global environment
project<- read.csv("Mall_Customers.csv")

project

nrow(project)
ncol(project)
dim(project)
str(project)
names(project)
head(project)
tail(project)
summary(project$Age)
sd(project$Age)     #standard deviation

summary(project$Annual_Income)
sd(project$Annual_Income)

project %>% select(1,2,3)->CS1
CS1
View(CS1)
project %>% select(3:5)->CS2
CS2
View(CS2)
project %>% select(starts_with("G"))->CS3
CS3
View(CS3)
project %>% select(ends_with("e"))->CS4
CS4
View(CS4)

project %>% filter(Age>23)->CS5
CS5
View(CS5)
project %>% filter(Gender=="Female" , Age<23)->CS6
CS6
View(CS6)

#barplot
a=table(project$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))#Female and male legend box 

#geom_bar
ggplot(data=project ,aes(x=Age , fill=Age)) + geom_bar(fill="pink")

##scatter plot
ggplot(data=project,aes(x=Annual_Income,y=Age,col="orange"))+geom_point()
       
       
c1_freq<-table(project)       
barplot(c1_freq ,beside=TRUE ,legend.Text=T , col=c(1,3))

pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
# we have to install this package if plotrix is not installing properly library(plotrix)

View(project)

ggplot(data = project , aes(x=Age))+geom_histogram(fill="purple" , col="black")




summary(project$Age)

hist(project$Age,
     col="purple",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=T)

boxplot(project$Age,
        col="red",
        main="Boxplot for Descriptive Analysis of Age")

#polygon plot
plot(density(project$Annual_Income),
     col="black",
     main="Density Plot for Annual Income",
     xlab="Gender",
     ylab="Age")
polygon(density(project$Annual_Income),
        col="#ff0000")


#k-means and clusters

install.packages("purrr")  #works with function and vector
library("purrr")
set.seed(123)

iss <- function(k) {
  kmeans(project[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
#value of k
k.values <- 1:10

# function to calculate total intra-cluster sum of square 

iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")




k6<-kmeans(project[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6

set.seed(1)
ggplot(project, aes(x =Annual_Income, y = Spending_score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", 
                                "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")





       
