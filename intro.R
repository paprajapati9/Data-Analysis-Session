# Section 1 : Analyzing data

# Define path to csv file
path <- "~/P/R/Dataset/Marketing/marketing_campaign.csv"

# create a import data function
importData <- function(path, seperator){
  # importing data from a csv
  readData = read.csv(path, header = TRUE, sep = seperator, stringsAsFactors = FALSE)
  return(readData)
}

# call import data function to import marketing data
marketing_data <- importData(path = path, seperator = "\t")
 
# get structure of data : what column has what kind of values
str(marketing_data)

#import ggplot2
library("ggplot2")

# lets see a plot between education and marital status
# bar graph
ggplot(data = marketing_data , aes(x = Education, fill=Marital_Status)) + 
  geom_bar()+
  labs(title = "Education and Martial Status",
       x="Eduacation",
       y="Count of people")

# remove rows containing NA values
finalData = marketing_data[complete.cases(marketing_data), ]

# set option of when to use scientific notations
options(scipen=1000000)

# plot b/w education and salary
ggplot() + 
  geom_boxplot(data = finalData, aes(x = Education, 
                                     y = Income)) +
  labs(title = "Education vs Income",
       x="Eduacation",
       y="Income")

# Plot b/w marital status, salary and education
ggplot() + 
  geom_boxplot(data = finalData, aes(x = Education, 
                                     y = Income, 
                                     fill = Marital_Status))

# As a company let's see what we are really interested in i.e 
# consumption of our products

# widows and divorced people have the largest wine consumption
ggplot() + 
  geom_boxplot(data = finalData, aes(x = Marital_Status, 
                                     y = MntWines,
                                     fill = Marital_Status))

# Importance of data manipulation

# This plot does not provides any useful insights
ggplot(data = marketing_data) + 
  geom_boxplot(aes(x = Marital_Status, y=MntWines, fill=(Kidhome)))+
  guides(fill=guide_legend(title="Kid at home"))

# As soon as we make these number of kids at home as factors
# This plot starts providing some useful insights
ggplot(data = marketing_data) + 
  geom_boxplot(aes(x = Marital_Status, y=MntWines, fill=as.factor(Kidhome)))+
  guides(fill=guide_legend(title="Kid at home"))

# Let's see how it changes based on number of teenagers at home
ggplot(data = marketing_data) + 
  geom_boxplot(aes(x = Marital_Status, y=MntWines, fill=as.factor(Teenhome)))+
  guides(fill=guide_legend(title="Teenager at home"))

# Try to plot similarly for other products
ggplot(data = marketing_data) + 
  geom_boxplot(aes(x = Marital_Status, y=MntFruits, fill=as.factor(Kidhome)))+
  guides(fill=guide_legend(title="Kid at home"))

# Plot age vs product

# lets check the age group of these people
Age <- c(2021-finalData$Year_Birth)
finalData$Age <- Age

productNames <- c("Wines", "Fruits", "Meat Products", "Fish Products", "Sweet Products", "Gold Products")

# Modify data and bring it to our required format
# For every customer we calculate which product does he/she consumes the most
for (age in 1:length(finalData$Age)) {
    wine <- finalData[age, "MntWines"]
    fruit <- finalData[age, "MntFruits"]
    meat <- finalData[age, "MntMeatProducts"]
    fish <- finalData[age, "MntFishProducts"]
    sweet <- finalData[age, "MntSweetProducts"]
    gold <- finalData[age, "MntGoldProds"]
    productValues <- c(wine, fruit, meat, fish, sweet, gold)
    products <- data.frame(productNames, productValues)
    maxOfAllProducts = products[productValues == max(productValues), ]
    finalData[age, "highest"] <- maxOfAllProducts$productNames[1]
}

# Once we have calculated the maximum used product by each customer
# Let's do some plotting and see which products are prominent in which age group
ggplot(data = finalData) + 
  geom_boxplot(aes(x = highest, y=Age))

# Let's see how kids at home affects the consumption
ggplot(data = finalData) + 
  geom_boxplot(aes(x = highest, y=Age, fill=as.factor(Kidhome)))

# Let's see how teenagers at home affects the consumption
ggplot(data = finalData) + 
  geom_boxplot(aes(x = highest, y=Age, fill=as.factor(Teenhome)))


# Section 2 : Creating word clouds

# import word cloud library
library(wordcloud2) 

tags_data_path = "~/P/R/Dataset/stackoverflow_tags/Tags.csv"
tags_data <- importData(path = tags_data_path, sep=",")

# Change the shape:
wordcloud2(table(tags_data$Tag), 
           size = 0.7,
           color="random-light", 
           backgroundColor = "black")

