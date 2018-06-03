install.packages("arules")
library(arules)

#1)Data Setup
groceries=read.transactions(file="C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/AR_Groceries/groceries-Association-Rules.csv",sep=",")

#2)Uderstanding data
summary(groceries)  
inspect(groceries[1:5])
itemFrequency(groceries[,1:3]) # Provides the proportion of items in the total transactions
itemFrequencyPlot(groceries,support=0.1) #Support limit the item set to display
itemFrequencyPlot(groceries,topN=20) #Display the top 20 items
image(groceries[1:5])
image(sample(groceries,100))

#3)Buildingand evaulating model performance 
groceryrules=apriori(groceries,parameter=list(support=0.006,confidence=0.25,minlen=2))
summary(groceryrules)
inspect(groceryrules[1:5])

#4)Improving Model Performance
inspect(sort(groceryrules,by="lift")[1:5])

#5)Taking Subsets of association rules
berryrules=subset(groceryrules, items %in% "berries")
inspect(berryrules)

milkrules=subset(groceryrules, items %in% "whole milk")
inspect(milkrules)

#6)Saving Association rules to file or data frame
write(groceryrules,file="C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/AR_Groceries/groceryrules.csv",sep=",",quote=T,row.names=F)
groceryrules_dataframe=as(groceryrules,"data.frame")


