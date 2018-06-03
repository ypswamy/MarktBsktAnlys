http://www.salemmarafi.com/code/market-basket-analysis-with-r/

 # Load the libraries

library (arules)

library (arulesViz)

library (datasets)

transDat <- read.transactions ("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/MBA_Grocer/groceries.csv")

transDat

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")

#We set the minimum support to 0.001
#We set the minimum confidence of 0.8
#We then show the top 5 rules

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
 
# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])

#The number of rules generated: 410
#The distribution of rules by length: Most rules are 4 items long
#The summary of quality measures: interesting to see ranges of support, lift, and #confidence.
#The information on the data mined: total data mined, and minimum parameters.

summary(rules)

#we wanted to have the most likely rules. We can easily sort by confidence by executing #the following code.

rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

#Rule 4 is perhaps excessively long. Lets say you wanted more concise rules. That is #also easy to do by adding a “maxlen” parameter to your apriori function:

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
inspect(rules[1:5])

#eliminate these repeated rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned


#What are customers likely to buy before buying whole milk
#What are customers likely to buy if they purchase whole milk?
#This essentially means we want to set either the Left Hand Side and Right Hand Side. 

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

#Likewise, we can set the left hand side to be “whole milk” and find its antecedents.
#Note the following:
#We set the confidence to 0.15 since we get no rules with 0.8
#We set a minimum length of 2 to avoid empty left hand side items
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

#Lets say you wanted to map out the rules in a graph. We can do that with another #library called “arulesViz”.

library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading=NA)


#To get ‘strong‘ rules, increase the value of ‘conf’ parameter.
#To get ‘longer‘ rules, increase ‘maxlen’

#Sort the rules, filter the redundant ones and show the Top 7 Rules.

rules <- sort (rules, decreasing=TRUE,by="confidence")
redundant <- which (colSums(is.subset(rules, rules)) > 1) # get redundant rules in vector
rules <- rules[-redundant] # remove redundant rules inspect (rules[1:7])





