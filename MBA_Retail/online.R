library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

#Data preprocessing and exploring
retail <- read_excel('C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/MBA_Retail/Online_retail.xlsx')
retail <- retail[complete.cases(retail), ]
colnames(retail)
retail <- retail %>% mutate(Description = as.factor(Description))
retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)

#What time do people often purchase online?
#In order to find the answer to this question, we need to extract “hour” from the time #column.

retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)

retail %>% 
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")
  
#There is a clear bias between the hour of day and order volume. Most orders happened #between 10:00–15:00.

#How many items each customer buy?

detach("package:plyr", unload=TRUE)

retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))
  
#People mostly purchased less than 10 items (less than 10 items in each invoice).

#Top 10 best sellers

tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp

tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()
  
#Association rules for online retailer
#Before using any rule mining algorithm, we need to transform the data from the data #frame format, into transactions such that we have all the items bought together in one #row.

#The function ddply() accepts a data frame, splits it into pieces based on one or more #factors, computes on the pieces, and then returns the results as a data frame. We use #“,” to separate different items.

#We only need item transactions, so remove customerID and Date columns.

retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                       function(df1)paste(df1$Description, 
                       collapse = ","))
					   
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")	

#Write the data fram to a csv file and check whether our transaction format is correct.
write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)	

#Let’s have a closer look at how many transactions we have and what they are.	

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)		  

#have a look at the item frequency plot,

#We use the Apriori algorithm in Arules library to mine frequent itemsets and #association rules.
#We pass supp=0.001 and conf=0.8 to return all the rules that have a support of at least #0.1% and confidence of at least 80%.
#We sort the rules by decreasing confidence.

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:5])

#And plot these top 10 rules.

topRules <- rules[1:10]
plot(topRules)

plot(topRules, method="graph")
plot(topRules, method = "grouped")

