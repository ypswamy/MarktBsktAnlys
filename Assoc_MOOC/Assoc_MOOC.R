https://www.kaggle.com/ypswamy/association-rule-mining-in-r-on-mooc-dataset/edit

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(arules)

data<- read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/Kaggle/Assoc_MOOC/big_student_clear_third_version.csv")

head(data)
str(data)
summary(data)
names(data)
dim(data)
nrow(data)

data$year= as.factor(data$year)
data$viewed=as.logical(data$viewed)
data$certified=as.logical(data$certified)
data$explored=as.logical(data$explored)

str(data)

newdata<- data[,c("institute","course_id","year","semester","userid_DI","viewed","explored","certified","final_cc_cname_DI","LoE_DI","gender","start_time_DI","last_event_DI")]

head(newdata)
str(newdata)

tData <- as (newdata, "transactions") # convert to 'transactions' class
tData

FrequentItems <- eclat (tData, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items

inspect(FrequentItems[1:10])

# Min Support as 0.001, confidence as 0.8.
rules <- apriori (tData, parameter = list(supp = 0.001, conf = 0.5)) 

# 'high-confidence' rules.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 

# show the support, lift and confidence for all rules
inspect(head(rules_conf)) 

# 'high-lift' rules.
rules_lift <- sort (rules, by="lift", decreasing=TRUE) 
# show the support, lift and confidence for all rules
inspect(head(rules_lift)) 

#Association Rules between Country and courses
cont_data<- data[,c("course_id","final_cc_cname_DI")]
# convert to 'transactions' class
tconData <- as (cont_data, "transactions") 
inspect(tconData[1:5])
# calculates support for frequent items
FrequentItems_con <- eclat (tconData, parameter = list(supp = 0.05, maxlen = 15)) 
inspect(FrequentItems_con[1:5])
# Min Support as 0.001, confidence as 0.8.
rules_con <- apriori (tconData, parameter = list(supp = 0.001, conf = 0.8)) 
# 'high-confidence' rules.
rules_conf_con <- sort (rules_con, by="confidence", decreasing=TRUE) 
# show the support, lift and confidence for all rules
inspect(head(rules_conf_con)) 





