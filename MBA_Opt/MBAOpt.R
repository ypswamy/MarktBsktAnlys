#Apriori

# Data Preprocessing
# install.packages('arules')
library(arules)
dataset = read.csv('C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/MBA_Opt/Market_Basket_Optimisation.csv', header = FALSE)
dataset = read.transactions('C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/MBA_Opt/Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)
nrow(dataset)
dim(dataset)
str(dataset)
names(dataset)
is.na(dataset)

itemFrequencyPlot(dataset, topN = 10)

# Training Apriori on the dataset
rules = apriori(data = dataset, parameter = list(support = 0.004, confidence = 0.2))
inspect(rules[1:5])

# Visualising the results
inspect(sort(rules, by = 'lift')[1:10])


