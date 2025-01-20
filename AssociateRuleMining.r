df<-train
head(df)
# installations of packages
install.packages("arules")
install.packages("data.table")
install.packages("arulesViz", dependencies = TRUE)
install.packages("tm")
install.packages("SnowballC")
install.packages("tidytext")
#importing the libraries
library(viridis)
library(arules)
library(TSP)
library(data.table)
library(tidyr)
library(arulesViz)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(tidytext)

# Create a list of transactions
transactions_list <- list()

# Loop through each row in the DataFrame
for (i in 1:nrow(df)) {
  # Extract unique items from 'text' and 'summary' columns
  items <- unique(c(strsplit(df$text[i], " ")[[1]], strsplit(df$summary[i], " ")[[1]]))
  
  # Remove any empty strings
  items <- items[items != ""]
  
  # Add the items to the transactions list
  transactions_list[[i]] <- items
}

# Convert the list to a transactions object
transactions <- as(transactions_list, "transactions")
length(transactions_list)
# Inspect the transactions
summary(transactions)
head(transactions)


# Perform association rule mining- apriori
rules_arm <- apriori(transactions, 
                     parameter = list(support = 0.05, confidence = 0.7, minlen = 7))

# Inspect the discovered rules
class(rules_arm)
summary(rules_arm)

# Display the top 15 rules for support
top_rules_support <- head(sort(rules_arm, by = "support"), 15)
top_rules_confidence <- head(sort(rules_arm, by = "confidence"), 15)
top_rules_lift <- head(sort(rules_arm, by = "lift"), 15)

detach(package:tm, unload=TRUE)
cat("Top 15 rules by support:\n")
inspect(top_rules_support)

# Display the top rules for confidence
cat("Top 15 rules by confidence:\n")
inspect(top_rules_confidence)

# Display the top rules for lift
cat("Top 15 rules by lift:\n")
inspect(top_rules_lift)
# Visualize the rules using a network plot
plot(top_rules_support, method = "graph", control = list(type = "graph"))
plot(top_rules_confidence, method = "graph", control = list(type = "graph"))
plot(top_rules_lift, method = "graph", control = list(type = "graph"))