##MDA Assignment#1 Association Rule Mining
#Step1 Data transformation
install.packages("readr")
library(readr)
library(arules)
library(arulesViz)
library(wordcloud)


#1
mooc_dataset <- read.csv("big_student_clear_third_version.csv")
mooc_dataset
Institute <- mooc_dataset$institute
Course <- mooc_dataset$course_id
Region <- mooc_dataset$final_cc_cname_DI
Degree <- mooc_dataset$LoE_DI

#2
Region <- gsub(" ", "", Region)
Region <- gsub("\\/", "", Region)
Region <- gsub("\\&", "", Region)
Region <- gsub("\\.", "", Region)
Region <- gsub("\\,", "", Region)


#3
RawTransactions <- paste(Institute, Course, Region, Degree, sep = "_")

#4 
MOOC_transactions <- paste(mooc_dataset$userid_DI, RawTransactions)
head(MOOC_transactions)

#5 
write.csv(MOOC_transactions, file="MOOC_User_Course.csv", row.names = FALSE)

head(MOOC_transactions)


#STEP2 Read data, exploratory analysis
#1 
MOOC <- read.transactions("MOOC_User_Course.csv", format = "single", cols = c(1,2), rm.duplicates=TRUE, skip =1)
head(MOOC)

summary(MOOC)
inspect(MOOC)

#2
MOOC_df <- as(MOOC,"data.frame" )

itemName <- itemLabels(MOOC)
itemCount <- itemFrequency(MOOC)*nrow(MOOC)
itemName
itemCount
summary(itemName)

col <- brewer.pal(7, "Dark2")
wordcloud(words = itemName, freq = itemCount, min.freq = 100, scale = c(2, 1), col = col , random.order = FALSE)

itemFrequencyPlot(MOOC, support = 0.01, cex.names=0.8)


#3
# Rule generation by Apriori 
rules <- apriori(MOOC, parameter=list(support=0.001, confidence=0.05))

# Check the generated rules
inspect(rules)

# List the first three rules with the highest lift values
inspect(sort(rules, by="lift"))

# Save the rules in a text file
write.csv(as(rules, "data.frame"), "MOOC_rules.csv", row.names = TRUE)

# Plot the rules
plot(rules, method = "scatterplot")
plotly_arules(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")
#두번째꺼는 interactive version
plot(rules, method="matrix")
plotly_arules(rules, method = "matrix", measure = c("support", "confidence"), shading = "lift")

# Rule generation by Apriori with another parameters
rules <- apriori(MOOC, parameter=list(support=0.001, confidence=0.05))

plot(rules, method="graph")
plot(rules, method="paracoord") #굵기가 support, 색상이 lift

