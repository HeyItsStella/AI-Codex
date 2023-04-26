library(ggplot2)

#Importing Data
os <- read.csv('/Users/ChenLiyao/Downloads/os-very-clean.csv')
#clean invalid data (programming years >10)
os <- subset(os, QID10_TEXT<=10)

cs2 <- read.csv('/Users/ChenLiyao/Downloads/cs2-very-clean.csv')
cs2 <- subset(cs2, QID10_TEXT<=10)

myData <- rbind(os, cs2)
#myData <- cs2

#subsetting Data
myData.female <- myData[ which(myData$QID4=='1'), ]
myData.male <- myData[ which(myData$QID4!='1'), ]

myData.firstgen<-myData[which(myData$QID9=='1'),]
myData.nonfirst<-myData[which(myData$QID9!='1'),]

myData.firstgen.female <- myData.firstgen[which(myData.firstgen$QID4=='1'), ]
myData.firstgen.male   <- myData.firstgen[which(myData.firstgen$QID4!='1'), ]

myData.nonfirst.female <- myData.nonfirst[which(myData.nonfirst$QID4=='1'), ]
myData.nonfirst.male <- myData.nonfirst[which(myData.nonfirst$QID4!='1'), ]




#Q1: Do Male Students feel more confident overall?
#{"ImportId":"QID27_4"}		Answer the following questions about your experience programming so far. 
#- I feel confident completing programming assignments by myself.


# Combine the dataframes into one dataframe for plotting
df_combined <- rbind(data.frame(values = myData.female$QID27_4, group = "F"),
                     data.frame(values = myData.male$QID27_4, group = "M"))

ggplot(df_combined, aes(x = group, y = values)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(x = "Group", y = "Values", title = "Boxplot of Two Dataframes")

#geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
res_Q1baselineConfidence <- wilcox.test(na.omit(myData.female$QID27_4),na.omit(myData.male$QID27_4))
print(res_Q1baselineConfidence)

#No, in our data, Male students do not feel more confident in their ability to compelete programming assignments by themselves.



#Q2: Students used Ai tools demonstrates more confidence in completing assignments by themselves?

#{"ImportId":"QID27_4"}		Answer the following questions about your experience programming so far. 
#- I feel confident completing programming assignments by myself.

#subsetting data by {"ImportId":"QID13"}		Do you know about ChatGPT, GitHub Copilot, or any other artificial intelligence (AI) tool for programming?what 
#1----have used, 2&3----haven't used
myData.AIUser <- myData[which(myData$QID13 == '1'),]
myData.nonAIUser <- myData[which(myData$QID13 != '1'),]

useAndNoUse <- rbind(data.frame(values = myData.AIUser$QID27_4, group = "AI users"),
                     data.frame(values = myData.nonAIUser$QID27_4,group = "non AI users"))

ggplot(useAndNoUse, aes(x = group, y = values)) +
  geom_violin() +
  labs(x = "Group", y = "Values", title = "Boxplot of Two Dataframes")

res_Q2useAndNoUseConfidence <- wilcox.test(na.omit(myData.AIUser$QID27_4),na.omit(myData.nonAIUser$QID27_4),exact=FALSE)
print(res_Q2useAndNoUseConfidence)
#




#Q3: Students used Ai tools demonstrates more confidence in finding ways to solve a problem?
#{"ImportId":"QID27_18"}		Answer the following questions about your experience programming so far. - I feel confident finding the steps to solve a programming assignment.

useAndNoUse <- rbind(data.frame(values = myData.AIUser$QID27_18, group = "AI users"),
                     data.frame(values = myData.nonAIUser$QID27_18,group = "non AI users"))

ggplot(useAndNoUse, aes(x = group, y = values)) +
  geom_violin() +
  labs(x = "Group", y = "Values", title = "Boxplot of Two Dataframes")

res_Q3useAndNoUseConfidenceInSolving <- wilcox.test(na.omit(myData.AIUser$QID27_18),na.omit(myData.nonAIUser$QID27_18),exact=FALSE)
print(res_Q3useAndNoUseConfidenceInSolving)

