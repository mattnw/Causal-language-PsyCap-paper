data=read.csv("PsyCap Review_R data file.csv") 

#Calculate the number of articles using cross-sectional vs longitudinal designs
freq_table1=table(data$design.type)
freq_table1

#Calculate the proportion that use mediation
freq_table2=table(data$mediation)
prop.table(freq_table2)

library(DescTools)
MultinomCI(freq_table2, conf.level = 0.95, method="sisonglaz")

#Calculate the frequency & proportions of root words identified in the abstracts 
freq_table3=table(data$abs.root)
freq_table3
prop.table(freq_table3)

MultinomCI(freq_table3, conf.level = 0.95, method="sisonglaz")


#Calculate the frequency & proportions of root words identified in the discussions
freq_table4=table(data$dis.root)
freq_table4
prop.table(freq_table4)

MultinomCI(freq_table4, conf.level = 0.95, method="sisonglaz")

#Calculate the frequency & proportions of the causal strength ratings of root words identified in the abstracts
freq_table5=table(data$abs.strength)

freq_table5
prop.table(freq_table5)

MultinomCI(freq_table5, conf.level = 0.95, method="sisonglaz")

#Calculate the proportion of each abstract root word at each rating of causal strength
freq_table6=table(data$abs.root, data$abs.strength)
prop.table(freq_table6, margin = 1)

apply(X = freq_table6, MARGIN = 1, FUN = MultinomCI, method="sisonglaz", simplify = FALSE)

#Calculate the frequency & proportions of the causal strength ratings of root words identified in the discussions
freq_table7=table(data$dis.strength)
freq_table7
prop.table(freq_table7)

MultinomCI(freq_table7, conf.level = 0.95, method="sisonglaz")

#Calculate the proportion of each discussion root word at each rating of causal strength
freq_table8=table(data$dis.root, data$dis.strength)
prop.table(freq_table8, margin =1) 

apply(X = freq_table8, MARGIN = 1, FUN = MultinomCI, method="sisonglaz", simplify = FALSE)

#Calculate the proportion of abstracts which included an action recommendation
freq_table9=table(data$abs.action)
prop.table(freq_table9)

MultinomCI(freq_table9, conf.level = 0.95, method="sisonglaz")

#Calculate the frequency & proportions of the causal strength ratings of action recommendations identified in the abstracts
freq_table10=table(data$abs.action, data$abs.action.strength)
freq_table10
prop.table(freq_table10, margin = 1)

apply(X = freq_table10, MARGIN = 1, FUN = MultinomCI, method="sisonglaz", simplify = FALSE)

#Calculate the proportion of discussions which included an action recommendation
freq_table11=table(data$dis.action)
prop.table(freq_table11)

MultinomCI(freq_table11, conf.level = 0.95, method="sisonglaz")

#Calculate the frequency & proportions of the causal strength ratings of action recommendations identified in the discussions
freq_table12=table(data$dis.action, data$dis.action.strength)
freq_table12
prop.table(freq_table12, margin = 1)

apply(X = freq_table12, MARGIN = 1, FUN = MultinomCI, method="sisonglaz", simplify = FALSE)

#Converting categorical variables in ordered factors for following calculations
data$abs.strength <- factor(data$abs.strength, levels = c("None","Weak","Moderate","Strong"), ordered = TRUE)
data$abs.action.strength <- factor(data$abs.action.strength, levels = c("None","Weak","Moderate","Strong"), ordered =TRUE)
data$dis.strength <- factor(data$dis.strength, levels = c("None","Weak","Moderate","Strong"), ordered =TRUE)
data$dis.action.strength <- factor(data$dis.action.strength, levels = c("None","Weak","Moderate","Strong"), ordered =TRUE)

#Create grouped bar plot showing the difference in distribution of discussion linking sentences' causality vs action recommendation causality
counts <- table(data$dis.action.strength,data$dis.strength)
strengths_long<-data.frame(type = c(rep("Linking sentence causal strength", times = nrow(data)), rep("Action recommendation causality", times = nrow(data))), 
                           strength = c(data$dis.strength, data$dis.action.strength))
strengths_table <-table(strengths_long$strength, strengths_long$type)
strengths_ptable <- prop.table(strengths_table, margin = 2) 

barplot(strengths_ptable, main="",
        xlab="Type",
        ylab="Proportion of Articles",ylim = c(0, 1),
        legend = rownames(counts), beside=TRUE)

#Discussion - Two-tailed Spearman's rho rank test: relationship between strength of causal linking word and action recommendation
data$num.abs.strength <- as.numeric(data$abs.strength)
data$num.abs.action.strength <- as.numeric(data$abs.action.strength)
data$num.dis.strength <- as.numeric(data$dis.strength)
data$num.dis.action.strength <-as.numeric(data$dis.action.strength)

cor.test(data$num.dis.strength, data$num.dis.action.strength, method = "spearman")

SpearmanRho(data$num.dis.strength, data$num.dis.action.strength, use = "pairwise.complete.obs",  conf.level = 0.95)

#Calculate the proportion of articles where the causal implication of action recommendation is stronger than and equal to linking word causal strength
data$num.action.stronger<-data$num.dis.action.strength > data$num.dis.strength
table(data$num.action.stronger)
data$num.action.equal<-data$num.dis.action.strength == data$num.dis.strength
table(data$num.action.equal)

MultinomCI(table(data$num.action.stronger), conf.level = 0.95, method="sisonglaz")
MultinomCI(table(data$num.action.equal), conf.level = 0.95, method="sisonglaz")

#Calculate the proportion of articles which transparently acknowledge their intent to draw causal inference
freq_table13=table(data$causal.intent)
prop.table(freq_table13)

MultinomCI(freq_table13, conf.level = 0.95, method="sisonglaz")

#Calculate the proportion of articles which transparently acknowledge their intent to draw causal inference per each category of causal implication of action recommendations 
freq_table14=table(data$causal.intent, data$dis.action.strength,useNA="always")
freq_table14
prop.table(freq_table14)

apply(X = freq_table14[1:2,], MARGIN = 1, FUN = MultinomCI, method="sisonglaz", simplify = FALSE)

#Calculate the proportion that use causal models
freq_table15=table(data$causal.model.yn)
prop.table(freq_table15)
MultinomCI(freq_table15, conf.level = 0.95, method="sisonglaz")

#Calculate the proportion that use confounds
freq_table16=table(data$confounds)
prop.table(freq_table16)
MultinomCI(freq_table16, conf.level = 0.95, method="sisonglaz")

#Calculate the proportion that control variables
freq_table17=table(data$control)
prop.table(freq_table17)
MultinomCI(freq_table17, conf.level = 0.95, method="sisonglaz")

#Calculate the proportion of articles which include explicit causal disclaimer statements
freq_table18=table(data$disclaimer)
prop.table(freq_table18)

MultinomCI(freq_table18, conf.level = 0.95, method="sisonglaz")

#Calculate the proportion of articles with explicit causal disclaimer statements WHICH implied causal links anyway
    #In the abstract
    disclaimer.yes<-subset(data,data$disclaimer=="Yes")
    freq_table19=table(disclaimer.yes$abs.strength)
    prop.table(freq_table19)

    MultinomCI(freq_table19, conf.level = 0.95, method="sisonglaz")

    #In the discussion
    disclaimer.yes<-subset(data,data$disclaimer=="Yes")
    freq_table20=table(disclaimer.yes$dis.strength)
    prop.table(freq_table20)

    MultinomCI(freq_table20, conf.level = 0.95, method="sisonglaz")

    #In action recommendations
    disclaimer.yes<-subset(data,data$disclaimer=="Yes")
    freq_table21=table(disclaimer.yes$dis.action.strength)
    prop.table(freq_table21)

    MultinomCI(freq_table21, conf.level = 0.95, method="sisonglaz")

#Note: Causal model subgroup proportions calculated in Excel. Frequency graphs created directly in results document.
