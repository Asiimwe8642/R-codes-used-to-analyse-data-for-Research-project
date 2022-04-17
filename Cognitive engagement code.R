rm(list=ls())
library(psych);library(dplyr);library(car);library(effsize);library(psych);library(dplyr);library(car); 
library(ggplot2);library(pastecs);library(psych);library(Rcmdr);library(devtools);library(lsr);
library(patchwork);library(dplyr);library(reshape);library(rstatix);library(ggpubr);library(stats);library(tidyverse)


#Note: incase of errors, run the all codes first, as some codes are not arranged in perfect order.


                      #Experimental group
#Lesson with Anitions 1
setwd("C:/Users/Administrator/OneDrive/Documents/Results section/Analysis/Engagement")
cognitiveengagement_a1<-read.csv("Situational Cognitive Engagement First animation analysis.csv",header = TRUE)
names(cognitiveengagement_a1)
#selecting questions only for likert
engagementitems_a1<-cognitiveengagement_a1[,c("X1..I.am.engaged.with.the.topic.at.hand",                
                                              "X2..I.put.in.a.lot.of.effort.understanding.the.topic.",  
                                              "X3..I.wish.I.could.still.continue.for.a.while" ,         
                                              "X4..I.am.so.involved.that.I.forget.everything.around.me")] 

#getting averages for each individual
A_1<-apply(engagementitems_a1,1, mean, na.rm= TRUE)
A_1
describe(A_1)
#normalitytest
shapiro.test(A_1)


# lesson with animation2

setwd("C:/Users/Administrator/OneDrive/Documents/Results section/Analysis/Engagement")
cognitiveengagement_a2<-read.csv("Situational Cognitive Engagement Animations fawe on meselson.csv", header = TRUE)
names(cognitiveengagement_a2)
#selecting questions only for likert
engagementitems_a2<-cognitiveengagement_a2[,c("X1..I.am.engaged.with.the.topic.at.hand",                
                                              "X2..I.put.in.a.lot.of.effort.understanding.the.topic.",  
                                              "X3..I.wish.I.could.still.continue.for.a.while",          
                                              "X4..I.am.so.involved.that.I.forget.everything.around.me")] 


#getting averages for each individual
A_2<-apply(engagementitems_a2,1, mean, na.rm= TRUE)
A_2
mean(A_2)
describe(A_2)





#lesson without animaitons 1 (c1)
setwd("C:/Users/Administrator/OneDrive/Documents/Results section/Analysis/Engagement")
cognitiveengagement_c1<-read.csv("Situational Cognitive Engagement kayonza discusion.csv", header = TRUE)
names(cognitiveengagement_c1)
#selecting questions only for likert
engagementitems_c1<-cognitiveengagement_c1[,c( "X1..I.am.engaged.with.the.topic.at.hand",
                                               "X2..I.put.in.a.lot.of.effort.understanding.the.topic.", 
                                               "X3..I.wish.I.could.still.continue.for.a.while",   
                                               "X4..I.am.so.involved.that.I.forget.everything.around.me")] 
#getting averages for each individual
C_1<-apply(engagementitems_c1,1, mean, na.rm= TRUE)
C_1
mean(C_1)
describe(C_1)

#normalitytest
shapiro.test(C_1)



#lesson without animaitons 2 (c2)


setwd("C:/Users/Administrator/OneDrive/Documents/Results section/Analysis/Engagement")

cognitiveengagement_c2<-read.csv("Situational Cognitive Engagement Modern (prac).csv", header = TRUE)

names(cognitiveengagement_c2)
#selecting questions only for likert
engagementitems_c2<-cognitiveengagement_c2[,c( "X1..I.am.engaged.with.the.topic.at.hand",
                                               "X2..I.put.in.a.lot.of.effort.understanding.the.topic.", 
                                               "X3..I.wish.I.could.still.continue.for.a.while",   
                                               "X4..I.am.so.involved.that.I.forget.everything.around.me")] 

#getting averages for each individual
C_2<-apply(engagementitems_c2,1, mean, na.rm= TRUE)
C_2
mean(C_2)
describe(C_2)
boxplot(C_2)
#normalitytest
shapiro.test(C_2)


#Comparing two types of lessons
Without_Animations<-(C_1+C_2)/2

describe(Without_Animations)

shapiro.test(Without_Animations)
With_Animations <-(A_1 + A_2)/2

shapiro.test(With_Animations)
mean(With_Animations)
describe(With_Animations)
t.test(Without_Animations,A_1)
t.test(With_Animations,Without_Animations, paired = TRUE)
cohensD(Without_Animations,With_Animations, method = 'paired')

mydata<-data.frame(Without_Animations,With_Animations)
mydata
boxplot(mydata)
#compare

dataonlessons<-mydata
dataonlessons$ID<-seq.int(nrow(mydata))
print(dataonlessons)
mynewdata<-melt(dataonlessons, id.vars=c("ID"))
print(mynewdata)
colnames(mynewdata) <- c('ID','Lesson','Cognitive Engagement')
print(mynewdata)

#boxplot for the types of two lessons
ggpaired(mynewdata, x = "Lesson", y = "Cognitive Engagement",
         color = "Lesson", line.color = "#D4D4D4", line.size = 0.4,
         palette = "jco")+
  stat_compare_means( method = "t.test",paired = TRUE)
#ggsave("on engagement.png", dpi = 400)







                      #Control Group
#lesson one
setwd("C:/Users/Administrator/OneDrive/Documents/Results section/Analysis/Engagement")
cognitiveengagement_Control1<-read.csv("Situational Cognitive Engagement FAWE 1.csv", header = TRUE)
names(cognitiveengagement_Control1)
#selecting questions only for likert
engagementitems_control1<-cognitiveengagement_Control1[,c("X1..I.am.engaged.with.the.topic.at.hand",                
                                                          "X2..I.put.in.a.lot.of.effort.understanding.the.topic.",  
                                                          "X3..I.wish.I.could.still.continue.for.a.while" ,         
                                                          "X4..I.am.so.involved.that.I.forget.everything.around.me")] 


#getting averages for each individual
averages_for_each_individual_control1<-apply(engagementitems_control1,1, mean, na.rm= TRUE)
averages_for_each_individual_control1
mean(averages_for_each_individual_control1)
describe(averages_for_each_individual_control1)





# second lesson

setwd("C:/Users/Administrator/OneDrive/Documents/Results section/Analysis/Engagement")
cognitiveengagement_Control2<-read.csv("Situational Cognitive Engagement fawe 2.csv", header = TRUE)
names(cognitiveengagement_Control2)
#selecting questions only for likert
engagementitems_control2<-cognitiveengagement_Control2[,c("X1..I.am.engaged.with.the.topic.at.hand",                
                                                          "X2..I.put.in.a.lot.of.effort.understanding.the.topic..", 
                                                          "X3..I.wish.I.could.still.continue.for.a.while",          
                                                          "X4..I.am.so.involved.that.I.forget.everything.around.me")] 


#getting averages for each individual
averages_for_each_individual_control2<-apply(engagementitems_control2,1, mean, na.rm= TRUE)
averages_for_each_individual_control2
mean(averages_for_each_individual_control2)
describe(averages_for_each_individual_control2)


t.test(averages_for_each_individual_control1,averages_for_each_individual_control2)



#bring data together

datacolum<-data.frame(C_1,C_2,A_1, A_2)
glimpse(datacolum)
myMatrix<-data.matrix(datacolum)
summary(datacolum)
myMatrix
friedmantest<-friedman.test(myMatrix)
friedmantest
myDataLong<-melt(dataonengagement, id.vars=c("ID"))
colnames(myDataLong) <- c('ID','Lesson','Cognitive_Engagement')
res.fried <- myDataLong %>% friedman_test(Cognitive_Engagement ~ Lesson |ID)
res.fried
#melting data = 
dataonengagement<-datacolum
dataonengagement$ID<-seq.int(nrow(dataonengagement))
print.abund(dataonengagement)

myDataLong<-melt(dataonengagement, id.vars=c("ID"))
myDataLong
myDataLong %>% friedman_effsize(Cognitive_Engagement ~ Lesson |ID)
myDataLong
colnames(myDataLong) <- c('ID','Lesson','Cognitive_Engagement')
print((myDataLong))
PWC<-myDataLong%>% wilcox_test(Cognitive_Engagement ~ Lesson , paired = TRUE, 
                               p.adjust.method = "bonferroni")
PWC

# Visualization: box plots with p-values

myDataLong%>%ggplot(aes(x=Lesson,y=Cognitive_Engagement,colour=Lesson))+geom_boxplot()+
  geom_point(size=2,alpha=0.5)+theme_classic()+xlab('Lesson')+ylab('Situational Cognitive Engagement')

PWC<- PWC %>% add_xy_position(x = "Lesson")
ggboxplot(myDataLong, x = "Lesson", y = "Cognitive_Engagement", add = "point")+
          stat_pvalue_manual(PWC, hide.ns = TRUE)+labs(subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(PWC))
#ggsave("boxplot for friedman test2.png",dpi = 400 )





