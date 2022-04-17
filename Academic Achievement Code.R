#Cleaning Enviroment
rm(list=ls())
#libraries
library(psych);library(dplyr);library(car);library(effsize);library(psych);library(dplyr);library(car); 
library(ggplot2);library(pastecs);library(psych);library(Rcmdr);library(devtools);library(lsr);
library(patchwork);library(dplyr);library(reshape);library(rstatix);library(ggpubr);library(stats);library(tidyverse)

#citing packages
citation(package = "stats")
citation(package = "ggplot2")
citation(package = "rstatix")
t.test()
                                #DATA VISUALISATION
#importdata
setwd("C:/Users/Administrator/OneDrive/Documents/Results section/Visualisation")
#Resultsc
Marks_pre_post<-read.csv("Marks For Control and Expermental Groups Pre and Post.csv", header = TRUE)
#viewing the grades
glimpse(Marks_pre_post)
View(Marks_pre_post)
#Pretest grades
attach(Marks_pre_post)
Anova<-aov(Grades~Test)
summary(Anova)


pretestMarks<-Marks_pre_post%>%filter(Test=="PRETEST")

#boxplot
pretestMarks%>%ggplot(aes(x=School,y=Grades,color=School))+geom_boxplot()+
  geom_point(size=2,alpha=0.5)+theme_bw()+xlab('Group')+ylab('Grades')
#ggsave("boxplot.png")
#rearranging the data
Results_mutate <-  mutate(Marks_pre_post,
                          Test = relevel(Test, ref="PRETEST"))
levels(Results_mutate$Test)
print(Results_mutate)

#bargraghv
#getting means
AVERAGES<-Results_mutate%>%group_by(Test,School)%>%summarise(Marks=mean(Grades),starndarddeviation=sd(Grades))
glimpse(AVERAGES)

#ploting bargragh for posttest and pretest for two groups
AVERAGES%>%ggplot(aes(x= School,y=Marks,fill=Test))+geom_bar(position = 'dodge',stat = 'identity', 
    color='black')+ theme_classic()+ scale_fill_manual(values = c(PRETEST='white',POSTTEST='dark gray'))+
    geom_errorbar(aes(ymin=Marks-starndarddeviation, ymax=Marks+starndarddeviation), width=.2,
    position=position_dodge(.9))+  xlab('Group')+ylab('Marks/80')
print(AVERAGES)

ggbarplot(Results_mutate, x = "School", y = "Grades",add = "mean_se",
          color = "Test", palette = "jco", position = position_dodge(0.8))+
          stat_compare_means(aes(group = Test),label = "p.signif" ) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#ggsave("newnewnewbargragh.png",dpi = 400)
#ave the graph
#ggsave("newnewnewbargraghforpreandpost.png",dpi = 400)





#visualisation on DNA test
ResultsonDNA<-read.csv("post test on DNA for both schools.csv", header = TRUE)
glimpse(ResultsonDNA)
View(ResultsonDNA)
summary(ResultsonDNA)
#boxplot for data on dna
BoxolotforquestionsonDNA<- ResultsonDNA%>%ggplot(aes(x=School,y=Grades,color=School))+geom_boxplot()+
  geom_point(size=1,alpha=0.5)+theme_bw()+xlab('Group')+ylab('Grades')+
   scale_fill_manual(values = c(Control='white',Experimental='dark gray'))+
  xlab('Group')+ylab('Marks/40')+theme(legend.position = 'none')
BoxolotforquestionsonDNA

#Visualisisng data on transport of molecules across cell membrane

Resultsotransport<-read.csv("Grades on Transport for both groups pre and post.csv", header = TRUE)
glimpse(Resultsotransport)
summary(ResultsonDNA)
View(Resultsotransport)
#boxplot for data on dna
Boxolotforquestionstransport<-Resultsotransport%>%ggplot(aes(x=School,y=Grades,color=School))+geom_boxplot()+
  geom_point(size=1,alpha=0.5)+theme_bw()+xlab('Group')+ylab('Grades')+
  scale_fill_manual(values = c(Control='white',Experimental='dark gray'))+
  xlab('Group')+ylab('Marks/40')+theme(legend.position = 'none')
Boxolotforquestionstransport
#printout
#BoxolotforquestionsonDNA +Boxolotforquestionstransport+plot_layout(ncol = 1)+
#plot_annotation(tag_levels = 'A')
Boxolotforquestionstransport+BoxolotforquestionsonDNA +
  plot_annotation(tag_levels = 'A')#+ggsave("comparing two post tests.png",dpi = 400)

                                


#Visualise the data on two units

setwd("C:/Users/Administrator/OneDrive/Documents/Results section/visualisation")
Data_on_posttests<-read.csv("Transport and DNA structure two groups.csv", header = TRUE)
names(Data_on_posttests)
stat.test <- stat.test %>%  add_xy_position(x = "Group", dodge = 0.8)
bxp <- ggboxplot(Data_on_posttests, x = "Unit", y = "Scores.40", color = "Group", 
                 palette = c("#045F5F", "#FF8C00"),add = "jitter")
bxp
#ggsave('newboxplot of two topics.png', dpi = 500)






  #DATA ANALYSIS
ResultsfromModern<-read.csv("results from Modern80.csv", header = TRUE)
names(ResultsfromModern)
ResultsfromFawe<-read.csv("results fro Fawe80 posttest pretest.csv", header = TRUE)
#Normality tests
  #Control group(Fawe)
    stat.desc(ResultsfromFawe$posttest.transport, basic = FALSE, norm = TRUE)
    shapiro.test(ResultsfromFawe$X.pretest.80)
    shapiro.test(ResultsfromFawe$X.posttest.80)
    shapiro.test(ResultsfromFawe$postest.dna)
    shapiro.test(ResultsfromFawe$posttest.transport)
    qqPlot(ResultsfromFawe$X.pretest.80)
  #Experimental group(Modern)
    describe(ResultsfromModern$X.80.pretest) 
    stat.desc(ResultsfromFawe$posttest.transport, basic = FALSE, norm = TRUE)
    shapiro.test(ResultsfromModern$X.80.pretest)
    shapiro.test(ResultsfromModern$X.80.postest)
    shapiro.test(ResultsfromModern$X.40.posttest.transport)
    shapiro.test(ResultsfromModern$X.40.posttest.DNA)
    
    
    qqPlot(ResultsfromModern$X.80.pretest)
    qqplot(ResultsfromModern$X.80.postest)
  
#testing diffenrences in means and effect sizes
  #pretest of two schools/80    
    t.test(ResultsfromModern$X.80.pretest, ResultsfromFawe$X.pretest.80)
    cohensD(ResultsfromFawe$X.pretest.80,ResultsfromModern$X.80.pretest)
    describe(ResultsfromFawe$X.pretest.80)
    describe(ResultsfromModern$X.80.pretest)
    t.test()
  #posttest of two schools/80
    t.test(ResultsfromModern$X.80.postest,ResultsfromFawe$X.posttest.80)
    cohensD(ResultsfromModern$X.80.postest,ResultsfromFawe$X.posttest.80)
    describe(ResultsfromModern$X.80.postest)
    describe(ResultsfromFawe$X.posttest.80)
  #posttest between two schools on transport
    t.test(ResultsfromModern$X.40.posttest.transport,ResultsfromFawe$posttest.transport)
    cohensD(ResultsfromFawe$posttest.transport,ResultsfromModern$X.40.posttest.transport)
  #posttest between two school on DNA replication
    t.test(ResultsfromModern$X.40.posttest.DNA,ResultsfromFawe$postest.dna)
    cohensD(ResultsfromFawe$postest.dna,ResultsfromModern$X.40.posttest.DNA)
    
    cohensD(ResultsfromFawe$X.pretest.80,ResultsfromModern$X.80.pretest)
    cohensD(ResultsfromModern$X.80.postest,ResultsfromFawe$X.posttest.8)
 
  #between two post tests
    t.test(ResultsfromFawe)
 
  #discriptive stastics
    describe(ResultsfromFawe$postest.dna)
    describe(ResultsfromModern$X.40.posttest.DNA)
    describe(ResultsfromFawe$posttest.transport)
    describe(ResultsfromModern$X.40.posttest.transpor)
   
  #anova
    


   