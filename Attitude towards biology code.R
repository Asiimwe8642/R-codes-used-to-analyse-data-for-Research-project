rm(list=ls())
library(psych);library(dplyr);library(car);library(effsize);library(psych);library(dplyr);library(car); 
library(ggplot2);library(pastecs);library(psych);library(Rcmdr);library(devtools);library(lsr);
library(patchwork);library(dplyr);library(reshape);library(rstatix);library(ggpubr);library(stats);library(tidyverse)
library(coin)

                                #PRETEST FOR Experimental Group

setwd("C:/Users/Administrator/OneDrive/Documents/Results section")
#data of pretest survey from Kayonza Modern
pretestattitudetowwardsbiology<-read.csv("A Biology Attitude Scale (BAS) Modern pre.csv", header = TRUE)
#selecting questions only for likert
pretestattitude_questions_only<-pretestattitudetowwardsbiology[,c("X1..Biology.is.very.interesting.to.me.",
                                               "X2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                                               "X3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                                               "X4..Biology.is.fascinating.and.fun",
                                               "X5..Biology.makes.me.feel.secure..and.at.the.same.time..it.is.stimulating.",
                                               "X6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                                               "X7..In.general..I.have.a.good.feeling.toward.biology.",
                                               "X8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                                               "X9..I.approach.biology.with.a.feeling.of.hesitation.",
                                               "X10..I.really.like.biology.",
                                               "X11..I.have.always.enjoyed.studying.biology.in.school.",
                                               "X12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.",
                                               "X13..I.feel.at.ease.in.biology.and.like.it.very.much.",
                                               "X14..I.feel.a.definite.positive.reaction.to.biology..it.s.enjoyable.")] 
#Cronbasch alpha
alpha(pretestattitude_questions_only, keys = c(1,-1,-1,1, 1,-1,1,-1,-1,1,1,-1,1,1))
#postively warded items
postiveitems<-c("X1..Biology.is.very.interesting.to.me.",
                "X4..Biology.is.fascinating.and.fun",
                "X5..Biology.makes.me.feel.secure..and.at.the.same.time..it.is.stimulating.",
                "X7..In.general..I.have.a.good.feeling.toward.biology.",
                "X10..I.really.like.biology.",
                "X11..I.have.always.enjoyed.studying.biology.in.school.",
                "X13..I.feel.at.ease.in.biology.and.like.it.very.much.",
                "X14..I.feel.a.definite.positive.reaction.to.biology..it.s.enjoyable.")
#negative items
negativeitems<-c("X2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                 "X3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                 "X6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                 "X8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                 "X9..I.approach.biology.with.a.feeling.of.hesitation.",
                 "X12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.")
#checking on my data
pretestattitude_questions_only[1:5,negativeitems]
#reversing process
negativeitemsReverse<-c("RX2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                      "RX3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                      "RX6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                      "RX8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                      "RX9..I.approach.biology.with.a.feeling.of.hesitation.",
                      "RX12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.")

#adding this new variable
pretestattitude_questions_only[, negativeitemsReverse]<-(5+1-pretestattitude_questions_only[,negativeitems])
#excluse some data to remain with data that has only reversed questions
pretestattitude_questions_analysis<-pretestattitude_questions_only[c(postiveitems,negativeitemsReverse)]
#check my data
names(pretestattitude_questions_analysis)
#Cronbasch alpha
alpha(pretestattitude_questions_analysis)
#summary statics
#getting sum for each 
summs_for_each_individual<-apply(pretestattitude_questions_analysis,1, sum, na.rm= TRUE)
summs_for_each_individual
mean(summs_for_each_individual)
describe(summs_for_each_individual)



                              #pOST TEST FOR Experimental Group

setwd("C:/Users/Administrator/OneDrive/Documents/Results section")
#data of posttest survey from Kayonza Modern
posttestattitudetowwardsbiology<-read.csv("A Biology Attitude Scale (BAS) post kayonza.csv", header = TRUE)
#posttestattitudetowwardsbiology<-read.csv("A Biology Attitude Scale (BAS) post kayonza 2.csv", header = TRUE)

#selecting questions only for likert
posttestattitude_questions_only<-posttestattitudetowwardsbiology[,c("X1..Biology.is.very.interesting.to.me.",
                                                                  "X2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                                                                  "X3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                                                                  "X4..Biology.is.fascinating.and.fun",
                                                                  "X5..Biology.makes.me.feel.secure..and.at.the.same.time..it.is.stimulating.",
                                                                  "X6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                                                                  "X7..In.general..I.have.a.good.feeling.toward.biology.",
                                                                  "X8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                                                                  "X9..I.approach.biology.with.a.feeling.of.hesitation.",
                                                                  "X10..I.really.like.biology.",
                                                                  "X11..I.have.always.enjoyed.studying.biology.in.school.",
                                                                  "X12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.",
                                                                  "X13..I.feel.at.ease.in.biology.and.like.it.very.much.",
                                                                  "X14..I.feel.a.definite.positive.reaction.to.biology..it.s.enjoyable.")] 
#Reliabilty test
alpha(posttestattitude_questions_only, keys = c(1,-1,-1,1, 1,-1,1,-1,-1,1,1,-1,1,1))
#postively warded items
postiveitemsp<-c("X1..Biology.is.very.interesting.to.me.",
                "X4..Biology.is.fascinating.and.fun",
                "X5..Biology.makes.me.feel.secure..and.at.the.same.time..it.is.stimulating.",
                "X7..In.general..I.have.a.good.feeling.toward.biology.",
                "X10..I.really.like.biology.",
                "X11..I.have.always.enjoyed.studying.biology.in.school.",
                "X13..I.feel.at.ease.in.biology.and.like.it.very.much.",
                "X14..I.feel.a.definite.positive.reaction.to.biology..it.s.enjoyable.")
#negative items post test
negativeitemsp<-c("X2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                 "X3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                 "X6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                 "X8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                 "X9..I.approach.biology.with.a.feeling.of.hesitation.",
                 "X12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.")
#checking on my data
posttestattitude_questions_only[1:5,negativeitemsp]
#reversing process
negativeitemsReversep<-c("RX2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                        "RX3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                        "RX6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                        "RX8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                        "RX9..I.approach.biology.with.a.feeling.of.hesitation.",
                        "RX12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.")

#adding this new variable
posttestattitude_questions_only[, negativeitemsReversep]<-(5+1-posttestattitude_questions_only[,negativeitemsp])
#excluse some data to remain with data that has only reversed questions
posttestattitude_questions_analysis<-posttestattitude_questions_only[c(postiveitemsp,negativeitemsReversep)]
#check my data
names(posttestattitude_questions_analysis)
#Cronbasch alpha
alpha(posttestattitude_questions_analysis)
#summary statics
#getting sum for each 
summs_for_each_individualp<-apply(posttestattitude_questions_analysis,1, sum, na.rm= TRUE)
summs_for_each_individualp
describe(summs_for_each_individualp)
mean(summs_for_each_individualp)

#normality test
shapiro.test(summs_for_each_individualp)
shapiro.test(summs_for_each_individual)
#t.test( summs_for_each_individualp,summs_for_each_individual)
#t.test(averages_for_each_individual,averages_for_each_individualp)
#cohensD(averages_for_each_individual,averages_for_each_individualp)

wilcoxModel<-wilcox.test(averages_for_each_individual,averages_for_each_individualp,paired = TRUE,correct= FALSE)
wilcoxModel

#effect size
wilcox_effsize(averages_for_each_individual~averages_for_each_individualp, paired = TRUE)



#Visualise the data

setwd("C:/Users/Administrator/OneDrive/Documents/Results section/visualisation")
Data_on_quastionnaires<-read.csv("Attitude scales for two groups.csv", header = TRUE)
head(Data_on_quastionnaires)
Results_mutate <-  mutate(Data_on_quastionnaires,Test = relevel(Test, ref="Pretest"))
stat.test <- Results_mutate %>% group_by(Group) %>%rstatix::wilcox_test(Score ~ Test,paired = TRUE)
stat.test <- stat.test %>%  add_xy_position(x = "Test", dodge = 0.8)
bxp <- ggboxplot(Results_mutate, x = "Test", y = "Score", color = "Test", 
palette = c("#00AFBB", "#E7B800"),add = "jitter", facet.by = "Group")
bxp+stat_pvalue_manual( stat.test,  label = "Wilcoxon test, p = {p}", tip.length = 0.01, step.increase = 0.05)+
scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
#ggsave('boxplotsnewnewnew.png', dpi = 400)
#ggsave('boxplots.png', dpi = 400, height = 5, width = 5, unit = 'in')
#ggsave('boxplotsnewnewnew.png', dpi = 400)

#PRETEST FOR control

setwd("C:/Users/Administrator/OneDrive/Documents/Results section")
#data of pretest survey from Kayonza Modern
pretestattitudetowwardsbiologycontrol<-read.csv("A Biology Attitude Scale (BAS) FAWE pre.csv", header = TRUE)
#selecting questions only for likert
pretestattitude_questions_onlycontrol<-pretestattitudetowwardsbiologycontrol[,c("X1..Biology.is.very.interesting.to.me.",
                                                                                "X2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                                                                                "X3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                                                                                "X4..Biology.is.fascinating.and.fun",
                                                                                "X5..Biology.makes.me.feel.secure..and.at.the.same.time..it.is.stimulating.",
                                                                                "X6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                                                                                "X7..In.general..I.have.a.good.feeling.toward.biology.",
                                                                                "X8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                                                                                "X9..I.approach.biology.with.a.feeling.of.hesitation.",
                                                                                "X10..I.really.like.biology.",
                                                                                "X11..I.have.always.enjoyed.studying.biology.in.school.",
                                                                                "X12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.",
                                                                                "X13..I.feel.at.ease.in.biology.and.like.it.very.much.",
                                                                                "X14..I.feel.a.definite.positive.reaction.to.biology..it.s.enjoyable.")] 
summary(pretestattitude_questions_onlycontrol)
#Reliabilty test
alpha(pretestattitude_questions_onlycontrol, keys = c(1,-1,-1,1,1,-1,1,-1,-1,1,1,-1,1,1))
#postively warded items
postiveitemscontrol<-c("X1..Biology.is.very.interesting.to.me.",
                       "X4..Biology.is.fascinating.and.fun",
                       "X5..Biology.makes.me.feel.secure..and.at.the.same.time..it.is.stimulating.",
                       "X7..In.general..I.have.a.good.feeling.toward.biology.",
                       "X10..I.really.like.biology.",
                       "X11..I.have.always.enjoyed.studying.biology.in.school.",
                       "X13..I.feel.at.ease.in.biology.and.like.it.very.much.",
                       "X14..I.feel.a.definite.positive.reaction.to.biology..it.s.enjoyable.")
#negative items
negativeitemscontrol<-c("X2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                        "X3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                        "X6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                        "X8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                        "X9..I.approach.biology.with.a.feeling.of.hesitation.",
                        "X12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.")
#checking on my data
pretestattitude_questions_onlycontrol[1:5,negativeitemscontrol]
#reversing process
negativeitemsReversecontrol<-c("X2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                               "X3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                               "X6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                               "X8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                               "X9..I.approach.biology.with.a.feeling.of.hesitation.",
                               "X12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.")
#adding this new variable
pretestattitude_questions_onlycontrol[, negativeitemsReversecontrol]<-(5+1-pretestattitude_questions_onlycontrol[,negativeitemscontrol])
#excluse some data to remain with data that has only reversed questions
pretestattitude_questions_analysiscontrol<-pretestattitude_questions_onlycontrol[c(postiveitemscontrol,negativeitemsReversecontrol)]
#check my data
names(pretestattitude_questions_analysiscontrol)

#Cronbasch alpha
alpha(pretestattitude_questions_analysiscontrol)
#summary statics
summary(pretestattitude_questions_analysiscontrol)
#getting averages for each individual
averages_for_each_individualcontrol<-apply(pretestattitude_questions_analysiscontrol,1, mean, na.rm= TRUE)
averages_for_each_individualcontrol
mean(averages_for_each_individualcontrol)
describe(averages_for_each_individualcontrol)
#getting sum for each 
summs_for_each_individualcontrol<-apply(pretestattitude_questions_analysiscontrol,1, sum, na.rm= TRUE)
summs_for_each_individualcontrol
shapiro.test(summs_for_each_individualcontrol)
describe(summs_for_each_individualcontrol)

mean(summs_for_each_individualcontrol)



#pOST TEST FOR KAYONZA control

setwd("C:/Users/Administrator/OneDrive/Documents/Results section")
#data of pretest survey from Kayonza Modern
posttestattitudetowwardsbiologycontrol<-read.csv("A Biology Attitude Scale (BAS) post fawe.csv", header = TRUE)
#selecting questions only for likert
posttestattitude_questions_onlycontrol<-posttestattitudetowwardsbiologycontrol[,c("X1..Biology.is.very.interesting.to.me.",
                                                                                  "X2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                                                                                  "X3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                                                                                  "X4..Biology.is.fascinating.and.fun",
                                                                                  "X5..Biology.makes.me.feel.secure..and.at.the.same.time..it.is.stimulating.",
                                                                                  "X6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                                                                                  "X7..In.general..I.have.a.good.feeling.toward.biology.",
                                                                                  "X8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                                                                                  "X9..I.approach.biology.with.a.feeling.of.hesitation.",
                                                                                  "X10..I.really.like.biology.",
                                                                                  "X11..I.have.always.enjoyed.studying.biology.in.school.",
                                                                                  "X12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.",
                                                                                  "X13..I.feel.at.ease.in.biology.and.like.it.very.much.",
                                                                                  "X14..I.feel.a.definite.positive.reaction.to.biology..it.s.enjoyable.")] 
#Reliabilty test
alpha(posttestattitude_questions_onlycontrol, keys = c(1,-1,-1,1, 1,-1,1,-1,-1,1,1,-1,1,1))
#postively warded items
postiveitemspcontrol<-c("X1..Biology.is.very.interesting.to.me.",
                        "X4..Biology.is.fascinating.and.fun",
                        "X5..Biology.makes.me.feel.secure..and.at.the.same.time..it.is.stimulating.",
                        "X7..In.general..I.have.a.good.feeling.toward.biology.",
                        "X10..I.really.like.biology.",
                        "X11..I.have.always.enjoyed.studying.biology.in.school.",
                        "X13..I.feel.at.ease.in.biology.and.like.it.very.much.",
                        "X14..I.feel.a.definite.positive.reaction.to.biology..it.s.enjoyable.")
#negative items post test
negativeitemspcontrol<-c("X2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                         "X3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                         "X6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                         "X8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                         "X9..I.approach.biology.with.a.feeling.of.hesitation.",
                         "X12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.")
#checking on my data
posttestattitude_questions_onlycontrol[1:5,negativeitemspcontrol]
#reversing process
negativeitemsReversepcontrol<-c("X2..I.don.t.like.biology..and.it.scares.me.to.have.to.take.it.",
                                "X3..I.am.always.under.a.terrible.strain.in.a.biology.class.",
                                "X6..Biology.makes.me.feel.uncomfortable..restless..irritable..and.impatient.",
                                "X8..When.I.hear.the.word.biology..I.have.a.feeling.of.dislike.",
                                "X9..I.approach.biology.with.a.feeling.of.hesitation.",
                                "X12..It.makes.me.nervous.to.even.think.about.doing.a.biology.experiment.")

#adding this new variable
posttestattitude_questions_onlycontrol[, negativeitemsReversepcontrol]<-(5+1-posttestattitude_questions_onlycontrol[,negativeitemspcontrol])
#excluse some data to remain with data that has only reversed questions
posttestattitude_questions_analysiscontrol<-posttestattitude_questions_onlycontrol[c(postiveitemspcontrol,negativeitemsReversepcontrol)]
#check my data
names(posttestattitude_questions_analysiscontrol)

#Cronbasch alpha
alpha(posttestattitude_questions_analysiscontrol)
#summary statics
summary(posttestattitude_questions_analysiscontrol)
#getting averages for each individual
averages_for_each_individualpcontrol<-apply(posttestattitude_questions_analysiscontrol,1, mean, na.rm= TRUE)
averages_for_each_individualpcontrol
describe(averages_for_each_individualpcontrol)
mean(averages_for_each_individualpcontrol)
#getting sum for each 
summs_for_each_individualpcontrol<-apply(posttestattitude_questions_analysiscontrol,1, sum, na.rm= TRUE)
summs_for_each_individualpcontrol
describe(summs_for_each_individualpcontrol)
mean(summs_for_each_individualpcontrol)
shapiro.test(summs_for_each_individualpcontrol)

t.test(summs_for_each_individualcontrol, summs_for_each_individualpcontrol)
t.test(summs_for_each_individualpcontrol,summs_for_each_individualcontrol, paired =TRUE)
t.test(averages_for_each_individualcontrol,averages_for_each_individualpcontrol)
cohensD(summs_for_each_individualcontrol, summs_for_each_individualpcontrol, method = 'paired')
#comparing the two means
wilcox.test(summs_for_each_individual,summs_for_each_individualcontrol,paired = FALSE)
wilcox.test(summs_for_each_individualp,summs_for_each_individualpcontrol,paired = FALSE)
Results_mutate %>% group_by(Group) %>%rstatix::wilcox_test(Score ~ Test,paired = TRUE)
Results_mutate %>% group_by(Group) %>%wilcox_effsize(Score ~ Test,paired = TRUE)
Results_mutate %>% group_by(Test) %>%rstatix::wilcox_test(Score ~ Group)











