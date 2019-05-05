---
title: "What influences language impairment in bilingual aphasia? A meta-analytic review"
date: "14/03/2019"
author: "Ekaterina Kuzmina"
output: 
  pdf_document: 
    fig_height: 4
highlight: atelier forest light
---

```{r setup,include=F}
options(width=120)
knitr::opts_chunk$set(echo=TRUE,warning=F,fig.align="center")
```
\center

# Setup
```{r,echo=TRUE,warning=F,message=F}
#loading packages
library(ggplot2)
library(bitops)
library(metafor)
library(devtools)
library(Hmisc)

#loading data
rm(list=ls())
data<-read.csv("bilingual_aphasia_data.csv",header=TRUE)

#changing types of several variables
data$study_rq     <-as.factor(data$study_rq)
data$proficiency  <-as.factor(data$proficiency)
data$language_use <-as.factor(data$language_use)
data$case_mpo     <-as.integer(data$case_mpo)
data$aoa_adj      <-as.numeric(data$aoa_adj)

#making all measure variables numeric
cols <- colnames(data[,40:153])
data[cols] <- sapply(data[cols],as.numeric)

#creating the 2-level linguistic distance variable
data$ling_similar_2levels<-as.factor(ifelse(data$ling_similarity>0,"similar","different"))

#creating the 3-level linguistic distance variable
data$ling_similar_3levels<-as.factor(ifelse(data$ling_similarity==0,"different",
                                     ifelse(data$ling_similarity==1,"close","very close")))
```

\newpage

# Correlational Analysis

### L1 - Auditory comprehension total and Auditory comprehension testing paradigms
```{r}
rcorr(as.matrix(data[,c("ac_commands_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_story_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_picture_matching_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_gram_judg_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_lex_dec_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_sem_rel_judg_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_other_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```

### L1 - Oral production total and Auditory comprehension testing paradigms
```{r}
rcorr(as.matrix(data[,c("ac_commands_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_story_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_picture_matching_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_gram_judg_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_lex_dec_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_sem_rel_judg_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_other_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_l1_cor_per","op_l1_cor_per")]),type="spearman")
```

### L1 - Overall performance and Auditory comprehension testing paradigms
```{r}
rcorr(as.matrix(data[,c("ac_commands_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_story_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_picture_matching_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_gram_judg_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_lex_dec_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_sem_rel_judg_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_other_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_l1_cor_per","total_l1_cor_per")]),type="spearman")
```

### L2 - Auditory comprehension total and Auditory comprehension testing paradigms
```{r}
rcorr(as.matrix(data[,c("ac_commands_l2_cor_per","ac_l2_cor_per")]), type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_story_l2_cor_per","ac_l2_cor_per")]), type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_picture_matching_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_gram_judg_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_lex_dec_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_sem_rel_judg_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_other_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```

### L2 - Oral production total and Auditory comprehension testing paradigms
```{r}
rcorr(as.matrix(data[,c("ac_commands_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_story_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_picture_matching_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_gram_judg_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_lex_dec_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_sem_rel_judg_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_other_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_l2_cor_per","op_l2_cor_per")]),type="spearman")
```

### L2 - Overall performance and Auditory comprehension testing paradigms 
```{r}
rcorr(as.matrix(data[,c("ac_commands_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_story_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_picture_matching_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_gram_judg_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_lex_dec_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_sem_rel_judg_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_other_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("ac_l2_cor_per","total_l2_cor_per")]),type="spearman")
```

### L1 - Auditory comprehension total and Oral production testing paradigms
```{r}
rcorr(as.matrix(data[,c("op_naming_l1_cor_per","ac_l1_cor_per")]), type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_repetition_l1_cor_per","ac_l1_cor_per")]), type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_ans_quest_sent_compl_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_sent_constr_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_semantic_opposites_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_morph_derivates_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_spont_semispont_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```

### L1 - Oral production total and Oral production testing paradigms
```{r}
rcorr(as.matrix(data[,c("op_naming_l1_cor_per","op_l1_cor_per")]), type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_repetition_l1_cor_per","op_l1_cor_per")]), type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_ans_quest_sent_compl_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_sent_constr_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_semantic_opposites_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_morph_derivates_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_spont_semispont_l1_cor_per","op_l1_cor_per")]),type="spearman")
```

### L1 - Overall performance and Oral production testing paradigms
```{r}
rcorr(as.matrix(data[,c("op_naming_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_repetition_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_ans_quest_sent_compl_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_sent_constr_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_semantic_opposites_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_morph_derivates_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_spont_semispont_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_l1_cor_per","total_l1_cor_per")]),type="spearman")
```

### L2 - Auditory comprehension total and Oral production testing paradigms
```{r}
rcorr(as.matrix(data[,c("op_naming_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_repetition_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_ans_quest_sent_compl_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_sent_constr_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_semantic_opposites_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_morph_derivates_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_spont_semispont_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```

### L2 - Oral production total and Oral production testing paradigms
```{r}
rcorr(as.matrix(data[,c("op_naming_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_repetition_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_ans_quest_sent_compl_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_sent_constr_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_semantic_opposites_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_morph_derivates_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_spont_semispont_l2_cor_per","op_l2_cor_per")]),type="spearman")
```

### L2 - Overall performance and Oral production testing paradigms
```{r}
rcorr(as.matrix(data[,c("op_naming_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_repetition_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_ans_quest_sent_compl_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_sent_constr_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_semantic_opposites_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_morph_derivates_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_spont_semispont_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("op_l2_cor_per","total_l2_cor_per")]),type="spearman")
```

### L1 - Auditory comprehension total and Other paradigms and Other tests
```{r}
rcorr(as.matrix(data[,c("ra_reading_aloud_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wc_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wp_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("other_l1_cor_per","ac_l1_cor_per")]),type="spearman")
```

### L1 - Oral production total and Other paradigms and Other tests
```{r}
rcorr(as.matrix(data[,c("ra_reading_aloud_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wc_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wp_l1_cor_per","op_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("other_l1_cor_per","op_l1_cor_per")]),type="spearman")
```

### L1 - Overall performance and Other paradigms and Other tests
```{r}
rcorr(as.matrix(data[,c("ra_reading_aloud_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wc_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wp_l1_cor_per","total_l1_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("other_l1_cor_per","total_l1_cor_per")]),type="spearman")
```

### L2 - Auditory comprehension total and Other paradigms and Other tests
```{r}
rcorr(as.matrix(data[,c("ra_reading_aloud_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wc_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wp_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("other_l2_cor_per","ac_l2_cor_per")]),type="spearman")
```

### L2 - Oral production total and Other paradigms and Other tests
```{r}
rcorr(as.matrix(data[,c("ra_reading_aloud_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wc_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wp_l2_cor_per","op_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("other_l2_cor_per","op_l2_cor_per")]),type="spearman")
```

### L2 - Overall performance and Other paradigms and Other tests
```{r}
rcorr(as.matrix(data[,c("ra_reading_aloud_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wc_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("wp_l2_cor_per","total_l2_cor_per")]),type="spearman")
```
```{r}
rcorr(as.matrix(data[,c("other_l2_cor_per","total_l2_cor_per")]),type="spearman")
```

\newpage

# Data Screening
```{r}
#removing simultaneous bilinguals from the dataset
data<-data[!(data$early_or_late == "simult"),]
```

### Overall Performance
```{r}
data<-escalc(measure="RR",
             ai=total_l1_cor,bi=total_l1_wrong,ci=total_l2_cor,di=total_l2_wrong, 
             data=data,append=TRUE)

data<-data[!is.na(data$yi),] 

overall<-rma(yi,vi,data=data)

summary(data$yi)
```
```{r }
sum(!is.na(data$yi)) 
```

### Auditory Comprehension
```{r}
data_ac<-escalc(measure="RR",
                ai=ac_l1_cor,bi=ac_l1_wrong,ci=ac_l2_cor,di=ac_l2_wrong,
                data=data,append=TRUE)

data_ac<-data_ac[!is.na(data_ac$yi),] 

ac<-rma(yi,vi,data=data_ac)

summary(data_ac$yi)
```
```{r }
sum(!is.na(data_ac$yi)) 
```

\newpage

### Oral Production
```{r}
data_op<-escalc(measure="RR",
                  ai=op_l1_cor,bi=op_l1_wrong,ci=op_l2_cor,di=op_l2_wrong,
                  data=data,append=TRUE)

data_op<-data_op[!is.na(data_op$yi),]

op<-rma(yi,vi,data=data_op)

summary(data_op$yi)
```
```{r }
sum(!is.na(data_op$yi)) 
```

\newpage

```{r}
#creating funnel plots
par(mfrow=c(1,3))
funnel_overall<-funnel(overall,
                      refline=0,level=c(90,95,99),shade=c("white","red","orange"),
                      ylab="Overall Performance",xlab="Risk Ratio (log)",
                      ylim=c(1.2,0),xlim=c(-2.5,2.5),steps=13,digits=c(1,1),
                      pch=1,cex.axis=0.5,cex.lab=0.8,cex.sub=0.8)
funnel_ac<-funnel(ac,
                      refline=0,level=c(90,95,99),shade=c("white","red","orange"),
                      ylab="Auditory Comprehension",xlab="Risk Ratio (log)",
                      ylim=c(1.2,0),xlim=c(-2.5,2.5),steps=13,digits=c(1,1),
                      pch=6,cex.axis=0.5,cex.lab=0.8,cex.sub=0.8)
funnel_op<-funnel(op,
                      refline=0,level=c(90,95,99),shade=c("white","red","orange"),
                      ylab="Oral Production",xlab="Risk Ratio (log)",
                      ylim=c(1.2,0),xlim=c(-2.5,2.5),steps=13,digits=c(1,1),
                      pch=5,cex.axis=0.5,cex.lab=0.8,cex.sub=0.8)
```

\newpage

```{r fig.width = 5, fig.height = 7.6}
par(mfrow=c(3,1))
hist(data$yi,breaks=seq(-2.5,2.5,by=0.10),main=NULL)
hist(data_ac$yi,breaks=seq(-2.5,2.5,by=0.10),main=NULL)
hist(data_op$yi,breaks=seq(-2.5,2.5,by=0.10),main=NULL)
```

\newpage

# Data Trimming (deleting cases with SE > 0.3)

### Overall Performance
```{r}
data=data[data$vi <= 0.09,]


summary(data$yi)
```
```{r}
sum(!is.na(data$yi))
```

### Auditory Comprehension
```{r}
data_ac=data_ac[data_ac$vi <= 0.09,]

summary(data_ac$yi)
```
```{r }
sum(!is.na(data_ac$yi))
```

### Oral Production
```{r}
data_op=data_op[data_op$vi <= 0.09,]

summary(data_op$yi)
```
```{r }
sum(!is.na(data_op$yi))
```

\newpage

```{r fig.width = 5, fig.height = 7.6}
par(mfrow=c(3,1))
hist(data$yi,breaks=seq(-2.5,2.5,by=0.10),main=NULL)
hist(data_ac$yi,breaks=seq(-2.5,2.5,by=0.10),main=NULL)
hist(data_op$yi,breaks=seq(-2.5,2.5,by=0.10),main=NULL)
```
\newpage

# Language status: Difference between L1 and L2

### Overall Performance
```{r}
data<-data[order(data=data$early_or_late, data=data$yi),]
print(m1<-rma(yi,vi,data=data))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m1$b,m1$ci.lb,m1$ci.ub)),digits=2)
```

\newpage

### Forestplot for Overall Performance for the whole group
```{r fig.width = 25, fig.height = 35}
forest(m1,showweights=TRUE,
       slab = paste(data$study_forestplot),
       xlim = c(-6, 2.5), cex = 1,font = 1, 
       at = log(c(0.15, 0.2, 0.3, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)), 
       atransf = exp,
       ilab =  data.frame(as.character(data$early_or_late), 
                          as.character(data$proficiency), 
                          as.character(data$language_use), 
                          as.character(data$ling_similar_2levels)),
       ilab.xpos = c(-4.5,-3.75,-3,-2.25))

op <- par(cex=1.2,font=2)
text(c(-4.5,-3.75,-3,-2.25),121,c("AoA subgroup", 
                                   "Proficiency", 
                                   "Language use", 
                                   "Linguistic similarity"))
text(-6, 121,"Cases",pos=4)
text(2.5, 121,"RR [95% CI]", pos=2)
text(2, 121,"Weight", pos=2)
```

### Auditorry Comprehension
```{r}
print(m2<-rma(yi,vi,data=data_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m2$b,m2$ci.lb,m2$ci.ub)),digits=2)
```

\newpage

### Oral Production
```{r}
print(m3<-rma(yi,vi,data=data_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m3$b,m3$ci.lb,m3$ci.ub)),digits=2)
```

\newpage

# Moderator Analysis

## Age of Language Aquisition (AoA)

## Does continuous AoA moderate the difference betwen L1 and L2?

### Overall Performance
```{r }
summary(data$aoa_adj)
```
```{r}
print(m4<-rma(yi,vi,mods=aoa_adj,data=data))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m4$b,m4$ci.lb,m4$ci.ub)),digits=2)
```

\newpage

### Plot based on 7 years as a cut-off for Overall Performance
```{r }
p <- ggplot(data, aes(x=aoa_adj, 
                      y=yi , 
                      color=early_or_late))+
  scale_color_manual(name = "AoA subgroup", 
                     values = c("red", "blue")) +
  geom_point(size = 1.5)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylim(-1.5, 1.5)+
  xlim(2, 40)+
  labs(y="Effect sizes", x = "AoA, year")
p
```

\newpage

### Auditory Comprehension
```{r }
summary(data_ac$aoa_adj)
```
```{r}
print(m5<-rma(yi,vi,mods=aoa_adj,data=data_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m5$b,m5$ci.lb,m5$ci.ub)),digits=2)
```

\newpage

### Oral Production
```{r }
summary(data_op$aoa_adj)
```
```{r}
print(m6<-rma(yi,vi,mods=aoa_adj,data=data_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m6$b,m6$ci.lb,m6$ci.ub)),digits=2)
```

\newpage

## Does the early-late bilingual status based on 7 years cut-off moderate the difference betwen L1 and L2?

### Overall Performance
```{r }
data$late <-ifelse(data$early_or_late=="late",1,0)
data$early<-ifelse(data$early_or_late=="early",1,0)
```
```{r}
print(c(sum(data$late=="1"),sum(data$early=="1")))
```
```{r}
print(m7<-rma(yi,vi,mods=cbind(late,early),intercept=F,data=data))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m7$b,m7$ci.lb,m7$ci.ub)),digits=2)
```
Checking whether the eraly-late bilingual status is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(late,early),data=data))[c(8,10,11,13,14)]
```

### Auditory Comprehension
```{r }
data_ac$late <-ifelse(data_ac$early_or_late=="late",1,0)
data_ac$early<-ifelse(data_ac$early_or_late=="early",1,0)
```
```{r}
summary(as.factor(data_ac$late))
```
```{r }
print(m8<-rma(yi,vi,mods=cbind(late,early),intercept=F,data=data_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m8$b,m8$ci.lb,m8$ci.ub)),digits=2)
```
Checking whether the eraly-late bilingual status is a significant moderator. 
```{r }
capture.output(rma(yi,vi,mods=cbind(late,early),data=data_ac))[c(8,10,11,13,14)]
```

### Oral Production
```{r }
data_op$late<-ifelse(data_op$early_or_late=="late",1,0)
data_op$early<-ifelse(data_op$early_or_late=="early",1,0)
```
```{r}
summary(as.factor(data_op$late))
```
```{r }
print(m9<-rma(yi,vi,mods=cbind(late,early),intercept=F,data=data_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m9$b,m9$ci.lb,m9$ci.ub)),digits=2)
```
Checking whether the eraly-late bilingual status is a significant moderator. 
```{r }
capture.output(rma(yi,vi,mods=cbind(late,early),data=data_op))[c(8,10,11,13,14)]
```

### Creating subsets for the early and late subgroups
```{r}
#for overall performance
data_e_overall<-data[data$early=="1",]
data_l_overall<-data[data$late=="1",]

#for auditory comprehension
data_e_ac<-data_ac[data_ac$early=="1",]
data_l_ac<-data_ac[data_ac$late=="1",]

#for oral production
data_e_op<-data_op[data_op$early=="1",]
data_l_op<-data_op[data_op$late=="1",]
```

\newpage

# Demographic and clinical details

```{r}
cols<-c("case_age","case_scholarity_years","case_gender","case_mpo","aoa_adj","case_lesion_side",
        "proficiency","language_use","ling_similar_2levels","ling_similar_3levels")
```

### Whole group
```{r}
sapply(data[cols],summary)
```

\newpage

### Early subgroup
```{r}
sapply(data_e_overall[cols],summary)

```

\newpage

### Late subgroup
```{r}
sapply(data_l_overall[cols],summary)
```

\newpage

## Does continuous AoA moderate the difference betwern L1 and L2 in the early and late subgroups separately?

### Early subgroup - Overall Performance
```{r }
summary(data_e_overall$aoa_adj)
```
```{r}
print(m10<-rma(yi,vi,mods=aoa_adj,data=data_e_overall))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m10$b,m10$ci.lb,m10$ci.ub)),digits=2)
```

\newpage

### Early subgroup - Auditory Comprehension
```{r }
summary(data_e_ac$aoa_adj)
```
```{r}
print(m11<-rma(yi,vi,mods=aoa_adj,data=data_e_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m11$b,m11$ci.lb,m11$ci.ub)),digits=2)
```

\newpage

### Early subgroup - Oral Production
```{r }
summary(data_e_op$aoa_adj)
```
```{r}
print(m12<-rma(yi,vi,mods=aoa_adj,data=data_e_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m12$b,m12$ci.lb,m12$ci.ub)),digits=2)
```

\newpage

### Late subgroup - Overall Performance
```{r }
summary(data_l_overall$aoa_adj)
```
```{r}
print(m13<-rma(yi,vi,mods=aoa_adj,data=data_l_overall))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m13$b,m13$ci.lb,m13$ci.ub)),digits=2)
```

\newpage

### Late subgroup - Auditory Comprehension
```{r }
summary(data_l_ac$aoa_adj)
```
```{r}
print(m14<-rma(yi,vi,mods=aoa_adj,data=data_l_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m14$b,m14$ci.lb,m14$ci.ub)),digits=2)
```

\newpage

### Late subgroup - Oral Production
```{r }
summary(data_l_op$aoa_adj)
```
```{r}
print(m15<-rma(yi,vi,mods=aoa_adj,data=data_l_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m15$b,m15$ci.lb,m15$ci.ub)),digits=2)
```

\newpage

## Premorbid Language Proficiency

## Does proficiency moderate the difference betwen L1 and L2?

### Whole group - Overall Performance
```{r}
summary(data$proficiency)
```
```{r }
data$equal<-ifelse(data$proficiency=="equal",1,0)
data$l1   <-ifelse(data$proficiency=="L1",1,0)
data$l2   <-ifelse(data$proficiency=="L2",1,0) 
```
```{r }
print(m16<-rma(yi,vi,mods=cbind(equal,l1,l2),intercept=F,data=data))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m16$b,m16$ci.lb,m16$ci.ub)),digits=2)
```
Checking whether proficiency is a significant moderator. 
```{r }
rma(yi,vi,mods=cbind(equal,l1),data=subset(data,proficiency %in% c("equal","L1")))
```

\newpage

### Whole group - Auditory Comprehension
```{r}
summary(data_ac$proficiency)
```
```{r }
data_ac$equal<-ifelse(data_ac$proficiency=="equal",1,0)
data_ac$l1   <-ifelse(data_ac$proficiency=="L1",1,0)
data_ac$l2   <-ifelse(data_ac$proficiency=="L2",1,0)
```
```{r } 
print(m17<-rma(yi,vi,mods=cbind(equal,l1,l2),intercept=F,data=data_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m17$b,m17$ci.lb,m17$ci.ub)),digits=2)
```
Checking whether proficiency is a significant moderator.
```{r }
rma(yi,vi,mods=cbind(equal,l1),data=subset(data_ac,proficiency %in% c("equal","L1")))
```

\newpage

### Whole group - Oral Production
```{r}
summary(data_op$proficiency)
```
```{r }
data_op$equal<-ifelse(data_op$proficiency=="equal",1,0)
data_op$l1   <-ifelse(data_op$proficiency=="L1",1,0)
data_op$l2   <-ifelse(data_op$proficiency=="L2",1,0)
```
```{r}
print(m18<-rma(yi,vi,mods=cbind(equal,l1,l2),intercept=F,data=data_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m18$b,m18$ci.lb,m18$ci.ub)),digits=2)
```
Checking whether proficiency is a significant moderator.
```{r }
rma(yi,vi,mods=cbind(equal,l1),data=subset(data_op,proficiency %in% c("equal","L1")))
```

\newpage

### Early subgroup - Overall Performance
```{r}
summary(data_e_overall$proficiency)
```
```{r }
data_e_overall$equal<-ifelse(data_e_overall$proficiency=="equal",1,0)
data_e_overall$l1   <-ifelse(data_e_overall$proficiency=="L1",1,0)
data_e_overall$l2   <-ifelse(data_e_overall$proficiency=="L2",1,0)
```
```{r }
print(m19<-rma(yi,vi,mods=cbind(equal,l1,l2),intercept=F,data=data_e_overall))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m19$b,m19$ci.lb,m19$ci.ub)),digits=2)
```

\newpage

### Early subgroup - Auditory Comprehension
```{r}
summary(data_e_ac$proficiency)
```
```{r }
data_e_ac$equal<-ifelse(data_e_ac$proficiency=="equal",1,0)
data_e_ac$l1   <-ifelse(data_e_ac$proficiency=="L1",1,0)
data_e_ac$l2   <-ifelse(data_e_ac$proficiency=="L2",1,0)
```
```{r }
print(m20<-rma(yi,vi,mods=cbind(equal,l1,l2),intercept=F,data=data_e_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m20$b,m20$ci.lb,m20$ci.ub)),digits=2)
```

\newpage

### Early subgroup - Oral Production
```{r}
summary(data_e_op$proficiency)
```
```{r }
data_e_op$equal<-ifelse(data_e_op$proficiency=="equal",1,0)
data_e_op$l1   <-ifelse(data_e_op$proficiency=="L1",1,0)
data_e_op$l2   <-ifelse(data_e_op$proficiency=="L2",1,0)
```
```{r}
print(m21<-rma(yi,vi,mods=cbind(equal,l1,l2),intercept=F,data=data_e_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m21$b,m21$ci.lb,m21$ci.ub)),digits=2)
```

\newpage

### Late subgroup - Overall Performance
```{r}
summary(data_l_overall$proficiency)
```
```{r }
data_l_overall$equal<-ifelse(data_l_overall$proficiency=="equal",1,0)
data_l_overall$l1   <-ifelse(data_l_overall$proficiency=="L1",1,0)
```
```{r}
print(m22<-rma(yi,vi,mods=cbind(equal,l1),intercept=F,data=data_l_overall))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m22$b,m22$ci.lb,m22$ci.ub)),digits=2)
```
Checking whether proficiency is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(equal,l1),data=data_l_overall))[c(8,10,11,13,14)]
```

\newpage

### Late subgroup - Auditory Comprehension
```{r}
summary(data_l_ac$proficiency)
```
```{r }
data_l_ac$equal<-ifelse(data_l_ac$proficiency=="equal",1,0)
data_l_ac$l1   <-ifelse(data_l_ac$proficiency=="L1",1,0)
```
```{r}
print(m23<-rma(yi,vi,mods=cbind(equal,l1),intercept=F,data=data_l_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m23$b,m23$ci.lb,m23$ci.ub)),digits=2)
```
Checking whether proficiency is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(equal,l1),data=data_l_ac))[c(8,10,11,13,14)]
```

\newpage

### Late subgroup - Oral Production
```{r}
summary(data_l_op$proficiency)
```
```{r }
data_l_op$equal<-ifelse(data_l_op$proficiency=="equal",1,0)
data_l_op$l1   <-ifelse(data_l_op$proficiency=="L1",1,0)
```
```{r}
print(m24<-rma(yi,vi,mods=cbind(equal,l1),intercept=F,data=data_l_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m24$b,m24$ci.lb,m24$ci.ub)),digits=2)
```
Checking whether proficiency is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(equal,l1),data=data_l_op))[c(8,10,11,13,14)]
```

\newpage

## Language Use

## Does language use moderate the difference betwen L1 and L2?

### Whole group - Overlall Performance
```{r}
summary(data$language_use)
```
```{r }
data$equal.use<-ifelse(data$language_use=="equal",1,0)
data$l1.use   <-ifelse(data$language_use=="L1",1,0)
data$l2.use   <-ifelse(data$language_use=="L2",1,0)
```
```{r}
print(m25<-rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),intercept=F,data=data))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m25$b,m25$ci.lb,m25$ci.ub)),digits=2)
```
Checking whether language use is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),data=data))[8:24]
```
```{r}
capture.output(rma(yi,vi,mods=~relevel(factor(language_use),ref="equal"),data=data))[8:24]
```

\newpage

### Whole group - Auditory Comprehension
```{r}
summary(data_ac$language_use)
```
```{r }
data_ac$equal.use<-ifelse(data_ac$language_use=="equal",1,0)
data_ac$l1.use   <-ifelse(data_ac$language_use=="L1",1,0)
data_ac$l2.use   <-ifelse(data_ac$language_use=="L2",1,0)
```
```{r}
print(m26<-rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),intercept=F,data=data_ac))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI.
round(exp(c(m26$b,m26$ci.lb,m26$ci.ub)),digits=2)
```
Checking whether language use is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),data=data_ac))[8:24]
```
```{r}
capture.output(rma(yi,vi,mods=~relevel(factor(language_use),ref="equal"),data=data_ac))[8:24]
```

\newpage

### Whole group - Oral Production
```{r}
summary(data_op$language_use)
```
```{r }
data_op$equal.use<-ifelse(data_op$language_use=="equal",1,0)
data_op$l1.use   <-ifelse(data_op$language_use=="L1",1,0)
data_op$l2.use   <-ifelse(data_op$language_use=="L2",1,0)
```
```{r}
print(m27<-rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),intercept=F,data=data_op))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI.
round(exp(c(m27$b,m27$ci.lb,m27$ci.ub)),digits=2)
```
Checking whether language use is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),data=data_op))[8:24]
```
```{r}
capture.output(rma(yi,vi,mods=~relevel(factor(language_use),ref="equal"), data=data_op))[8:24]
```

\newpage

### Early subgroup - Overall Performance
```{r}
summary(data_e_overall$language_use)
```
```{r }
data_e_overall$equal.use<-ifelse(data_e_overall$language_use=="equal",1,0)
data_e_overall$l1.use   <-ifelse(data_e_overall$language_use=="L1",1,0)
data_e_overall$l2.use   <-ifelse(data_e_overall$language_use=="L2",1,0)
```
```{r}
print(m28<-rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),intercept=F,data=data_e_overall))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m28$b,m28$ci.lb,m28$ci.ub)),digits=2)
```
Checking whether language use is a significant moderator. 
```{r}
data_e_overall_2<-subset(data_e_overall,language_use %in% c("equal","L2"))
capture.output(rma(yi,vi,mods=cbind(equal.use,l2.use),data=data_e_overall_2))[c(8,10,11,13,14)]
```

### Early subgroup - Auditory Comprehension
```{r}
summary(data_e_ac$language_use)
```
```{r }
data_e_ac$equal.use<-ifelse(data_e_ac$language_use=="equal",1,0)
data_e_ac$l1.use   <-ifelse(data_e_ac$language_use=="L1",1,0)
data_e_ac$l2.use   <-ifelse(data_e_ac$language_use=="L2",1,0)
```
```{r}
print(m29<-rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),intercept=F,data=data_e_ac))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI.
round(exp(c(m29$b,m29$ci.lb,m29$ci.ub)),digits=2)
```
Checking whether language use is a significant moderator. 
```{r}
data_e_ac_2<-subset(data_e_ac,language_use %in% c("equal","L2"))
capture.output(rma(yi,vi,mods=cbind(equal.use,l2.use),data=data_e_ac_2))[c(8,10,11,13,14)]
```

### Early subgroup - Oral Production
```{r}
summary(data_e_op$language_use)
```
```{r }
data_e_op$equal.use<-ifelse(data_e_op$language_use=="equal",1,0)
data_e_op$l1.use   <-ifelse(data_e_op$language_use=="L1",1,0)
data_e_op$l2.use   <-ifelse(data_e_op$language_use=="L2",1,0)
```
```{r}
print(m30<-rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),intercept=F,data=data_e_op))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI.
round(exp(c(m30$b,m30$ci.lb,m30$ci.ub)),digits=2)
```
Checking whether language use is a significant moderator. 
```{r}
data_e_op_2<-subset(data_e_op,language_use %in% c("equal","L2"))
capture.output(rma(yi,vi,mods=cbind(equal.use,l2.use),data=data_e_op_2))[c(8,10,11,13,14)]
```

\newpage

### Late subgroup - Overall Performance
```{r}
summary(data_l_overall$language_use)
```
```{r }
data_l_overall$equal.use<-ifelse(data_l_overall$language_use=="equal",1,0)
data_l_overall$l1.use   <-ifelse(data_l_overall$language_use=="L1",1,0)
data_l_overall$l2.use   <-ifelse(data_l_overall$language_use=="L2",1,0)
```
```{r}
print(m31<-rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),intercept=F,data=data_l_overall))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m31$b,m31$ci.lb,m31$ci.ub)),digits=2)
```
Checking whether language use is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),data=data_l_overall))[c(8,10,11,13,14)]
```

### Late subgroup - Auditory Comprehension
```{r}
summary(data_l_ac$language_use)
```
```{r }
data_l_ac$equal.use<-ifelse(data_l_ac$language_use=="equal",1,0)
data_l_ac$l1.use   <-ifelse(data_l_ac$language_use=="L1",1,0)
data_l_ac$l2.use   <-ifelse(data_l_ac$language_use=="L2",1,0)
```
```{r}
print(m32<-rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),intercept=F,data=data_l_ac))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m32$b,m32$ci.lb,m32$ci.ub)),digits=2)
```
Checking whether language use is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),data=data_l_ac))[c(8,10,11,13,14)]
```

### Late subgroup - Oral Production
```{r}
summary(data_l_op$language_use)
```
```{r  }
data_l_op$equal.use<-ifelse(data_l_op$language_use=="equal",1,0)
data_l_op$l1.use   <-ifelse(data_l_op$language_use=="L1",1,0)
data_l_op$l2.use   <-ifelse(data_l_op$language_use=="L2",1,0)
```
```{r}
print(m33<-rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),intercept=F,data=data_l_op))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m33$b,m33$ci.lb,m33$ci.ub)),digits=2)
```
Checking whether language use is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(equal.use,l1.use,l2.use),data=data_l_op))[c(8,10,11,13,14)]
```

\newpage

## Linguistic Similarity

## Does 2-level linguistic similarity moderate the difference betwen L1 and L2?

### Whole group - Overlall Performance
```{r}
summary(data$ling_similar_2levels)
```
```{r }
data$similar_binary  <-ifelse(data$ling_similar_2levels=="similar",1,0)
data$different_binary<-ifelse(data$ling_similar_2levels=="different",1,0)
```
```{r}
print(m34<-rma(yi,vi,mods=cbind(similar_binary,different_binary),intercept=F,data=data))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m34$b,m34$ci.lb,m34$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(similar_binary,different_binary),data=data))[c(8,10,11,13,14)]
```

### Whole group - Auditory Comprehension
```{r}
summary(data_ac$ling_similar_2levels)
```
```{r }
data_ac$similar_binary  <-ifelse(data_ac$ling_similar_2levels=="similar",1,0)
data_ac$different_binary<-ifelse(data_ac$ling_similar_2levels=="different",1,0)
```
```{r}
print(m35<-rma(yi,vi,mods=cbind(similar_binary,different_binary),intercept=F,data=data_ac))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m35$b,m35$ci.lb,m35$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(similar_binary,different_binary),data=data_ac))[c(8,10,11,13,14)]
```

### Whole group - Oral Production
```{r}
summary(data_op$ling_similar_2levels)
```
```{r }
data_op$similar_binary  <-ifelse(data_op$ling_similar_2levels=="similar",1,0)
data_op$different_binary<-ifelse(data_op$ling_similar_2levels=="different",1,0)
```
```{r}
print(m36<-rma(yi,vi,mods=cbind(similar_binary,different_binary),intercept=F,data=data_op))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m36$b,m36$ci.lb,m36$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(similar_binary,different_binary),data=data_op))[c(8,10,11,13,14)]
```

### Early subgroup - Overall Performance
```{r}
summary(data_e_overall$ling_similar_2levels)
```
```{r }
data_e_overall$similar_binary  <-ifelse(data_e_overall$ling_similar_2levels=="similar",1,0)
data_e_overall$different_binary<-ifelse(data_e_overall$ling_similar_2levels=="different",1,0)
```
```{r}
print(m37<-rma(yi,vi,mods=cbind(similar_binary,different_binary),intercept=F,data=data_e_overall))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m37$b,m37$ci.lb,m37$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(similar_binary,different_binary),data=data_e_overall))[c(8,10,11,13,14)]
```

### Early subgroup - Auditory Comprehension
```{r}
summary(data_e_ac$ling_similar_2levels)
```
```{r }
data_e_ac$similar_binary  <-ifelse(data_e_ac$ling_similar_2levels=="similar",1,0)
data_e_ac$different_binary<-ifelse(data_e_ac$ling_similar_2levels=="different",1,0)
```
```{r}
print(m38<-rma(yi,vi,mods=cbind(similar_binary,different_binary),intercept=F,data=data_e_ac))
```
```{r }
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m38$b,m38$ci.lb,m38$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(similar_binary,different_binary),data=data_e_ac))[c(8,10,11,13,14)]
```

### Early subgroup - Oral Production
```{r}
summary(data_e_op$ling_similar_2levels)
```
```{r }
data_e_op$similar_binary  <-ifelse(data_e_op$ling_similar_2levels=="similar",1,0)
data_e_op$different_binary<-ifelse(data_e_op$ling_similar_2levels=="different",1,0)
```
```{r}
print(m39<-rma(yi,vi,mods=cbind(similar_binary,different_binary),intercept=F,data=data_e_op))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m39$b,m39$ci.lb,m39$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(similar_binary,different_binary),data=data_e_op))[c(8,10,11,13,14)]
```

### Late subgroup - Overall Performance
```{r}
summary(data_l_overall$ling_similar_2levels)
```
```{r }
data_l_overall$similar_binary  <-ifelse(data_l_overall$ling_similar_2levels=="similar",1,0)
data_l_overall$different_binary<-ifelse(data_l_overall$ling_similar_2levels=="different",1,0)
```
```{r}
print(m40<-rma(yi,vi,mods=cbind(similar_binary,different_binary),intercept=F,data=data_l_overall))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m40$b,m40$ci.lb,m40$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(similar_binary,different_binary),data=data_l_overall))[c(8,10,11,13,14)]
```

### Late subgroup - Auditory Comprehension
```{r}
summary(data_l_ac$ling_similar_2levels)
```
```{r }
data_l_ac$similar_binary  <-ifelse(data_l_ac$ling_similar_2levels=="similar",1,0)
data_l_ac$different_binary<-ifelse(data_l_ac$ling_similar_2levels=="different",1,0)
```
```{r}
print(m41<-rma(yi,vi,mods=cbind(similar_binary,different_binary),intercept=F,data=data_l_ac))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m41$b,m41$ci.lb,m41$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(similar_binary,different_binary),data=data_l_ac))[c(8,10,11,13,14)]
```

### Late subgroup - Oral Production
```{r}
summary(data_l_op$ling_similar_2levels)
```
```{r}
data_l_op$similar_binary  <-ifelse(data_l_op$ling_similar_2levels=="similar",1,0)
data_l_op$different_binary<-ifelse(data_l_op$ling_similar_2levels=="different",1,0)
```
```{r}
print(m42<-rma(yi,vi,mods=cbind(similar_binary,different_binary),intercept=F,data=data_l_op))
```
```{r}
round(exp(c(m42$b,m42$ci.lb,m42$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(similar_binary,different_binary),
                   data=data_l_op))[c(8,10,11,13,14)]
```

\newpage

## Does 3-level linguistic similarity moderate the difference betwen L1 and L2?

### Whole group - Overlall Performance
```{r}
summary(data$ling_similar_3levels)
```
```{r }
data$very_close<-ifelse(data$ling_similar_3levels=="very close",1,0)
data$different <-ifelse(data$ling_similar_3levels=="different",1,0)
data$close     <-ifelse(data$ling_similar_3levels=="close",1,0)
```
```{r}
print(m34<-rma(yi,vi,mods=cbind(very_close,different,close),intercept=F,data=data))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m34$b,m34$ci.lb,m34$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(very_close,different,close),data=data))[c(8,10,11,13,14)]
```

### Whole group - Auditory Comprehension
```{r}
summary(data_ac$ling_similar_3levels)
```
```{r }
data_ac$very_close<-ifelse(data_ac$ling_similar_3levels=="similar",1,0)
data_ac$different <-ifelse(data_ac$ling_similar_3levels=="different",1,0)
data_ac$close     <-ifelse(data_ac$ling_similar_3levels=="close",1,0)
```
```{r}
print(m35<-rma(yi,vi,mods=cbind(very_close,different,close),intercept=F,data=data_ac))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m35$b,m35$ci.lb,m35$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(very_close,different,close),data=data_ac))[c(8,10,11,13,14)]
```

### Whole group - Oral Production
```{r}
summary(data_op$ling_similar_3levels)
```
```{r }
data_op$very_close<-ifelse(data_op$ling_similar_3levels=="similar",1,0)
data_op$different <-ifelse(data_op$ling_similar_3levels=="different",1,0)
data_op$close     <-ifelse(data_op$ling_similar_3levels=="close",1,0)
```
```{r}
print(m36<-rma(yi,vi,mods=cbind(very_close,different,close ),intercept=F,data=data_op))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m36$b,m36$ci.lb,m36$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(very_close,different,close),data=data_op))[c(8,10,11,13,14)]
```

### Early subgroup - Overall Performance
```{r}
summary(data_e_overall$ling_similar_3levels)
```
```{r }
data_e_overall$very_close<-ifelse(data_e_overall$ling_similar_3levels=="similar",1,0)
data_e_overall$different <-ifelse(data_e_overall$ling_similar_3levels=="different",1,0)
data_e_overall$closet_new<-ifelse(data_e_overall$ling_similar_3levels=="close",1,0)
```
```{r}
print(m37<-rma(yi,vi,mods=cbind(very_close,different,closet_new),intercept=F,data=data_e_overall))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m37$b,m37$ci.lb,m37$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(very_close,different,closet_new),data=data_e_overall))[c(8,10,11,13,14)]
```

### Early subgroup - Auditory Comprehension
```{r}
summary(data_e_ac$ling_similar_3levels)
```
```{r }
data_e_ac$very_close  <-ifelse(data_e_ac$ling_similar_3levels=="similar",1,0)
data_e_ac$different<-ifelse(data_e_ac$ling_similar_3levels=="different",1,0)
data_e_ac$close<-ifelse(data_e_ac$ling_similar_3levels=="close",1,0)
```
```{r}
print(m38<-rma(yi,vi,mods=cbind(very_close,different,close),intercept=F,data=data_e_ac))
```
```{r }
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m38$b,m38$ci.lb,m38$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(very_close,different,close),data=data_e_ac))[c(8,10,11,13,14)]
```

### Early subgroup - Oral Production
```{r}
summary(data_e_op$ling_similar_3levels)
```
```{r }
data_e_op$very_close  <-ifelse(data_e_op$ling_similar_3levels=="similar",1,0)
data_e_op$different<-ifelse(data_e_op$ling_similar_3levels=="different",1,0)
data_e_op$close<-ifelse(data_e_op$ling_similar_3levels=="close",1,0)
```
```{r}
print(m39<-rma(yi,vi,mods=cbind(very_close,different,close),intercept=F,data=data_e_op))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m39$b,m39$ci.lb,m39$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(very_close,different,close),data=data_e_op))[c(8,10,11,13,14)]
```

### Late subgroup - Overall Performance
```{r}
summary(data_l_overall$ling_similar_3levels)
```
```{r }
data_l_overall$very_close  <-ifelse(data_l_overall$ling_similar_3levels=="similar",1,0)
data_l_overall$different<-ifelse(data_l_overall$ling_similar_3levels=="different",1,0)
data_l_overall$close<-ifelse(data_l_overall$ling_similar_3levels=="close",1,0)
```
```{r}
print(m40<-rma(yi,vi,mods=cbind(very_close,different,close),intercept=F,data=data_l_overall))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m40$b,m40$ci.lb,m40$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(very_close,different,close),data=data_l_overall))[c(8,10,11,13,14)]
```

### Late subgroup - Auditory Comprehension
```{r}
summary(data_l_ac$ling_similar_3levels)
```
```{r }
data_l_ac$very_close  <-ifelse(data_l_ac$ling_similar_3levels=="similar",1,0)
data_l_ac$different<-ifelse(data_l_ac$ling_similar_3levels=="different",1,0)
data_l_ac$close<-ifelse(data_l_ac$ling_similar_3levels=="close",1,0)
```
```{r}
print(m41<-rma(yi,vi,mods=cbind(very_close,different,close),intercept=F,data=data_l_ac))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m41$b,m41$ci.lb,m41$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(very_close,different,close),data=data_l_ac))[c(8,10,11,13,14)]
```

### Late subgroup - Oral Production
```{r}
summary(data_l_op$ling_similar_3levels)
```
```{r}
data_l_op$very_close  <-ifelse(data_l_op$ling_similar_3levels=="similar",1,0)
data_l_op$different<-ifelse(data_l_op$ling_similar_3levels=="different",1,0)
data_l_op$close<-ifelse(data_l_op$ling_similar_3levels=="close",1,0)
```
```{r}
print(m42<-rma(yi,vi,mods=cbind(very_close,different,close),intercept=F,
                 data=data_l_op))
```
```{r}
round(exp(c(m42$b,m42$ci.lb,m42$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(very_close,different,close),data=data_l_op))[c(8,10,11,13,14)]
```
\newpage

# Additional Variables

## Does age at the time of assessment moderate the difference betwen L1 and L2?

### Whole group - Overall Performance
```{r }
summary(data$case_age)
```
```{r}
print(mpo_all_overall<-rma(yi,vi,mods=case_age,data=data))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_all_overall$b,mpo_all_overall$ci.lb,mpo_all_overall$ci.ub)),digits=2)
```

### Whole group - Auditory Comprehension
```{r }
summary(data_ac$case_age)
```
```{r}
print(mpo_all_ac<-rma(yi,vi,mods=case_age,data=data_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_all_ac$b,mpo_all_ac$ci.lb,mpo_all_ac$ci.ub)),digits=2)
```

#### Whole group - Oral Production
```{r }
summary(data_op$case_age)
```
```{r}
print(mpo_all_op<-rma(yi,vi,mods=case_age,data=data_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_all_op$b,mpo_all_op$ci.lb,mpo_all_op$ci.ub)),digits=2)
```

### Early subgroup - Overearly Performance
```{r }
summary(data_e_overall$case_age)
```
```{r}
print(mpo_early_overall<-rma(yi,vi,mods=case_age,data=data_e_overall))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_early_overall$b,mpo_early_overall$ci.lb,mpo_early_overall$ci.ub)),digits=2)
```

### Early subgroup - Auditory Comprehension
```{r }
summary(data_e_ac$case_age)
```
```{r}
print(mpo_early_ac<-rma(yi,vi,mods=case_age,data=data_e_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_early_ac$b,mpo_early_ac$ci.lb,mpo_early_ac$ci.ub)),digits=2)
```

#### Early subgroup - Oral Production
```{r }
summary(data_e_op$case_age)
```
```{r}
print(mpo_early_op<-rma(yi,vi,mods=case_age,data=data_e_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_early_op$b,mpo_early_op$ci.lb,mpo_early_op$ci.ub)),digits=2)
```

### Late subgroup - Overlate Performance
```{r }
summary(data_l_overall$case_age)
```
```{r}
print(mpo_late_overall<-rma(yi,vi,mods=case_age,data=data_l_overall))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_late_overall$b,mpo_late_overall$ci.lb,mpo_late_overall$ci.ub)),digits=2)
```

### Late subgroup - Auditory Comprehension
```{r }
summary(data_l_ac$case_age)
```
```{r}
print(mpo_late_ac<-rma(yi,vi,mods=case_age,data=data_l_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_late_ac$b,mpo_late_ac$ci.lb,mpo_late_ac$ci.ub)),digits=2)
```

#### Late subgroup - Oral Production
```{r }
summary(data_l_op$case_age)
```
```{r}
print(mpo_late_op<-rma(yi,vi,mods=case_age,data=data_l_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_late_op$b,mpo_late_op$ci.lb,mpo_late_op$ci.ub)),digits=2)
```

\newpage

## Do years of education moderate the difference betwen L1 and L2?

### Whole group - Overall Performance 
```{r }
summary(data$case_scholarity_years)
```
```{r}
print(education_all_overall<-rma(yi,vi,mods=case_scholarity_years,data=data))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(education_all_overall$b,education_all_overall$ci.lb,education_all_overall$ci.ub)),digits=2)
```

### Whole group - Auditory Comprehension
```{r }
summary(data_ac$case_scholarity_years)
```
```{r}
print(education_all_ac<-rma(yi,vi,mods=case_scholarity_years,data=data_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(education_all_ac$b,education_all_ac$ci.lb,education_all_ac$ci.ub)),digits=2)
```

### Whole group - Oral Production
```{r }
summary(data_op$case_scholarity_years)
```
```{r}
print(education_all_op<-rma(yi,vi,mods=case_scholarity_years,data=data_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(education_all_op$b,education_all_op$ci.lb,education_all_op$ci.ub)),digits=2)
```

### Early subgroup - Overall Performance
```{r }
summary(data_e_overall$case_scholarity_years)
```
```{r}
print(education_early_overall<-rma(yi,vi,mods=case_scholarity_years,data=data_e_overall))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(education_early_overall$b,education_early_overall$ci.lb,education_early_overall$ci.ub)),digits=2)
```

### Early subgroup - Auditory Comprehension
```{r }
summary(data_e_ac$case_scholarity_years)
```
```{r}
print(education_early_ac<-rma(yi,vi,mods=case_scholarity_years,data=data_e_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(education_early_ac$b,education_early_ac$ci.lb,education_early_ac$ci.ub)),digits=2)
```

### Early subgroup - Oral Production
```{r }
summary(data_e_op$case_scholarity_years)
```
```{r}
print(education_early_op<-rma(yi,vi,mods=case_scholarity_years,data=data_e_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(education_early_op$b,education_early_op$ci.lb,education_early_op$ci.ub)),digits=2)
```

### Late subgroup - Overall Performance
```{r }
summary(data_l_overall$case_scholarity_years)
```
```{r}
print(education_late_overall<-rma(yi,vi,mods=case_scholarity_years,data=data_l_overall))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(education_late_overall$b,education_late_overall$ci.lb,education_late_overall$ci.ub)),digits=2)
```

### Late subgroup - Auditory Comprehension
```{r }
summary(data_l_ac$case_scholarity_years)
```
```{r}
print(education_late_ac<-rma(yi,vi,mods=case_scholarity_years,data=data_l_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(education_late_ac$b,education_late_ac$ci.lb,education_late_ac$ci.ub)),digits=2)
```

### Late subgroup - Oral Production
```{r }
summary(data_l_op$case_scholarity_years)
```
```{r}
print(education_late_op<-rma(yi,vi,mods=case_scholarity_years,data=data_l_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(education_late_op$b,education_late_op$ci.lb,education_late_op$ci.ub)),digits=2)
```

\newpage

## Do months post onset moderate the difference betwen L1 and L2?

### Whole group - Overall Performance
```{r }
summary(data$case_mpo)
```
```{r}
print(mpo_all_overall<-rma(yi,vi,mods=case_mpo,data=data))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_all_overall$b,mpo_all_overall$ci.lb,mpo_all_overall$ci.ub)),digits=2)
```

### Whole group - Auditory Comprehension
```{r }
summary(data_ac$case_mpo)
```
```{r}
print(mpo_all_ac<-rma(yi,vi,mods=case_mpo,data=data_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_all_ac$b,mpo_all_ac$ci.lb,mpo_all_ac$ci.ub)),digits=2)
```

#### Whole group - Oral Production
```{r }
summary(data_op$case_mpo)
```
```{r}
print(mpo_all_op<-rma(yi,vi,mods=case_mpo,data=data_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_all_op$b,mpo_all_op$ci.lb,mpo_all_op$ci.ub)),digits=2)
```

### Early subgroup - Overearly Performance
```{r }
summary(data_e_overall$case_mpo)
```
```{r}
print(mpo_early_overall<-rma(yi,vi,mods=case_mpo,data=data_e_overall))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_early_overall$b,mpo_early_overall$ci.lb,mpo_early_overall$ci.ub)),digits=2)
```

### Early subgroup - Auditory Comprehension
```{r }
summary(data_e_ac$case_mpo)
```
```{r}
print(mpo_early_ac<-rma(yi,vi,mods=case_mpo,data=data_e_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_early_ac$b,mpo_early_ac$ci.lb,mpo_early_ac$ci.ub)),digits=2)
```

#### Early subgroup - Oral Production
```{r }
summary(data_e_op$case_mpo)
```
```{r}
print(mpo_early_op<-rma(yi,vi,mods=case_mpo,data=data_e_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_early_op$b,mpo_early_op$ci.lb,mpo_early_op$ci.ub)),digits=2)
```

### Late subgroup - Overlate Performance
```{r }
summary(data_l_overall$case_mpo)
```
```{r}
print(mpo_late_overall<-rma(yi,vi,mods=case_mpo,data=data_l_overall))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_late_overall$b,mpo_late_overall$ci.lb,mpo_late_overall$ci.ub)),digits=2)
```

### Late subgroup - Auditory Comprehension
```{r }
summary(data_l_ac$case_mpo)
```
```{r}
print(mpo_late_ac<-rma(yi,vi,mods=case_mpo,data=data_l_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_late_ac$b,mpo_late_ac$ci.lb,mpo_late_ac$ci.ub)),digits=2)
```

#### Late subgroup - Oral Production
```{r }
summary(data_l_op$case_mpo)
```
```{r}
print(mpo_late_op<-rma(yi,vi,mods=case_mpo,data=data_l_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(mpo_late_op$b,mpo_late_op$ci.lb,mpo_late_op$ci.ub)),digits=2)
```

\newpage

## Does gender moderate the difference betwen L1 and L2?

### Whole group - Overlall Performance
```{r}
summary(data$case_gender)
```
```{r }
data$male  <-ifelse(data$case_gender=="m",1,0)
data$female<-ifelse(data$case_gender=="f",1,0)
```
```{r}
print(m_gender<-rma(yi,vi,mods=cbind(male,female),intercept=F,data=data))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m_gender$b,m_gender$ci.lb,m_gender$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(male,female),data=data))[c(8,10,11,13,14)]
```

### Whole group - Auditory Comprehension
```{r}
summary(data_ac$case_gender)
```
```{r }
data_ac$male  <-ifelse(data_ac$case_gender=="m",1,0)
data_ac$female<-ifelse(data_ac$case_gender=="f",1,0)
```
```{r}
print(m35<-rma(yi,vi,mods=cbind(male,female),intercept=F,data=data_ac))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m35$b,m35$ci.lb,m35$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(male,female),data=data_ac))[c(8,10,11,13,14)]
```

### Whole group - Oral Production
```{r}
summary(data_op$case_gender)
```
```{r }
data_op$male  <-ifelse(data_op$case_gender=="m",1,0)
data_op$female<-ifelse(data_op$case_gender=="f",1,0)
```
```{r}
print(m36<-rma(yi,vi,mods=cbind(male,female),intercept=F,data=data_op))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m36$b,m36$ci.lb,m36$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(male,female),data=data_op))[c(8,10,11,13,14)]
```

### Early subgroup - Overall Performance
```{r}
summary(data_e_overall$case_gender)
```
```{r }
data_e_overall$male  <-ifelse(data_e_overall$case_gender=="m",1,0)
data_e_overall$female<-ifelse(data_e_overall$case_gender=="f",1,0)
```
```{r}
print(m37<-rma(yi,vi,mods=cbind(male,female),intercept=F,data=data_e_overall))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m37$b,m37$ci.lb,m37$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(male,female),data=data_e_overall))[c(8,10,11,13,14)]
```

### Early subgroup - Auditory Comprehension
```{r}
summary(data_e_ac$case_gender)
```
```{r }
data_e_ac$male  <-ifelse(data_e_ac$case_gender=="m",1,0)
data_e_ac$female<-ifelse(data_e_ac$case_gender=="f",1,0)
```
```{r}
print(m38<-rma(yi,vi,mods=cbind(male,female),intercept=F,data=data_e_ac))
```
```{r }
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m38$b,m38$ci.lb,m38$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(male,female),data=data_e_ac))[c(8,10,11,13,14)]
```

### Early subgroup - Oral Production
```{r}
summary(data_e_op$case_gender)
```
```{r }
data_e_op$male  <-ifelse(data_e_op$case_gender=="m",1,0)
data_e_op$female<-ifelse(data_e_op$case_gender=="f",1,0)
```
```{r}
print(m39<-rma(yi,vi,mods=cbind(male,female),intercept=F,data=data_e_op))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m39$b,m39$ci.lb,m39$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(male,female),data=data_e_op))[c(8,10,11,13,14)]
```

### Late subgroup - Overall Performance
```{r}
summary(data_l_overall$case_gender)
```
```{r }
data_l_overall$male  <-ifelse(data_l_overall$case_gender=="m",1,0)
data_l_overall$female<-ifelse(data_l_overall$case_gender=="f",1,0)
```
```{r}
print(m40<-rma(yi,vi,mods=cbind(male,female),intercept=F,data=data_l_overall))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m40$b,m40$ci.lb,m40$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(male,female),data=data_l_overall))[c(8,10,11,13,14)]
```

### Late subgroup - Auditory Comprehension
```{r}
summary(data_l_ac$case_gender)
```
```{r }
data_l_ac$male  <-ifelse(data_l_ac$case_gender=="m",1,0)
data_l_ac$female<-ifelse(data_l_ac$case_gender=="f",1,0)
```
```{r}
print(m41<-rma(yi,vi,mods=cbind(male,female),intercept=F,data=data_l_ac))
```
```{r}
#RR estimate  lower and upper boundaries of the 95% CI
round(exp(c(m41$b,m41$ci.lb,m41$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator.
```{r}
capture.output(rma(yi,vi,mods=cbind(male,female),data=data_l_ac))[c(8,10,11,13,14)]
```

### Late subgroup - Oral Production
```{r}
summary(data_l_op$case_gender)
```
```{r}
data_l_op$male  <-ifelse(data_l_op$case_gender=="m",1,0)
data_l_op$female<-ifelse(data_l_op$case_gender=="f",1,0)
```
```{r}
print(m42<-rma(yi,vi,mods=cbind(male,female),intercept=F,data=data_l_op))
```
```{r}
round(exp(c(m42$b,m42$ci.lb,m42$ci.ub)),digits=2)
```
Checking whether linguistic similarity is a significant moderator. 
```{r}
capture.output(rma(yi,vi,mods=cbind(male,female),data=data_l_op))[c(8,10,11,13,14)]
```
\newpage

## Does the research question type moderate the difference betwen L1 and L2??

### Overall Performance - Whole group
```{r }
data$rq_rel  <-ifelse(data$study_rq=="yes"|data$study_rq=="yes_rep",1,0)
data$rq_unrel<-ifelse(data$study_rq=="no"|data$study_rq=="no_rep",1,0)
```
```{r}
print(c(sum(data$rq_rel=="1"),sum(data$rq_unrel=="1")))
```
```{r }
print(m_rq_overall<-rma(yi,vi,mods=cbind(rq_rel,rq_unrel),intercept=F,data=data))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m_rq_overall$b,m_rq_overall$ci.lb,m_rq_overall$ci.ub)),digits=2)
```
Checking whether the research question is a significant moderator. 
```{r }
capture.output(rma(yi,vi,mods=cbind(rq_rel,rq_unrel),data=data))[c(8,10,11,13,14)]
```

### Auditory comprehension - Whole group 
```{r }
data_ac$rq_rel  <-ifelse(data_ac$study_rq=="yes"|data_ac$study_rq=="yes_rep",1,0)
data_ac$rq_unrel<-ifelse(data_ac$study_rq=="no"|data_ac$study_rq=="no_rep",1,0)
```
```{r}
print(c(sum(data_ac$rq_rel=="1"), sum(data_ac$rq_unrel=="1")))
```
```{r }
print(m_rq_ac<-rma(yi,vi,mods=cbind(rq_rel,rq_unrel),intercept=F,data=data_ac))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m_rq_ac$b,m_rq_ac$ci.lb,m_rq_ac$ci.ub)),digits=2)
```
Checking whether the research question is a significant moderator.  
```{r }
capture.output(rma(yi,vi,mods=cbind(rq_rel ,rq_unrel),data=data_ac))[c(8,10,11,13,14)]
```

### Oral production - Whole group 
```{r }
data_op$rq_rel  <-ifelse(data_op$study_rq=="yes"|data_op$study_rq=="yes_rep",1,0)
data_op$rq_unrel<-ifelse(data_op$study_rq=="no"|data_op$study_rq=="no_rep",1,0)
```
```{r}
print(c(sum(data_op$rq_rel=="1"), sum(data_op$rq_unrel=="1")))
```
```{r }
print(m_rq_oral<-rma(yi,vi,mods=cbind(rq_rel,rq_unrel),intercept=F,data=data_op))
```
```{r}
#RR estimate,lower and upper boundaries of the 95% CI
round(exp(c(m_rq_oral$b,m_rq_oral$ci.lb,m_rq_oral$ci.ub)),digits=2)
```
Checking whether the research question is a significant moderator. 
```{r }
capture.output(rma(yi,vi,mods=cbind(rq_rel ,rq_unrel),data=data_op))[c(8,10,11,13,14)]
```
\center
