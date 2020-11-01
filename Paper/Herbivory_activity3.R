###Dateset 
https://docs.google.com/spreadsheets/d/1ftlxC3Ms6x11HnxCO0FuarwZEnJYdRL2oXaLCNrug6Q/edit#gid=959908961
library(tidyverse)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(ggpmisc)

update.packages("rlang")

getwd()
setwd("~/Desktop/Ecology lab/Paper")
rm(list = ls(all = TRUE))

dat <- read.csv( "ENY6203 Herbivory Data.csv")
## calculate mean % damage on the 10 leaves
dat$percLfMean <- rowMeans(dat[,10:19], na.rm=T)   

X<-dat
X<-X[1:297,]

meta<-read.csv("metadate.csv")


new.dat<-merge(X,meta,by.x="surveyID")
X<-new.dat
X<-X[!is.na(X$percLfMean),]

view(X)
##
genus_perc<-ggplot(aes(Plant_gens,percLfMean),data=X)+
geom_point(aes(color=Plant_gens),position=position_jitter(width=0.1,height=0),
           alpha=1,size=2)+
    theme_classic2()+
    theme(legend.position = "None")+
    stat_pvalue_manual(stat.test_1,label="p.adj.signif",tip.length = 0,size = 6,hide.ns = T)+
    labs(x="Plant genus",y="% of leaf damage")+
    stat_summary(fun=mean,geom="crossbar",width=0.5,aes(color=Plant_gens),data=X)

genus_perc


stat.test_1<-X %>%
    tukey_hsd(percLfMean~Plant_gens) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance() %>%
    add_xy_position(x="Plant_gens",y.trans=function(x) {x/8+20})


genus_perc


aggregate(X$percLfMean,list(X$Plant_gens),mean,na.rm=TRUE)

aggregate(X$percLfMean,list(X$Plant_gens),max,na.rm=TRUE)
aggregate(X$percLfMean,list(X$Plant_gens),min,na.rm=TRUE)



## Does damage vary by height? Across species 

## remove Litchi and Salix

X2<- X[X$Plant_gens!="Litchi"&X$Plant_gens!="Salix", ]


model1<-glm(percLfMean~ht_cm,X2,family =gaussian(link = "identity"))
model2<-lme4::lmer(percLfMean~ht_cm+(1|Plant_gens),X2)



library(emmeans)

Anova(model1)  ### P=0.57
summary(model1) ### AIC=1491.6

Anova(model2)  ### 'P=0.07
summary(model2)
AIC(model2)  ##AIC=1460.81

X$Plant_gens
p<-ggplot(X2, aes(x= ht_cm , y = percLfMean)) + geom_point(aes(color=Plant_gens))+
    geom_smooth(method = "glm")+
    theme_classic()+
    labs(x="Plant height (cm)",y="% of leaf damage",color="Genus")+
    annotate("text",label = "GLMM,P=0.07,AIC=1460.81 \nGLM(gaussian),P=0.57,AIC=1491.6",x=100,y=40)

p

#Within species 
install.packages("ggpmisc")
library(ggpmisc)
library(ggplot2)
unloadNamespace("rlang")





packageVersion("ggplot2")

ggplot(X, aes(x= ht_cm , y = percLfMean,color=site)) + geom_point()+
    facet_wrap(~Plant_gens,nrow = 2)+
    geom_point()+
    theme_classic()+
    labs(x="Plant height (cm)",y="% of leaf damage")+
    geom_line(data=cbind(X,pred=predict(model2)),aes(y=pred))

View(X)

model2
p<-ggplot(X, aes(x= ht_cm , y = percLfMean)) + geom_point(aes(color=site))+
    facet_wrap(~Plant_gens,nrow = 2)+
    theme_classic()+
    labs(x="Plant height (cm)",y="% of leaf damage")+
    geom_smooth(method = "lm")+
    stat_poly_eq(formula = y~x, 
                 aes(label = paste( ..rr.label..,..p.value.label..,sep = "~~~")), 
                 parse = TRUE,size=2)
    
p


tiff(filename = "GLMM(figure3.2).tiff",
     width = 6, height = 6, units = "in", res=300,
     bg = "black")
plot(p)
dev.off()




