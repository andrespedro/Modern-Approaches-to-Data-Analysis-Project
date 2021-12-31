
#c<-read.delim("Cereal.txt",header=TRUE,sep="\t",dec=".")

#Andres Pedro
#This Works!!!!!
library(ggcorrplot)
library(tibble)
library(ggplot2)     # plots
library(tidyverse)   # pipe operator
library(magrittr)    # set rownames, colnames
library(DT)
library(factoextra)  # pca biplot
library(leaps)
c<-read.csv2("cereal.csv",sep=";",dec=",")
#There are 78 records because the first record
#contains our data types

#really there are 77
c2<-c[-1,]

c2$mfr<-as.factor(c2$mfr)
c2$type<-as.factor(c2$type)
c2$calories<-as.numeric(c2$calories)
c2$protein<-as.numeric(c2$protein)
c2$fat<-as.numeric(c2$fat)
c2$sodium<-as.numeric(c2$sodium)
c2$fiber<-as.numeric(c2$fiber)
c2$carbo<-as.numeric(c2$carbo)
c2$sugars<-as.numeric(c2$sugars)
c2$potass<-as.numeric(c2$potass)
c2$vitamins<-as.numeric(c2$vitamins)
c2$shelf<-as.numeric(c2$shelf)
c2$weight<-as.numeric(c2$weight)
c2$cups<-as.numeric(c2$cups)
c2$rating<-as.numeric(c2$rating)

as_tibble(c2)
cc<-c2[,c(-1,-2,-3)]
cr=cor(cc)
ggcorrplot(cr,method="circle")



#We see there is heavy correlation between fiber and potass. some correlation between
#calories and weight with little correlation between rating and fiber
cereal.lm<-lm(rating~.,data=cc)
summary(cereal.lm)
plot(cereal.lm)
lm1<-lm(rating~.-name-mfr-type-cups-weight-shelf,data=c2)
summary(lm1)
rr<-regsubsets(rating~.-name-mfr,data=c2,nvmax=13,method="exhaust")
sum1<-summary(rr)
sum1
sum1$bic
min(sum1$bic)
#Observations 71, 26 and 18 might be high leverage outliers


p<-ggplot(c2,aes(x=name,y=rating))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
p
#The highest rated cereal is all-bran with extra fiber?1
#That's crazy
#It also the highest amount of fiber

#Let us check the association between continuous var's against ratings

#data.frame(cc)
#as_tibble(cc)
c3<-column_to_rownames(c,var="name")
c3<-c3[-1,c(-1,-2)]
c3$calories<-as.numeric(c3$calories)
c3$protein<-as.numeric(c3$protein)
c3$fat<-as.numeric(c3$fat)
c3$sodium<-as.numeric(c3$sodium)
c3$fiber<-as.numeric(c3$fiber)
c3$carbo<-as.numeric(c3$carbo)
c3$sugars<-as.numeric(c3$sugars)
c3$potass<-as.numeric(c3$potass)
c3$vitamins<-as.numeric(c3$vitamins)
c3$shelf<-as.numeric(c3$shelf)
c3$weight<-as.numeric(c3$weight)
c3$cups<-as.numeric(c3$cups)
c3$rating<-as.numeric(c3$rating)
c3
datatable(c3, options = list(pageLength = 10))
#Before applying PCA, it's Important to scale/normalize the data
#df<-data.frame(C$X)
scaled_df <- 
  apply(c3, 2, scale) %>% 
  set_colnames(paste0(names(c3), "_scaled")) %>% 
  set_rownames(rownames(c3))

datatable(round(scaled_df, 4), options = list(pageLength = 10))

covariance_matrix<-cov(scaled_df)
Eigen<-eigen(covariance_matrix)
Eigen

#We will compute the PC scores
pc_df<-
  as.matrix(scaled_df)%*% Eigen$vectors%>%
  data.frame() %>%
  set_colnames(paste0("PC",1:ncol(scaled_df)))

datatable(round(pc_df,4), options=list(pageLength=10))

#we Want to compare the results to the built-in function?
pca_result<-prcomp(c3,scale=TRUE)
datatable(round(pca_result$x,4),options=list(pageLength=10))


# rotate 180 degree
fviz_pca_biplot(
  pca_result, 
  axes = c(1, 2),              
  geom = c("point", "text"),  
  labelsize = 3,               
  pointsize = 0.7,             
  col.ind = "grey70",          
  col.var = "contrib",         
  gradient.cols = c("navyblue", "orange", "red4")    
) +
  xlim(c(-3.1,3.1)) + ylim(c(-2.5,2.5))      # rotate the biplot
#Seems like cereals with higher vitamins tend to have higher sodium
#It makes sense that cereals with more cups tend to have more carbs.



fviz_pca_biplot(
  pca_result, 
  axes = c(5, 6),              
  geom = c("point", "text"),  
  labelsize = 3,               
  pointsize = 0.7,             
  col.ind = "grey70",          
  col.var = "contrib",         
  gradient.cols = c("navyblue", "orange", "red4")    
) +
  xlim(c(-3.1,3.1)) + ylim(c(-2.5,2.5))

#Based on plot2 Dim3 and 4
#We can see cereals high in fat tend to have way less vitamins

#Cereals with higher carbo tend to have
#Higher ratings, weight, potassium and some sodium.

#There also seems to be an indication that cereal with high sugars
#tend to have lower ratings?

p<-ggplot(c2,aes(x=name,y=sugars))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
p

p<-ggplot(c2,aes(x=name,y=carbo))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
p
#Recall cereals with value -1 means Not available?

# We want to create a datatable and look at the highest fiber,carbs and vitamins
data.frame(c3)
c3<-c3[order(-c3$fiber,-c3$carbo),]
datatable(c3)
c3<-c3[order(-c3$vitamins),]
datatable(c3)
c3<-c3[order(-c3$carbo),]
datatable(c3)
#Carbs don't really seem to be correlated with rating
#Finally,
c2<-c2[order(-c2$rating),]
datatable(c2)
mean(c3$protein)
mean(c3$fi)

#One thing we can see from our table is that Nabisco seems to have higher rated
#cereals and they tend to have no sugar. The highest rated also seem to have the least amount of sugar.
# 100% vitamins cereal do not have the highest score. It really varies

#This could potentially be a great visualization. Kellogs All bran with extra fiber is obviously an outlier
b<-ggplot(c2,aes(x=mfr,y=rating))+
  geom_boxplot()
b + stat_summary(fun=mean, geom="point", shape=23, size=4)

b<-ggplot(c2,aes(x=mfr,y=calories))+
  geom_boxplot()
b + stat_summary(fun=mean, geom="point", shape=23, size=4)

b<-ggplot(c2,aes(x=mfr,y=sugars))+
  geom_boxplot()
b + stat_summary(fun=mean, geom="point", shape=23, size=4)

b<-ggplot(c2,aes(x=mfr,y=sodium))+
  geom_boxplot()
b + stat_summary(fun=mean, geom="point", shape=23, size=4)

b<-ggplot(c2,aes(x=mfr,y=carbo))+
  geom_boxplot()
b + stat_summary(fun=mean, geom="point", shape=23, size=4)

b<-ggplot(c2,aes(x=mfr,y=vitamins))+
  geom_boxplot()
b + stat_summary(fun=mean, geom="point", shape=23, size=4)

#Categorical variables now
c2$shelf<-as.factor(c2$shelf)
b<-ggplot(c2,aes(x=mfr,y=shelf,fill=shelf))+
  geom_bar(stat="identity")
b
