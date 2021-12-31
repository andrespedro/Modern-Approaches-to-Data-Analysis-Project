#Andres Pedro
#Final Exam

#Part 2 Netflix data
library(tidyverse)
library(ggcorrplot)
library(ggplot2)
library(tibble)
library(caret)
library(leaps)
library(gt)
library(glmnet)
library(tidyverse)   # pipe operator
library(magrittr)    # set rownames, colnames
library(DT)  
library(readr)
library(factoextra)  # pca biplot

#Ntflx<-read.csv("Netflix.csv")
N<-read.csv("Netflix.csv",sep=",",dec=".")
N<-na.omit(N)# We will remove data with NA in rows
N$Boxoffice<-as.integer(gsub("[$,]","",N$Boxoffice))
N$Genre<-as.factor(gsub("[,]","",N$Genre))
#This works
N<-N[order(-N$Boxoffice),]

N2<-N[,c(-29,-28,-27,-26,-24,-23,-22,-3)]
N2<-N2[order(-N2$Boxoffice),]
datatable(N2)

nc<-N2[,c(-1,-2,-3,-4,-6,-7,-8,-9,-10,-11,-18,-19,-20),]
#nc$Hidden.Gem.Score<-as.integer(nc$Hidden.Gem.Score)
#nc$IMDb.Score<-as.integer(nc$IMDb.Score)
#ggcorrplot(cr,method="circle")

#nc$Rotten.Tomatoes.Score<-as.numeric(nc$Rotten.Tomatoes.Score)
#nc$Metacritic.Score<-as.numeric(nc$Metacritic.Score)
#nc$Awards.Received<-as.numeric(nc$Awards.Received)
#nc$Awards.Nominated.For<-as.numeric(nc$Awards.Nominated.For)
#nc$Boxoffice<-as.numeric(nc$Boxoffice)
#nc$IMDb.Votes<-as.numeric(nc$IMDb.Votes)
cr=cor(nc)
ggcorrplot(cr,method="circle")
#cat<-unique(N$Genre)
#Part a
#filter using reg exp
ntflx_movies<-N%>%
  select(1,2,3,5,6,13,14,15,16,17,18,25)%>%
  filter(N$Series.or.Movie=="Movie")

#genres<-c('Action','Adult', 'Crime','Music','Musical','Sci-Fi','War','Thriller','Horror','Fantasy','Drama','Western','Mystery','Family','Romance','Sport','History',
#          'Talk-Show','Comedy','Adventure','News','Reality-TV','Animation','Biography','Short')

#ntflx_movies$Genre<-as.factor(ntflx_movies$Genre)

ntflx_movies$Boxoffice<-as.numeric(ntflx_movies$Boxoffice)
nm2<-subset(ntflx_movies,Boxoffice>300000000)#Only checking 100,000,000 conditioned rows
p<-ggplot(nm2, aes(x=Genre,y=Boxoffice))+
  geom_bar(stat="identity",color="blue")+
  theme(axis.text.x=element_text(angle=45,size=10,hjust=1))
p


#p+theme_grey(base_size=28)




#Part b
#Let us filter it to the top 25%
nm3<-subset(ntflx_movies,IMDb.Votes>600000)
p2<-ggplot(nm3, aes(x=Genre,y=IMDb.Votes))+
  geom_bar(stat="identity",color="blue")+
  theme(axis.text.x=element_text(angle=45,size=10,hjust=1))
p2
#We filtered down our barplot to genres that obtained a cumultative score of over 600,000 votes.

#Part c
nm4<-subset(N, Metacritic.Score>88)
p3<-ggplot(nm4, aes(x=Genre,y=Metacritic.Score))+
  geom_bar(stat="identity",color="blue")+
  geom_col(position=position_dodge((0)))+
  theme(axis.text.x=element_text(angle=45,size=10,hjust=1))
p3









##Predictive Model e
N2<-N%>%
  select(2,4,5,6,7,8,9,10,12,13,14,15,16,17,18,21,25)

N2$Genre<-as.factor(N2$Genre)
N2$Languages<-as.factor(N2$Languages)
N2$Series.or.Movie<-as.factor(N2$Series.or.Movie)
N2$Country.Availability<-as.factor(N2$Country.Availability)
N2$Runtime<-as.factor(N2$Runtime)
N2$Director<-as.factor(N2$Director)
N2$Writer<-as.factor(N2$Writer)
N2$View.Rating<-as.factor(N2$View.Rating)
N2$Production.House<-as.factor(N2$Production.House)

set.seed(42)
#80/20 split
nc_idx = sample(nrow(N2), 2138, replace = F)
nc_trn = nc[nc_idx, ]
nc_tst = nc[-nc_idx, ]
y<-nc_tst$Hidden.Gem.Score
n_pred<-lm(Hidden.Gem.Score~.,data=nc_trn)
summary(n_pred)
pred<-predict(n_pred,newdata=nc_tst,type='response')
plot(y,pred)
# Our plot works! Doesn't seem to be all that accurate though

#Let us try Ridge regression!
N3<-N2%>%
  select(3,4,6,10,11,12,13,14,15,17)
x=model.matrix(Hidden.Gem.Score~.,data=N3)
y=N3$Hidden.Gem.Score
set.seed(1234)
#index1
index1=seq(1,2138,1)
t=sample(index1,2138,replace = F)
train <- N2[t,]
test<-N2[-t,]
y.test=N2$Hidden.Gem.Score[-t]
#Ridge lambda
cv.out=cv.glmnet(x[t,],y[t],alpha=0)
best.lam=cv.out$lambda.min
best.lam
ridge.c=glmnet(x,y,alpha=0)
rp<-predict(ridge.c,x[-t,],type="response",s=best.lambda)
plot(y.test,rp)
#-------------------------------------------------------------#

#--------------------------------------------#
#Problem f Among the movies, variables that influence the box office more?
#Perfect for PCA
N_pca<-ntflx_movies[,c(-1,-2,-3,-4)]
N_pca<-na.omit(N_pca)


#we Want to compare the results to the built-in function?
pca_result<-prcomp(N_pca,scale=TRUE)
fviz_pca_biplot(
  pca_result, 
  axes = c(3, 4),              # choose two PC's to plot
  geom = c("point", "text"),   # plot states as point, text labels, or both
  labelsize = 3,               # label font size 
  pointsize = 0.7,             # point size for states
  col.ind = "grey70",          # point and label color for states
  col.var = "contrib",         # use variable contributions to PC as legend 
  gradient.cols = c("steelblue", "#E7B800", "red")    # color for variable contrib
) +
  xlim(c(-3.1,3.1)) + ylim(c(-2.5,2.5))

#Interesting finds; however, it seems like  it doesn't
#provide enough information and we still don't know what influences
#Boxoffice. It could be genres. Maybe producers?

#Maybe we can solve this using lasso regression since
#it applies model selection

#-------------------------------------#
#Problem g
set.seed(42)
#80/20 split
nc_idx = sample(nrow(N_pca), 2138, replace = F)
nc_trn = N_pca[nc_idx, ]
nc_tst = N_pca[-nc_idx, ]
y<-nc_tst$Hidden.Gem.Score
n_pred2<-lm(Boxoffice~.,data=nc_trn)
pred<-predict(n_pred,newdata=nc_tst,type='response')
plot(y,pred)



#If time, let us attempt ridge regression
N_pca$Boxoffice<-as.integer(N_pca$Boxoffice)
x=model.matrix(Boxoffice~.,N_pca)
y=N_pca$Boxoffice
set.seed(1)
idx = sample(nrow(N_pca), 2138, replace = F)
trn = N_pca[idx, ]
tst = N_pca[-idx, ]
y.test=y[-idx]
ridge.mod=glmnet(x[trn ,],y[trn],alpha=0)

cv.out=cv.glmnet(x[train ,],y[ train],alpha=0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam

ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])

#---------------------------------------------------#
#Problem h
library(mclust)
mc.ntflx=Mclust(N_pca)
plot(mc.ntflx,what="classification")
