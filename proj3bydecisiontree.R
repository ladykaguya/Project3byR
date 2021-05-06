library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)
setwd("/Users/mrunalsawant/Desktop/Project/Project 3/")

rg_train=read.csv("product_train.csv",stringsAsFactors = FALSE)
rg_test=read.csv("product_test.csv",stringsAsFactors = FALSE)

rg_test$went_on_backorder=NA
rg_train$data='train'
rg_test$data='test'

rg=rbind(rg_train,rg_test)

library(dplyr)
rg=rg%>%
  mutate(went_on_backorder=ifelse(went_on_backorder=="Yes",1,0),
         went_on_backorder=as.numeric(went_on_backorder))

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(rg)

lapply(rg,function(x) length(unique(x)))

names(rg)[sapply(rg,function(x) is.character(x))]

# we'll exclude column named data as it simply represent which dataset the observation is from

cat_cols=c("potential_issue","deck_risk","oe_constraint",
           "ppap_risk","stop_auto_buy","rev_stop")

for(cat in cat_cols){
  rg=CreateDummies(rg,cat,50)
}
rg=rg %>%
  select(-sku,-in_transit_qty,-min_bank)

for(col in names(rg)){
  
  if(sum(is.na(rg[,col]))>0 & !(col %in% c("data","went_on_backorder"))){
    
    rg[is.na(rg[,col]),col]=mean(rg[rg$data=='train',col],na.rm=T)
  }
  
}

rg_train=rg %>% filter(data=='train') %>% select(-data)
rg_test=rg %>% filter(data=='test') %>% select (-data,-went_on_backorder)

set.seed(2)
s=sample(1:nrow(rg_train),0.8*nrow(rg_train))
rg_train1=rg_train[s,]
rg_train2=rg_train[-s,]

ld.tree=tree(went_on_backorder~.,data=rg_train1)

## Tree in text format

ld.tree

## Visual Format

plot(ld.tree)
text(ld.tree)

## Performance on validation set

val.IR=predict(ld.tree,newdata = rg_train2)

rmse_val=((val.IR)-(rg_train2$went_on_backorder))^2 %>% mean() %>% sqrt()
rmse_val

rg.tree.final=tree(went_on_backorder~.,data=rg_train)
test.pred=predict(rg.tree.final,newdata=rg_test)

train.score=predict(rg.tree.final,newdata=rg_train,type='vector')
real=rg_train$went_on_backorder

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

## Once we know the cutoff we can use it to convert test score to 
## hard classes

test.predicted=as.numeric(test.pred>my_cutoff)
write.csv(test.predicted,"mrunal_sawant_p3_part3.csv",row.names = F)
