dt = read.table("modules.tsv",header=T,row.names = 1,sep="\t",check.names = F)

map = read.table("./module_GMM_Module_function_list",sep="\t", header=F, check.names=F)
taxo = read.table("../输入数据/new.group.txt", header=T,sep="\t",check.names = F)
map = map[,c(1,2,3)]

dt_t = t(dt)


dm = merge(map, dt, by.y='row.names', by.x='V1')

selection = c("amino acid degradation", "lipid degradation",
              "carbohydrate degradation", "organic acid metabolism")

library(dplyr)
library(ggplot2)
library(reshape2)

data = dm[dm$V3 %in% selection,]
dd = melt(data)
p1 = ggplot(data = dd, aes(x=V3, y=value, fill=V3))+geom_boxplot()

dt2 = merge(dd, taxo, by.x='variable', by.y='id')

library(ggsignif)

dt3 = dt2[order(dt2$V3,dt2$V2, decreasing = TRUE),]
dt3$V3 = factor(dt3$V3, levels=unique(dt3$V3))
dt3$V2 = factor(dt3$V2, levels=unique(dt3$V2))
dt3$taxo_v1 = factor(dt3$taxo_v1, levels=unique(dt3$taxo_v1))

ggplot(data = dt3, aes(x=taxo_v1, y=value, fill=taxo_v1))+
  geom_boxplot()+
  facet_wrap(~V2, scales ="free_y")+
  theme_bw()






########################################################################



library(vegan)

pvalue = rbind()


acids = unique(dt3$V2)
taxos = unique(dt3$taxo_v1)

for(k in acids){
  temp = dt3[which(dt3$V2 == k),]
  for (i in taxos){
  x = temp[which(temp$taxo_v1 == i),5]
  for(j in taxos){
    y = temp[which(temp$taxo_v1 == j),5]
    if (i != j){
      wt = wilcox.test(x, y)$p.value
      tt = t.test(x, y)$p.value
      c = data.frame(d=k, a=i, b=j, t.test=tt, w.test=wt)
      pvalue = rbind(pvalue, c)
    }
  }
}
}





library(fdrtool)
pvalue$t.test.qvalue = fdrtool(pvalue$t.test, statistic = 'pvalue')$qval
pvalue$w.test.qvalue = fdrtool(pvalue$w.test, statistic = 'pvalue')$qval

write.table(pvalue, "pvalue.csv", sep=",")

library(plyr)





zy = function(x){
  num = count(x)
  if (num[1,1] == 0){
    num=num[1,2]
  }else{
    num = 0
  }
  num
}

ddd = aggregate(data[,3:5], by=list(data[,2]), zy)

library("patchwork")
xx = melt(ddd)
p2 = ggplot(data = xx, aes(x=Group.1, y=value, fill=Group.1))+geom_boxplot()

