rm(list=ls())
library(ggplot2)
library(Aitchison)
library(vegan)
library(ggsci)
library(ggrepel)


map <- read.table("../输入数据/group.id2full_taxo.txt",  header=T, check.names = F)


dt <- read.table("../输入数据/cazy.pro",sep="\t", check.names=F, header=T, row.names=1)
dt <- read.table("../输入数据/data.bin_kegg.matrix",sep="\t", check.names=F, header=T, row.names=1)
dt_t = t(dt)

otu.dist <- vegdist(dt_t,method="bray", binary=F)
#  计算组之间的差异
#  statistic R: 0.1496（R[-1,1]，>0表示组间有差异）
#  Significance: 0.001 P值
#anosim(otu.dist, map$phylum, permutations = 999, distance="bray")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

otu_pcoa<- cmdscale(otu.dist,eig=TRUE,
                    k=2   # 维度
                    )


pc12 <- otu_pcoa$points[,1:2]
pc_importance<-round(otu_pcoa$eig/sum(otu_pcoa$eig)*100,digits = 2)
pc12 <- as.data.frame(pc12)
pc12$samples <- row.names(pc12)
data <- merge(pc12, map, by.x="samples", by.y='id')

ggplot(data,aes(x=V2,y=V1,colour=phylum), size=16) + 
  scale_fill_manual(values=cbPalette)+
  #stat_ellipse(type="norm",level = 0.95)+  # 添加置信圈
  geom_point(size=1) + labs(x=paste("PCoA 1 (", pc_importance[1],digits=4,"%)", sep=""), y=paste("PCoA 2 (", pc_importance[2],digits=4, "%)", sep=""),title="bray_curtis PCoA") #绘制点图并设定大小


write.table(data,"data.csv", sep=",")




#######################################################################################
#                             boxplot
#######################################################################################

gg = rbind()
x <- as.matrix(otu.dist)
xx = melt(x)
c1 =1
c2 = 2
for (i in unique(map$taxo_v1)[c1:4]){
  for (j in unique(map$taxo_v1)[c2:4]) {
    if (j != i){
    name_1 = map[which(map$taxo_v1 == i),]$id
    name_2 = map[which(map$taxo_v1 == j),]$id
    b = x[c(name_1),c(name_2)]
    k = melt(b)
    k['Var3'] = paste(i, " vs ", j ,sep="")
    gg = rbind(gg, k)
    }
    c2 = 1 + c2
  }
c1 = c1 + 1
c2 = c1
}

xxxx =  merge(xx, map, by.x='Var2', by.y='id')
xxx = merge(xx, map, by.x='Var1', by.y='id')

x2 = xxx[,c('Var1','Var2','value','taxo_v1')]
x2['taxo_v2'] = xxxx$taxo_v1
x3 = merge(x2,x1, by=c('Var1','Var2'))
x3['tt'] = paste(x3$taxo_v2, " vs ", x3$taxo_v1.y, sep="")
ggplot(x3, aes(x=tt, y=value,fill=tt)) +geom_boxplot()+theme(axis.text.x = element_text(angle = 60, hjust = 1))# 旋转周标签角度


#######################################################################################
#                             距离原点的距离boxplot
#######################################################################################
dist = otu_pcoa$points
dist = as.data.frame(dist)
abs_dist = as.data.frame(sqrt(dist$V1^2 +dist$V2^2))
row.names(abs_dist) = row.names(dist)
colnames(abs_dist) = c("abs_dist")
abs_distm = merge(abs_dist, map, by.x='row.names',by.y='id')

compaired = list(c('Ascomycota_acc','Ascomycota_other'),
  c('Ascomycota_acc', 'Basidiomycota'),
  c('Ascomycota_acc', 'Mucoromycota'),
  c('Ascomycota_other', 'Basidiomycota'),
  c('Ascomycota_other', 'Mucoromycota'),
  c('Basidiomycota', 'Mucoromycota'))


ggplot(abs_distm, aes(y=abs_dist, x=taxo_v1, fill=taxo_v1)) +
  geom_boxplot() +
  stat_compare_means(comparisons=compaired, label = "p.format")+ # 添加显著性p：p.format/signif: p.signif
  theme(axis.text.x = element_text(angle = 60, hjust = 1))# 旋转周标签角度



#######################################################################################
#                             V1距离   boxplot
#######################################################################################
dist = as.data.frame(otu_pcoa$points)
dist['id'] = row.names(dist)
dist = dist[,-2] # 除去V2

distm = merge(dist, map,by='id')

compaired = list(c('Ascomycota_acc','Ascomycota_other'),
                 c('Ascomycota_acc', 'Basidiomycota'),
                 c('Ascomycota_acc', 'Mucoromycota'),
                 c('Ascomycota_other', 'Basidiomycota'),
                 c('Ascomycota_other', 'Mucoromycota'),
                 c('Basidiomycota', 'Mucoromycota'),
                 c('Ascomycota_acc', 'Ascomycota_acc')
                 )


ggplot(distm, aes(y=V1, x=taxo_v1, fill=taxo_v1)) +
  geom_boxplot() +
  stat_compare_means(comparisons=compaired, label = "p.format")+ # 添加显著性p：p.format/signif: p.signif
  theme(axis.text.x = element_text(angle = 60, hjust = 1))# 旋转周标签角度






########################################################
testdt = data[which(data$V1 >0.05 | data$V2 < -0.1),]
other = testdt$samples


ggplot(testdt,aes(x=V2, y=V1,colour=genus, color=genus), size=16) + 
  scale_fill_manual(values=cbPalette)+
  #stat_ellipse(type="norm",level = 0.95)+  # 添加置信圈
  geom_point(size=2) + labs(x=paste("PCoA 1 (", pc_importance[1],digits=4,"%)", sep=""), y=paste("PCoA 2 (", pc_importance[2],digits=4, "%)", sep=""),title="bray_curtis PCoA") #绘制点图并设定大小







#######################################################################################
#                   除去非酒酿酵母后，计算距离
#######################################################################################
testdt = data[which(data$V1 >0.05 | data$V2 < -0.1),]
other = testdt$samples
o_dt = dt_t[other,]



otu.dist <- vegdist(o_dt,method="bray", binary=F)
#  计算组之间的差异
#  statistic R: 0.1496（R[-1,1]，>0表示组间有差异）
#  Significance: 0.001 P值
#anosim(otu.dist, map$phylum, permutations = 999, distance="bray")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

otu_pcoa<- cmdscale(otu.dist,eig=TRUE)
pc12 <- otu_pcoa$points[,1:2]
pc_importance<-round(otu_pcoa$eig/sum(otu_pcoa$eig)*100,digits = 2)
pc12 <- as.data.frame(pc12)
pc12$samples <- row.names(pc12)
data <- merge(pc12, map, by.x="samples", by.y='id')

ggplot(data,aes(x=V2,y=V1,colour=family), size=16) + 
  scale_fill_manual(values=cbPalette)+
  #stat_ellipse(type="norm",level = 0.95)+  # 添加置信圈
  geom_point(size=1) + labs(x=paste("PCoA 1 (", pc_importance[1],digits=4,"%)", sep=""), y=paste("PCoA 2 (", pc_importance[2],digits=4, "%)", sep=""),title="bray_curtis PCoA") #绘制点图并设定大小

