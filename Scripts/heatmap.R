#library(venn)


youzi =read.table("data.family.matrix.txt", header=T, row.names=1,sep="\t", stringsAsFactors=F, check.names=F)



#data1 <- as.list(data)

#venn(data1,zcolor='style', ilcs=1, sncs=1.5, box=F)



library(ggplot2)
library(RColorBrewer)
#library(reshape2)


ggplot(data, aes(y,x)) + 
  geom_tile(aes(fill = s.x),colour = "white")+
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90))+
  scale_fill_gradient(name="Value", low = "white",high = "red")

map <- read.table("heat.map.txt", sep="\t", header=T, row.names=1)
map$phylum = as.factor(map$phylum)
color_matrix = cbind(brewer.pal(4,"Set1")[map$phylum])
colnames(color_matrix) = c('Phylum') # 重命名列名


library(heatmap3)
youzi = as.matrix(youzi)

heatmap3(youzi,             
        #改为从红色渐变到黄色再变到白色
        #col=c(rev(heat.colors(youzi,alpha=1,rev=FALSE))),
        #col=c(rev(rainbow(99,start=0.7,end=0.1)),"black"),
        key=T,densadj=0.1,denscol=T,
        trace="none",
        na.rm=T,na.color="white",
        #cexCol=0.5,cexRow=0.5,margins=c(6,6),
        ColSideColors = color_matrix, 
        RowSideColors = color_matrix,# 添加竖列的分组
        scale="column",
        )












library(ggtree)
library(treeio)

tree = read.newick("./family.tree.v4.nwk")
tb = as_tibble(tree)
temp = read.table("label.map", sep="\t", header=T)
tb = merge(tb, temp, by='label', all.x=TRUE)
tb$label = tb$label.c
tb = tb[c('parent','node','branch.length','label')]
class(tb) <- c("tbl_tree","tbl_df","tbl","data.frame")
tb = tb[order(tb$node),] #必须按照node排序才行！！！！！！！！！！！！！！
tree <- as.treedata(tb)

p = ggtree(tree) + 
  geom_tiplab(aes(label=label),offset=1, align=1,hjust=.5, size=3,linesize = 0.10, ) +#+ #显示特有genemfamily
  theme_tree2()
p





library(aplot)
library(dplyr)
library(reshape2)
library(ggplot2)
library(treeio)
library(ggtree)
#~~~~~~~~~~~~~~~~~~~~~ jinh
youzi =read.table("data.family.matrix.txt", header=T,sep="\t", check.names=F)
raw_tree_plot = ggtree(tree)+geom_tiplab(size=3, aes(label=label),align=1) + theme_tree2() + scale_y_reverse()+xlim(NA,20)
raw_tree_data = raw_tree_plot$data %>% subset(isTip=="TRUE") %>% arrange(y) 
dd = melt(youzi)


dd$X = factor(dd$X, levels = raw_tree_data$label) #排序！
dd$variable = factor(dd$variable, levels = raw_tree_data$label) 
#theme_bw() 有边框
p4 <- ggplot(dd, aes(x=X, y=variable), size=0.1) + 
  geom_tile(aes(fill=value)) + 
  theme_tree2()+
  scale_fill_gradient2(name="Value", low ='#01b2eb', mid='#f9ff07', midpoint = 0.5,  high = '#ec201d')+
  theme(axis.text.x=element_text(angle = 50))

p4


a = p4 %>% insert_left(raw_tree_plot)

ggsave(a,file="heatmap.pdf",width = 14,height = 7)











