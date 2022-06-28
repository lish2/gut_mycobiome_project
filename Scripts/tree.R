rm(list=ls())
#--------------------------------------------------------------
#                    2020/05/20
#--------------------------------------------------------------
library(reshape2)
library(cowplot)
library(ggplot2)
library(ggtree)
library(treeio) #就是解析各种进化树的包，nwk的话，可以加载
library(phytools)
library(tibble) # 加强版数据框
library(ggstance)

tree <- read.newick("family.tree.v4.nwk")
info <- read.table("01.内部节点的share pan基因.txt", sep="\t", check.names = FALSE, header=T)
info <- read.table("02.pan family.txt", sep="\t", check.names = FALSE, header=T)
info <- read.table("03.percent.txt", sep="\t", check.names = FALSE, header=T)
info <- read.table("04.clad.tot.uniq.family.txt", sep="\t", check.names = FALSE, header=T)
info <- read.table("05.leaf.abs.uniq.txt", sep="\t", check.names = FALSE, header=T)
info <- read.table("06.leaf.abs.uniq.percent.txt", sep="\t", check.names = FALSE, header=T)


tb <- as_tibble(tree)
#write.table(tb, "v1.tree.map.csv", row.names=F)

tb[c("node.num", "leaf.num", "label.name")]  <- merge(tb, info, all=TRUE)[,5:7]
tree <- as.treedata(tb)

p <- ggtree(tree) + 
  #geom_treescale(x=20, y=0, width=0.1, color='red') +
  geom_label(aes(label=node.num), hjust=0.5, size=2 , fill='#3d3d3d', color='white') + #节点显示数字
  geom_tiplab(aes(label=leaf.num),offset=0, geom="label",  hjust=.5, size=2, linesize = 5.0, ) + #显示特有genemfamily
  geom_tiplab(aes(label=label.name),offset=1, align=1, hjust=.5, size=4) + 
  #geom_text2(aes(subset=!isTip, label=node.num), hjust=0,)  +# 显示节点
  #xlim(NA,20) +ylim(NA,39) +
  scale_y_reverse() + # 翻转
  theme_tree2()

p




















#===================================================
#                   添加来源线
#===================================================

#dat = read.table("clipboard", sep="\t", header=F, check.names=F)

p+geom_taxalink(taxa1="Sporidiobolaceae (11)", taxa2="Metschnikowiaceae (4)")

p + geom_taxalink(data=dat,
                mapping=aes(taxa1=V2, taxa2=V1, color='red'),
                ncp=10, 
                #hratio=0,
                size=1,
                alpha=0.1,
                offset=-0.03,
                arrow=arrow(length=unit(0.01, "npc")),# 画箭头
                )
p1




