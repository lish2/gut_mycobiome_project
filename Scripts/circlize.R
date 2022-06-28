# BiocManager::install("statnet")
library(statnet)
library(circlize)

data<-read.csv("SC.csv",header=T,row=1)
my.data<-as.matrix(data) # 矩阵化
rownames(my.data) <-c("CCK", "CNPK", "GCCK", "GCNPK")
colnames(my.data) <-c("Alphaproteobacteria","Betaproteobacteria","Gammaproteobacteria",
                      "Deltaproteobacteria","Acidobacteria","Actinobacteria",
                      "Bacteroidetes","Chloroflexi","Firmicutes",
                      "Gemmatimonadetes","Planctomycetes","Thaumarchaeota" ,
                      "Verrucomicrobia","Ascomycota",  "Basidiomycota",
                      "Zygomycota")


grid.col = NULL

# 定义处理的颜色，这里随便选取了4个颜色，大家可以根据自己的喜好制定好看的配色
grid.col[c("CCK", "CNPK", "GCCK", "GCNPK")] = c("blue", "black", "orange", "chocolate")

# 定义微生物各个门的颜色，
grid.col[colnames(my.data)] = c("lavender", "khaki","mistyrose",
                                "sienna1", "skyblue", "brown1",
                                "gold", "maroon", "salmon", "moccasin",
                                "wheat","black","green","cyan","pink","orange")


#==========================================================
#                 画图
#==========================================================
# 参数设置
circos.par(gap.degree = c(rep(2, nrow(my.data)-1), 10, rep(2, ncol(my.data)-1), 10),
           start.degree = 180)

# 出图，本人这里只用了少部分参数，所有参数见此包的help文档，或者看下文
chordDiagram(my.data,
             directional = TRUE,
             diffHeight = 0.06,
             grid.col = grid.col,
             transparency = 0.5)

# 图例制作
legend("right",pch=20,legend=colnames(my.data),
       col=grid.col[colnames(my.data)],bty="n",
       cex=1,pt.cex=3,border="black") # 设定图例










dt <- read.table("data.v4.txt",sep="\t", header = T, row.names=1, check.names = F)
# dt$HGT_count <- as.vector(dt$HGT_count)

dt <- as.matrix(dt)
# 设置颜色

# grid.col = c(Syncephalastraceae = "#ffff33", Lichtheimiaceae = "#00b0ff", Rhizopodaceae = "#ff7f00", Mucoraceae = "#1f78b4", Sporidiobolaceae = "#0069e9", Filobasidiaceae = "#39012c", Dipodascaceae = "#a6cee3", Pichiaceae = "#a5a9fd", Metschnikowiaceae = "#2C03B0", Debaryomycetaceae = "#7551a0", Hypocreaceae = "#ee0000", Sporocadaceae = "#FE4365", Coniochaetaceae = "#1ed76d", Chaetomiaceae = "#7e7a4c", Pleosporaceae = "#D040ED", Trichocomaceae = "#b38987", Thermoascaceae = "#f781bf", Aspergillaceae = "#A54A22")

#grid.col = c(Syncephalastraceae="#000000", Lichtheimiaceae="#00b0ff", Rhizopodaceae="#ff7f00", Mucoraceae="#7f2b45", Sporidiobolaceae="#0069e9", Filobasidiaceae="#1f78b4", Dipodascaceae="#b17b59", Pichiaceae="#a5a9fd", Metschnikowiaceae="#ee0000", Debaryomycetaceae="#7551a0", Hypocreaceae="#2c03B0", Sporocadaceae="#FE3465", Coniochaetaceae="#206f44", Chaetomiaceae="#7e7a4c", Pleosporaceae="#D040ED", Trichocomaceae="#bf95a2", Thermoascaceae="#f781bf", Aspergillaceae="#8d161b")
grid.col = c(Syncephalastraceae="#000000", Lichtheimiaceae="#00b0ff", Rhizopodaceae="#ff7f00", Mucoraceae="#aaab42", Sporidiobolaceae="#0069e9", Filobasidiaceae="#1f78b4", Dipodascaceae="#bdbb66", Pichiaceae="#a5a9fd", Metschnikowiaceae="#ee0000", Debaryomycetaceae="#7551a0", Hypocreaceae="#2c03B0", Sporocadaceae="#FE3465", Coniochaetaceae="#206f44", Chaetomiaceae="#cbcb8c", Pleosporaceae="#D040ED", Trichocomaceae="#dddfb5", Thermoascaceae="#f781bf", Aspergillaceae="#9a9a37")
# 设置有多少源头
circos.par(gap.degree = c(rep(2, 18)),
           start.degree = 180)
# 画图
chordDiagram(dt, 
             annotationTrack = 'grid',
             annotationTrackHeight = 0.1,
             grid.col = grid.col,
             #link.sort = TRUE,
             #annotationTrack = c("name", "grid"),
             # annotationTrackHeight = c(0.01, 5),#设(置标签离图的距离，条带宽窄)，他和上面的成对使用
             #preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(my.dt))))),
             
             )

# 设置标签
circos.track(
  track.index = 1, panel.fun = function(x, y)
    {
  circos.text(CELL_META$xcenter, 
              CELL_META$ylim[1],
              CELL_META$sector.index, 
              facing = "clockwise", 
              niceFacing = TRUE, adj = c(-0.4, 0.5)
              )
    }, bg.border = NA
  )
circos.clear()





