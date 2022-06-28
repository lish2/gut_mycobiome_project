library(dplyr)
library(ggplot2)
# library(BiocManager)
# BiocManager::install("socviz")
# BiocManager::install("gapminder")
# library(gapminder)
library(socviz)

tab3 <- gss_sm %>%
  group_by(bigregion, religion) %>%
  summarize(N = n()) %>%
  mutate(prop = N / sum(N),
         pct = round(100 * prop, 2))


p <- ggplot(data=tab3,
            mapping = aes(
              x = religion, 
              fill = religion,
              y = pct )) 
p + geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "百分数") +
  guides(fill = FALSE) +
  coord_flip() + 
  facet_grid(~ bigregion)


library(reshape2)

dt <- read.table("data.txt", sep="\t", header=T)
dt_long <- melt(dt, id.vars=c("taxo"), variable.name = "database", value.name = "count")

p <- ggplot(data=dt_long,
            mapping = aes(
              x = taxo, 
              fill = database,
              y = count )) 
p + geom_bar(stat='identity', position = "dodge") +
  labs(x=NULL, y='database') +
  guides(fill=FALSE) +
  coord_flip() +
  facet_grid( ~ database)








# ===============================================================
#                   gene_count --  gene_family
# ===============================================================
library(ggplot2)
library(reshape2)
dt <- read.table("bar.data.red.txt", sep="\t", header=T, check.names = F)
dt_long <- melt(dt, id.vars = c("taxo_num", "taxonomy"))
#dt_long$taxo_num <- as.character(dt_long$taxo_num)
dt_long$taxo_num <- as.factor(dt_long$taxo_num)

ggplot(data=dt_long, aes(x=taxo_num,fill=variable, y=value)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(panel.grid.major=element_line(colour=NA))














