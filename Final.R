library(dplyr)
library(igraph)
library(lubridate)

###############
#data cleaning#
###############

data <- read.csv("soc-sign-bitcoinalpha.csv", header = FALSE)
colnames(data) <- c("source","target","rating","time")
data$time <- as.POSIXct(data$time, format="%Y-%m-%d", origin="1970-01-01",tz="GMT")

#add mean values to the dataset
mean_table <- aggregate(data[, 3], list(data$target), mean)
colnames(mean_table) <- c("source","source_mean") 
data_with_mean <- merge(data, mean_table, by = "source", all = TRUE, sort = FALSE)

data_with_mean$target_mean <- ave(data_with_mean$rating, data_with_mean$target)

#add labels for each user
data_with_mean$source <- paste("user", data_with_mean$source)
data_with_mean$target <- paste("user", data_with_mean$target)

#categorize users
levels <- c(-10, -6, -2, 2, 6, 10)
labels <- c("Highly Untrustworthy","Somehow Untrustworthy","Neutral","Somehow Trustworthy","Highly Trustworthy")
data_with_mean <- data_with_mean %>% 
  mutate(source_level = cut(source_mean, levels, labels = labels)) %>%
  mutate(target_level = cut(target_mean, levels, labels = labels))

#take sample
data_with_mean$year <- year(data_with_mean$time)
data_with_mean$month <- month(data_with_mean$time)
data_with_mean$day <- day(data_with_mean$time)
table(data_with_mean$year)

data_sample <- filter(data_with_mean, year == 2013)
data_sample <- na.omit(data_sample)
table(data_sample$month)
data_sample_11 <- filter(data_sample,month==11)
data_sample_11 <- select(data_sample_11,source,target,rating,day,source_level,target_level)
data_sample_12 <- filter(data_sample,month==12)
data_sample_12 <- select(data_sample_12,source,target,rating,day,source_level,target_level)

##########
#November#
##########

#to get a sense of the distribution of this attribute across vertices
source.level.11 <- data_frame(as.character(data_sample_11$source_level))
target.level.11 <- data_frame(as.character(data_sample_11$target_level))
colnames(source.level.11) <- c("level")
colnames(target.level.11) <- c("level")
vertex.status.11 <- rbind(source.level.11,target.level.11)
table(vertex.status.11)

status.t.11 <- table(data_sample_11$source_level,data_sample_11$target_level)
status.t.11 <- status.t.11 + t(status.t.11)
diag(status.t.11) <- round(diag(status.t.11)/2)
status.t.11

#hist

tmp.es.11 <- paste(data_sample_11$source_level, "-", data_sample_11$target_level, sep="") 
e.status.11 <- character(dim(data_sample_11)[[1]])
e.status.11[tmp.es.11=="Neutral-Neutral"] <- "Neutral-Neutral"
e.status.11[tmp.es.11=="Somehow Trustworthy-Somehow Trustworthy"] <- "ST-ST"
e.status.11[tmp.es.11=="Highly Trustworthy-Highly Trustworthy"] <- "HT-HT"
e.status.11[tmp.es.11=="Somehow Untrustworthy-Somehow Untrustworthy"] <- "SU-SU"
e.status.11[tmp.es.11=="Highly Untrustworthy-Highly Untrustworthy"] <- "HU-HU"

e.status.11[tmp.es.11=="Neutral-Somehow Trustworthy"] <- "Neutral-ST"
e.status.11[tmp.es.11=="Neutral-Highly Trustworthy"] <- "Neutral-HT"
e.status.11[tmp.es.11=="Neutral-Somehow Untrustworthy"] <- "Neutral-SU"
e.status.11[tmp.es.11=="Neutral-Highly Untrustworthy"] <- "Neutral-HU"

e.status.11[tmp.es.11=="Somehow Trustworthy-Neutral"] <- "ST-Neutral"
e.status.11[tmp.es.11=="Somehow Trustworthy-Highly Trustworthy"] <- "ST-HT"
e.status.11[tmp.es.11=="Somehow Trustworthy-Somehow Untrustworthy"] <- "ST-SU"
e.status.11[tmp.es.11=="Somehow Trustworthy-Highly Untrustworthy"] <- "ST-HU"

e.status.11[tmp.es.11=="Highly Trustworthy-Neutral"] <- "HT-Neutral"
e.status.11[tmp.es.11=="Highly Trustworthy-Somehow Trustworthy"] <- "HT-ST"
e.status.11[tmp.es.11=="Highly Trustworthy-Somehow Untrustworthy"] <- "HT-SU"
e.status.11[tmp.es.11=="Highly Trustworthy-Highly Untrustworthy"] <- "HT-HU"

e.status.11[tmp.es.11=="Somehow Untrustworthy-Neutral"] <- "SU-Neutral"
e.status.11[tmp.es.11=="Somehow Untrustworthy-Somehow Trustworthy"] <- "SU-ST"
e.status.11[tmp.es.11=="Somehow Untrustworthy-Highly Trustworthy"] <- "SU-HT"
e.status.11[tmp.es.11=="Somehow Untrustworthy-Highly Untrustworthy"] <- "SU-HU"

e.status.11[tmp.es.11=="Highly Untrustworthy-Neutral"] <- "HU-Neutral"
e.status.11[tmp.es.11=="Highly Untrustworthy-Somehow Trustworthy"] <- "HU-ST"
e.status.11[tmp.es.11=="Highly Untrustworthy-Highly Trustworthy"] <- "HU-HT"
e.status.11[tmp.es.11=="Highly Untrustworthy-Somehow Untrustworthy"] <- "HU-SU"

sample.hist.11 <- data.frame(day = data_sample_11$day,source_level = data_sample_11$source_level,
                          target_level = data_sample_11$target_level,status = e.status.11)
library(lattice)
histogram( ~ day|status, data=sample.hist.11, xlab="Day_November",
           layout=c(4,4))
#Timeline

library(networkDynamic)
spls.11 <- cbind(data_sample_11$day-0.5, data_sample_11$day,
                 data_sample_11$source_level, data_sample_11$target_level)
dn.11 <- networkDynamic(edge.spells=spls.11)
dn.11 <- as.data.frame(dn.11)

mycols.11 <- numeric(nrow(dn.11)-2)
mycols.11[tmp.es.11=="Neutral-Neutral"] <- 1
mycols.11[tmp.es.11=="Somehow Trustworthy-Somehow Trustworthy"] <- 2
mycols.11[tmp.es.11=="Highly Trustworthy-Highly Trustworthy"] <- 3
mycols.11[tmp.es.11=="Somehow Untrustworthy-Somehow Untrustworthy"] <- 4
mycols.11[tmp.es.11=="Highly Untrustworthy-Highly Untrustworthy"] <- 5

mycols.11[tmp.es.11=="Neutral-Somehow Trustworthy"] <- 6
mycols.11[tmp.es.11=="Neutral-Highly Trustworthy"] <- 7
mycols.11[tmp.es.11=="Neutral-Somehow Untrustworthy"] <- 8
mycols.11[tmp.es.11=="Neutral-Highly Untrustworthy"] <- 9

mycols.11[tmp.es.11=="Somehow Trustworthy-Neutral"] <- 10
mycols.11[tmp.es.11=="Somehow Trustworthy-Highly Trustworthy"] <- 11
mycols.11[tmp.es.11=="Somehow Trustworthy-Somehow Untrustworthy"] <- 12
mycols.11[tmp.es.11=="Somehow Trustworthy-Highly Untrustworthy"] <- 13

mycols.11[tmp.es.11=="Highly Trustworthy-Neutral"] <- 14
mycols.11[tmp.es.11=="Highly Trustworthy-Somehow Trustworthy"] <- 15
mycols.11[tmp.es.11=="Highly Trustworthy-Somehow Untrustworthy"] <- 16
mycols.11[tmp.es.11=="Highly Trustworthy-Highly Untrustworthy"] <- 17

mycols.11[tmp.es.11=="Somehow Untrustworthy-Neutral"] <- 18
mycols.11[tmp.es.11=="Somehow Untrustworthy-Somehow Trustworthy"] <- 19
mycols.11[tmp.es.11=="Somehow Untrustworthy-Highly Trustworthy"] <- 20
mycols.11[tmp.es.11=="Somehow Untrustworthy-Highly Untrustworthy"] <- 21

mycols.11[tmp.es.11=="Highly Untrustworthy-Neutral"] <- 22
mycols.11[tmp.es.11=="Highly Untrustworthy-Somehow Trustworthy"] <- 23
mycols.11[tmp.es.11=="Highly Untrustworthy-Highly Trustworthy"] <- 24
mycols.11[tmp.es.11=="Highly Untrustworthy-Somehow Untrustworthy"] <- 25
my.palette.11 <- rainbow(25)

ne.11 <- max(dn.11$edge.id)
max.t.11 <- max(dn.11$terminus) - 2
plot(c(0, max.t.11), c(0, ne.11), ann=F, type='n') 
segments(dn.11$onset, dn.11$edge.id,
         dn.11$terminus, dn.11$edge.id,
         col=my.palette.11[mycols.11]) 
title(xlab="Time (hours)",ylab="Interacting Pair(Ordered by First Interaction)",main="November")
abline(v=c(7, 15, 23, 30), lty="dashed", lw=2, col="lightgray")
status.pairs <- c("NEUTRAL-NEUTRAL", "ST-ST", "HT-HT", "SU-SU", "HU-HU", 
                  "ST-NEUTRAL", "ST-HT","ST-SU", "ST-HU",
                  "HT-NEUTRAL", "HT-ST","HT-SU", "HT-HU",
                  "SU-NEUTRAL", "SU-ST","SU-HT", "SU-HU",
                  "HU-NEUTRAL", "HU-ST","HU-HT", "HU-SU")
detach(package:networkDynamic)

##########
#December#
##########

#to get a sense of the distribution of this attribute across vertices
source.level.12 <- data_frame(as.character(data_sample_12$source_level))
target.level.12 <- data_frame(as.character(data_sample_12$target_level))
colnames(source.level.12) <- c("level")
colnames(target.level.12) <- c("level")
vertex.status.12 <- rbind(source.level.12,target.level.12)
table(vertex.status.12)

status.t.12 <- table(data_sample_12$source_level,data_sample_12$target_level)
status.t.12 <- status.t.12 + t(status.t.12)
diag(status.t.12) <- round(diag(status.t.12)/2)
status.t.12

#hist

tmp.es.12 <- paste(data_sample_12$source_level, "-", data_sample_12$target_level, sep="") 
e.status.12 <- character(dim(data_sample_12)[[1]])
e.status.12[tmp.es.12=="Neutral-Neutral"] <- "Neutral-Neutral"
e.status.12[tmp.es.12=="Somehow Trustworthy-Somehow Trustworthy"] <- "ST-ST"
e.status.12[tmp.es.12=="Highly Trustworthy-Highly Trustworthy"] <- "HT-HT"
e.status.12[tmp.es.12=="Somehow Untrustworthy-Somehow Untrustworthy"] <- "SU-SU"
e.status.12[tmp.es.12=="Highly Untrustworthy-Highly Untrustworthy"] <- "HU-HU"

e.status.12[tmp.es.12=="Neutral-Somehow Trustworthy"] <- "Neutral-ST"
e.status.12[tmp.es.12=="Neutral-Highly Trustworthy"] <- "Neutral-HT"
e.status.12[tmp.es.12=="Neutral-Somehow Untrustworthy"] <- "Neutral-SU"
e.status.12[tmp.es.12=="Neutral-Highly Untrustworthy"] <- "Neutral-HU"

e.status.12[tmp.es.12=="Somehow Trustworthy-Neutral"] <- "ST-Neutral"
e.status.12[tmp.es.12=="Somehow Trustworthy-Highly Trustworthy"] <- "ST-HT"
e.status.12[tmp.es.12=="Somehow Trustworthy-Somehow Untrustworthy"] <- "ST-SU"
e.status.12[tmp.es.12=="Somehow Trustworthy-Highly Untrustworthy"] <- "ST-HU"

e.status.12[tmp.es.12=="Highly Trustworthy-Neutral"] <- "HT-Neutral"
e.status.12[tmp.es.12=="Highly Trustworthy-Somehow Trustworthy"] <- "HT-ST"
e.status.12[tmp.es.12=="Highly Trustworthy-Somehow Untrustworthy"] <- "HT-SU"
e.status.12[tmp.es.12=="Highly Trustworthy-Highly Untrustworthy"] <- "HT-HU"

e.status.12[tmp.es.12=="Somehow Untrustworthy-Neutral"] <- "SU-Neutral"
e.status.12[tmp.es.12=="Somehow Untrustworthy-Somehow Trustworthy"] <- "SU-ST"
e.status.12[tmp.es.12=="Somehow Untrustworthy-Highly Trustworthy"] <- "SU-HT"
e.status.12[tmp.es.12=="Somehow Untrustworthy-Highly Untrustworthy"] <- "SU-HU"

e.status.12[tmp.es.12=="Highly Untrustworthy-Neutral"] <- "HU-Neutral"
e.status.12[tmp.es.12=="Highly Untrustworthy-Somehow Trustworthy"] <- "HU-ST"
e.status.12[tmp.es.12=="Highly Untrustworthy-Highly Trustworthy"] <- "HU-HT"
e.status.12[tmp.es.12=="Highly Untrustworthy-Somehow Untrustworthy"] <- "HU-SU"

sample.hist.12 <- data.frame(day = data_sample_12$day,source_level = data_sample_12$source_level,
                          target_level = data_sample_12$target_level,status = e.status.12)
library(lattice)
histogram( ~ day|status, data=sample.hist.12, xlab="Day_December",
           layout=c(4,4))

#Timeline
library(networkDynamic)
spls.12 <- cbind(data_sample_12$day-0.5, data_sample_12$day,
                 data_sample_12$source_level, data_sample_12$target_level)
dn.12 <- networkDynamic(edge.spells=spls.12)
dn.12 <- as.data.frame(dn.12)

mycols.12 <- numeric(nrow(dn.12)-2)
mycols.12[tmp.es.12=="Neutral-Neutral"] <- 1
mycols.12[tmp.es.12=="Somehow Trustworthy-Somehow Trustworthy"] <- 2
mycols.12[tmp.es.12=="Highly Trustworthy-Highly Trustworthy"] <- 3
mycols.12[tmp.es.12=="Somehow Untrustworthy-Somehow Untrustworthy"] <- 4
mycols.12[tmp.es.12=="Highly Untrustworthy-Highly Untrustworthy"] <- 5

mycols.12[tmp.es.12=="Neutral-Somehow Trustworthy"] <- 6
mycols.12[tmp.es.12=="Neutral-Highly Trustworthy"] <- 7
mycols.12[tmp.es.12=="Neutral-Somehow Untrustworthy"] <- 8
mycols.12[tmp.es.12=="Neutral-Highly Untrustworthy"] <- 9

mycols.12[tmp.es.12=="Somehow Trustworthy-Neutral"] <- 10
mycols.12[tmp.es.12=="Somehow Trustworthy-Highly Trustworthy"] <- 11
mycols.12[tmp.es.12=="Somehow Trustworthy-Somehow Untrustworthy"] <- 12
mycols.12[tmp.es.12=="Somehow Trustworthy-Highly Untrustworthy"] <- 13

mycols.12[tmp.es.12=="Highly Trustworthy-Neutral"] <- 14
mycols.12[tmp.es.12=="Highly Trustworthy-Somehow Trustworthy"] <- 15
mycols.12[tmp.es.12=="Highly Trustworthy-Somehow Untrustworthy"] <- 16
mycols.12[tmp.es.12=="Highly Trustworthy-Highly Untrustworthy"] <- 17

mycols.12[tmp.es.12=="Somehow Untrustworthy-Neutral"] <- 18
mycols.12[tmp.es.12=="Somehow Untrustworthy-Somehow Trustworthy"] <- 19
mycols.12[tmp.es.12=="Somehow Untrustworthy-Highly Trustworthy"] <- 20
mycols.12[tmp.es.12=="Somehow Untrustworthy-Highly Untrustworthy"] <- 21

mycols.12[tmp.es.12=="Highly Untrustworthy-Neutral"] <- 22
mycols.12[tmp.es.12=="Highly Untrustworthy-Somehow Trustworthy"] <- 23
mycols.12[tmp.es.12=="Highly Untrustworthy-Highly Trustworthy"] <- 24
mycols.12[tmp.es.12=="Highly Untrustworthy-Somehow Untrustworthy"] <- 25
my.palette.12 <- rainbow(25)

ne.12 <- max(dn.12$edge.id)
max.t.12 <- max(dn.12$terminus) - 2
plot(c(0, max.t.12), c(0, ne.12), ann=F, type='n') 
segments(dn.12$onset, dn.12$edge.id,
          dn.12$terminus, dn.12$edge.id,
          col=my.palette.12[mycols.12]) 
title(xlab="Time (hours)",ylab="Interacting Pair(Ordered by First Interaction)",main="December")
abline(v=c(7, 15, 23, 30), lty="dashed", lw=2, col="lightgray")
status.pairs <- c("NEUTRAL-NEUTRAL", "ST-ST", "HT-HT", "SU-SU", "HU-HU", 
                  "ST-NEUTRAL", "ST-HT","ST-SU", "ST-HU",
                  "HT-NEUTRAL", "HT-ST","HT-SU", "HT-HU",
                  "SU-NEUTRAL", "SU-ST","SU-HT", "SU-HU",
                  "HU-NEUTRAL", "HU-ST","HU-HT", "HU-SU")
detach(package:networkDynamic)

#################
#igraph November#
#################
set.seed(1)
v_11 <- sort(unique(c(data_sample_11$source, data_sample_11$target)))
g_11 <- graph.data.frame(data_sample_11[,c("source","target","day","rating","source_level","target_level")],
                         vertices=data.frame(v_11),directed=TRUE)
V(g_11)$comp <- components(g_11)$membership
#g_main <- induced_subgraph(g,V(g)$comp==1)
E(g_11)$weight <- data_sample_11$rating

status_11 <- unique(rbind(data.frame(id=data_sample_11$source, status=data_sample_11$source_level), 
                          data.frame(id=data_sample_11$target, status=data_sample_11$target_level)))
V(g_11)$Status <- as.character(status_11[order(status_11[,1]),2])
V(g_11)$label <- v_11
l = layout.fruchterman.reingold(g_11) 
v.cols.11 <- character(214)
v.cols.11[V(g_11)$Status=="Highly Trustworthy"] <- "yellow" 
v.cols.11[V(g_11)$Status=="Somehow Trustworthy"] <- "blue" 
v.cols.11[V(g_11)$Status=="Neutral"] <- "green" 
v.cols.11[V(g_11)$Status=="Somehow Untrustworthy"] <- "black"
v.cols.11[V(g_11)$Status=="Highly Untrustworthy"] <- "red"

degree_11 <- data.frame(degree(g_11))
strength_11 <- data.frame(graph.strength(g_11))
colMeans(strength_11[1])

hist(degree(g_11), col="lightblue",
     xlab="Vertex Degree",ylab="Frequency",main="")
hist(graph.strength(g_11), col="pink",
     xlab="Vertex Strength",ylab="Frequency",main="")

plot(g_11, vertex.size = 5,vertex.label=NA,vertex.color= v.cols.11,
     edge.arrow.size = 0.1,edge.width=E(g_11)$weight/2,edge.color="black",layout = l)
title(main = "November")
#dynamic
g_11_week <- lapply(1:4, function(i) { 
  g <- subgraph.edges(g_11,
                      E(g_11)[day > 0+8*(i-1) & day <= 7+8*(i-1)],
                      delete.vertices = FALSE)
  #simplify(g)
})

sapply(g_11_week,vcount)
sapply(g_11_week,ecount)

mean_11 <- data.frame(matrix(ncol=1,nrow=4))
for(i in (1:4)) {
  mean_11[i,] <- mean(degree(g_11_week[[i]])) 
}

par(mfrow=c(2,2))
for(i in (1:4)) {
  plot(g_11_week[[i]], layout=l, vertex.size=5, edge.width=E(g_12)$weight/3, 
       vertex.color=ifelse(V(g_11)$label == "user 5", "red", "blue"),
       vertex.label=NA,edge.color="black",edge.arrow.size = 0.1)
  title(paste("Week ",i)) 
}


all.deg.11 <- sapply(g_11_week, degree) 
sl.lab.11<- unlist(lapply(1:4, function(i)
paste("Week",i, sep=""))) 
deg.df.11 <- data.frame(Degree=as.vector(all.deg.11),
                     Slice = rep(sl.lab.11, each=237),
                     Status = rep(V(g_11)$Status, times=4)) 
library(ggplot2)
p.11 = qplot(factor(Degree), data=deg.df.11,
          geom="bar", fill=Status)
p.11 + facet_grid(Slice ~ .)+xlab("Degree")+ylab("Count")+labs(title= "November")


#################
#igraph December#
#################
set.seed(1)
v_12 <- sort(unique(c(data_sample_12$source, data_sample_12$target)))
g_12 <- graph.data.frame(data_sample_12[,c("source","target","day","rating","source_level","target_level")],
                      vertices=data.frame(v_12),directed=TRUE)
V(g_12)$comp <- components(g_12)$membership
#g_main <- induced_subgraph(g,V(g)$comp==1)
E(g_12)$weight <- data_sample_12$rating

status_12 <- unique(rbind(data.frame(id=data_sample_12$source, status=data_sample_12$source_level), 
                          data.frame(id=data_sample_12$target, status=data_sample_12$target_level)))
V(g_12)$Status <- as.character(status_12[order(status_12[,1]),2])
V(g_12)$label <- v_12
  
l = layout.fruchterman.reingold(g_12) 
v.cols.12 <- character(214)
v.cols.12[V(g_12)$Status=="Highly Trustworthy"] <- "yellow" 
v.cols.12[V(g_12)$Status=="Somehow Trustworthy"] <- "blue" 
v.cols.12[V(g_12)$Status=="Neutral"] <- "green" 
v.cols.12[V(g_12)$Status=="Somehow Untrustworthy"] <- "black"
v.cols.12[V(g_12)$Status=="Highly Untrustworthy"] <- "red"

degree_12 <- data.frame(degree(g_12))
strength_12 <- data.frame(graph.strength(g_12))
colMeans(strength_12)

hist(degree(g_12), col="lightblue",
     xlab="Vertex Degree",ylab="Frequency",main="")
hist(graph.strength(g_12), col="pink",
     xlab="Vertex Strength",ylab="Frequency",main="")

plot(g_12, vertex.size = 5,vertex.label=NA,vertex.color= v.cols.12,
     edge.arrow.size = 0.1,edge.width=E(g_12)$weight/3,edge.color="black",layout = l)
title(main = "December")
#dynamic
g_12_week <- lapply(1:4, function(i) { 
  g <- subgraph.edges(g_12,
                      E(g_12)[day > 0+8*(i-1) & day <= 7+8*(i-1)],
                      delete.vertices = FALSE)
  #simplify(g)
})

sapply(g_12_week,vcount)
sapply(g_12_week,ecount)

mean_12 <- data.frame(matrix(ncol=1,nrow=4))
for(i in (1:4)) {
  mean_12[i,] <- mean(degree(g_12_week[[i]])) 
}

degree_12_week_1 <- data.frame(degree(g_12_week[[1]]))
degree_12_week_2 <- data.frame(degree(g_12_week[[2]]))
degree_12_week_3 <- data.frame(degree(g_12_week[[3]]))
degree_12_week_4 <- data.frame(degree(g_12_week[[4]]))

par(mfrow=c(2,2))
for(i in (1:4)) {
  plot(g_12_week[[i]], layout=l, vertex.size=5, edge.width=E(g_12)$weight/1.5, 
       vertex.color=ifelse(V(g_12)$label == "user 5", "red", "blue"),
       vertex.label=NA,edge.color="black",edge.arrow.size = 0.1)
       title(paste("Week ",i)) 
}


all.deg.12 <- sapply(g_12_week, degree) 
sl.lab.12<- unlist(lapply(1:4, function(i)
  paste("Week",i, sep=""))) 
deg.df.12 <- data.frame(Degree=as.vector(all.deg.12),
                        Slice = rep(sl.lab.12, each=214),
                        Status = rep(V(g_12)$Status, times=4)) 
library(ggplot2)
p.12 = qplot(factor(Degree), data=deg.df.12,
          geom="bar", fill=Status)
p.12 + facet_grid(Slice ~ .)+xlab("Degree")+ylab("Count")+labs(title= "December")

