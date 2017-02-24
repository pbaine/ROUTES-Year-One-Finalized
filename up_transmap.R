setwd("E:/ROUTES/Year One Airport Seizure Analysis/Finalized")

library(ggplot2)
library(maps)
library(geosphere)
library(cobs)
library(RColorBrewer)
library(sp)
library(extrafont)
library(devtools)
library(ggalt)

####TOTALS####

map <- map_data("world")
map <- subset(map, region!="Antarctica")

tots <- read.csv("tots_map.csv")

fps <- gcIntermediate(tots[,c('c1long', 'c1lat')], tots[,c('c2long', 'c2lat')], n=100, breakAtDateLine = T, addStartEnd=T, sp = T)
fps <- as(fps, "SpatialLinesDataFrame")
fps.ff <- fortify(fps)

tots$id <-as.character(c(1:nrow(tots)))
gcir2 <- merge(fps.ff, tots, all.x=T, by="id")

bubs <- subset(tots, select= c("Unique","lat","long","total.sum"))
bubs <- data.frame(bubs[complete.cases(bubs),])

wm <- ggplot(data = map, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  geom_map(map = map, aes(map_id = region), 
           color = "black", 
           fill = "gray75", 
           lwd = .05) + 
  coord_proj("+proj=robin") + ylim(c(-86,86)) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey92"),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(0,0,0,0),"lines"))

wm <- wm + geom_point(data = bubs, aes(size = total.sum, group=1), color = "purple4", alpha = .9) + 
  scale_size(breaks = c(30,60,90,114), label = c("30","60","90","114")) +
  geom_line(data = gcir2, aes(x = long.x, y = lat.x, group = group, alpha = sum), color = "purple3", lwd = .7) + 
  scale_alpha_continuous(range = c(0.15, 0.6)) +
  theme(legend.key.height = unit(0.025, "in"),
        legend.position = c(0.5, 0.15),
        legend.justification = c("center", "center"),
        legend.box.just = "center",
        legend.direction = "horizontal",
        legend.background = element_rect(color = "grey20"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT"))

png(file = "totals_transmap.png", res=600, width = 10, height = 5, units = "in")

wm

dev.off()


####IVORY####


map <- map_data("world")
map <- subset(map, region!="Antarctica")

iv <- read.csv("ivory_map.csv")
iv <- iv[order(rev(iv$count)),]

fps <- gcIntermediate(iv[,c('c1long', 'c1lat')], iv[,c('c2long', 'c2lat')], n=100, breakAtDateLine = T, addStartEnd=T, sp = T)
fps <- as(fps, "SpatialLinesDataFrame")
fps.ff <- fortify(fps)

iv$id <-as.character(c(1:nrow(iv))) # that rts.ff$id is a char
gcir <- merge(fps.ff, iv, all.x=T, by="id")

new_data <- gcir[gcir$count > 1,]

new_lines_data <- NULL
groups <- unique(new_data$group)

for(i in 1:length(groups)){
  group <- groups[i]
  data <- new_data[new_data$group == group,]
  count <- max(data$count)
  second_data <- NULL
  for(j in 1:count){
    if(j==1){
      next
    }
    
    data_2 <- data
    
    df <- data.frame(long=data_2$long,lat=data_2$lat)
    
    ## first get new latitute line
    df$lat[nrow(df)/2] <- df$lat[nrow(df)/2] + j
    
    ## create the contraints
    start <- c(0, head(df$long,1) ,head(df$lat,1))
    mid <- c(0, df$long[nrow(df)/2], df$lat[nrow(df)/2])
    end <- c(0, tail(df$long,1), tail(df$lat,1))
    cons <- rbind(start,mid,end)
    knots <- c(head(df$long,1),df$long[nrow(df)/2],tail(df$long,1))
    
    ## make like with contrained b-spline
    cob_lat <- cobs(df$long,df$lat,knots=knots,pointwise=cons)
    
    ## this will be the new curved latitute line
    lat_fit <- cob_lat$fitted
    
    data_2$lat <- lat_fit
    
    data_2$group <- paste(group,".",j,sep="") 
    
    second_data <- rbind(second_data,data_2)
  }
  inter_data <- rbind(data,second_data)
  new_lines_data <- rbind(new_lines_data, inter_data)
}

gcir2 <- gcir[gcir$count == 1,]
gcir2 <- rbind(gcir2, new_lines_data)

bubs <- subset(iv, select = c(Unique, ulat, ulong, sum))
bubs <- data.frame(bubs[complete.cases(bubs),])


wm <- ggplot(data = map, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  geom_map(map = map, aes(map_id = region), 
           color = "black", 
           fill = "gray75", 
           lwd = .05) + 
  coord_proj("+proj=robin") + ylim(c(-86,86)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey92"),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(0,0,0,0),"lines"))

wm <- wm + geom_point(data = bubs, aes(x = ulong, y = ulat, size = sum), color = "darkorange", alpha = .9) + 
  scale_size(breaks = c(25,50,79), label = c("25","50","79")) +
  geom_line(data = gcir2, aes(color = order, group = group), lwd = .7, alpha = .3) + 
  scale_color_distiller(palette = 'Oranges', direction = 1, guide = "colorbar", name = "Flight\nPath", 
                        breaks = c(10,80), label = c("Origin" , "Destination")) +
  theme(legend.key.height = unit(0.025, "in"),
        legend.position = c(0.5, 0.15),
        legend.justification = c("center", "center"),
        legend.box.just = "center",
        legend.direction = "horizontal",
        legend.background = element_rect(color = "grey20"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT"))

png(file = "ivory_transmap.png", res=600, width = 10, height = 5, units = "in")

wm

dev.off()


####RHINO HORN####


map <- map_data("world")
map <- subset(map, region!="Antarctica")

Rhino <- read.csv("rh_map.csv")
Rhino <- Rhino[order(Rhino$count),]

fps <- gcIntermediate(Rhino[,c('c1long', 'c1lat')], Rhino[,c('c2long', 'c2lat')], n=100, breakAtDateLine = T, addStartEnd=T, sp = T)
fps <- as(fps, "SpatialLinesDataFrame")
fps.ff <- fortify(fps)

Rhino$id <-as.character(c(1:nrow(Rhino))) # that rts.ff$id is a char
gcir <- merge(fps.ff, Rhino, all.x=T, by="id") # join attributes, we keep them all, just in case

## make more lines for count > 1 

new_data <- gcir[gcir$count > 1,]

new_lines_data <- NULL
groups <- unique(new_data$group)

for(i in 1:length(groups)){
  group <- groups[i]
  data <- new_data[new_data$group == group,]
  count <- max(data$count)
  second_data <- NULL
  for(j in 1:count){
    if(j==1){
      next
    }
    
    data_2 <- data
    
    df <- data.frame(long=data_2$long,lat=data_2$lat)
    
    ## first get new latitute line
    df$lat[nrow(df)/2] <- df$lat[nrow(df)/2] + j
    
    ## create the contraints
    start <- c(0, head(df$long,1) ,head(df$lat,1))
    mid <- c(0, df$long[nrow(df)/2], df$lat[nrow(df)/2])
    end <- c(0, tail(df$long,1), tail(df$lat,1))
    cons <- rbind(start,mid,end)
    knots <- c(head(df$long,1),df$long[nrow(df)/2],tail(df$long,1))
    
    ## make like with contrained b-spline
    cob_lat <- cobs(df$long,df$lat,knots=knots,pointwise=cons)
    
    ##plot(cob_lat)
    
    ## this will be the new curved latitute line
    lat_fit <- cob_lat$fitted
    
    data_2$lat <- lat_fit
    
    data_2$group <- paste(group,".",j,sep="") 
    
    second_data <- rbind(second_data,data_2)
  }
  inter_data <- rbind(data,second_data)
  new_lines_data <- rbind(new_lines_data, inter_data)
}

gcir2 <- gcir[gcir$count == 1,]
gcir2 <- rbind(gcir2, new_lines_data)

bubs <- subset(Rhino, select = c(Unique, ulat, ulong, sum))
bubs <- data.frame(bubs[complete.cases(bubs),])


wm <- ggplot(data = map, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  geom_map(map = map, aes(map_id = region), 
           color = "black", 
           fill = "gray75", 
           lwd = .05) + 
  coord_proj("+proj=robin") + ylim(c(-86,86)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey92"),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(0,0,0,0),"lines"))

wm <- wm + geom_point(data = bubs, aes(x = ulong, y = ulat, size = sum), color = "Red", alpha = .9) + 
  scale_size(breaks = c(1,6,11,16), label = c("1","6","11","16")) +
  geom_line(data = gcir2, aes(color = order, group = group), lwd = .7, alpha = .4) + 
  scale_color_distiller(palette = 'Reds', direction = 1, guide = "colorbar", name = "Flight\nPath", 
                        breaks = c(10,80), label = c("Origin" , "Destination")) +
theme(legend.key.height = unit(0.025, "in"),
      legend.position = c(0.5, 0.15),
      legend.justification = c("center", "center"),
      legend.box.just = "center",
      legend.direction = "horizontal",
      legend.background = element_rect(color = "grey20"),
      legend.title = element_blank(),
      legend.text = element_text(family = "Gill Sans MT"))

png(file = "rhinohorn_transmap.png", res=600, width = 10, height = 5, units = "in")

wm

dev.off()


####REPTILES####

map <- map_data("world")
map <- subset(map, region!="Antarctica")

rep <- read.csv("rep_map.csv")
rep <- rep[order(rep$count),]

fps <- gcIntermediate(rep[,c('c1long', 'c1lat')], rep[,c('c2long', 'c2lat')], n=100, breakAtDateLine = T, addStartEnd=T, sp = T)
fps <- as(fps, "SpatialLinesDataFrame")
fps.ff <- fortify(fps)

rep$id <-as.character(c(1:nrow(rep))) # that rts.ff$id is a char
gcir <- merge(fps.ff, rep, all.x=T, by="id") # join attributes, we keep them all, just in case

## make more lines for count > 1 

new_data <- gcir[gcir$count > 1,]

new_lines_data <- NULL
groups <- unique(new_data$group)

for(i in 1:length(groups)){
  group <- groups[i]
  data <- new_data[new_data$group == group,]
  count <- max(data$count)
  second_data <- NULL
  for(j in 1:count){
    if(j==1){
      next
    }
    
    data_2 <- data
    
    df <- data.frame(long=data_2$long,lat=data_2$lat)
    
    ## first get new latitute line
    df$lat[nrow(df)/2] <- df$lat[nrow(df)/2] + j
    
    ## create the contraints
    start <- c(0, head(df$long,1) ,head(df$lat,1))
    mid <- c(0, df$long[nrow(df)/2], df$lat[nrow(df)/2])
    end <- c(0, tail(df$long,1), tail(df$lat,1))
    cons <- rbind(start,mid,end)
    knots <- c(head(df$long,1),df$long[nrow(df)/2],tail(df$long,1))
    
    ## make like with contrained b-spline
    cob_lat <- cobs(df$long,df$lat,knots=knots,pointwise=cons)
    
    ##plot(cob_lat)
    
    ## this will be the new curved latitute line
    lat_fit <- cob_lat$fitted
    
    data_2$lat <- lat_fit
    
    data_2$group <- paste(group,".",j,sep="") 
    
    second_data <- rbind(second_data,data_2)
  }
  inter_data <- rbind(data,second_data)
  new_lines_data <- rbind(new_lines_data, inter_data)
}

gcir2 <- gcir[gcir$count == 1,]
gcir2 <- rbind(gcir2, new_lines_data)

bubs <- subset(rep, select = c(Unique, ulat, ulong, sum))
bubs <- data.frame(bubs[complete.cases(bubs),])

wm <- ggplot(data = map, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  geom_map(map = map, aes(map_id = region), 
           color = "black", 
           fill = "gray75", 
           lwd = .05) + 
  coord_proj("+proj=robin") + ylim(c(-86,86)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey92"),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(0,0,0,0),"lines"))

wm <- wm + geom_point(data = bubs, aes(x = ulong, y = ulat, size = sum), color = "green4", alpha = .9) + 
  scale_size(breaks = c(1,25,54), label = c("1","25","54")) +
  geom_line(data = gcir2, aes(color = order, group = group), lwd = .7, alpha = .4) + 
  scale_color_distiller(palette = 'Greens', direction = 1, guide = "colorbar", name = "Flight\nPath", 
                        breaks = c(10,80), label = c("Origin" , "Destination")) +
theme(legend.key.height = unit(0.025, "in"),
      legend.position = c(0.5, 0.15),
      legend.justification = c("center", "center"),
      legend.box.just = "center",
      legend.direction = "horizontal",
      legend.background = element_rect(color = "grey20"),
      legend.title = element_blank(),
      legend.text = element_text(family = "Gill Sans MT"))

png(file = "reptile_transmap.png", res=600, width = 10, height = 5, units = "in")

wm

dev.off()


####BIRDS####


map <- map_data("world")
map <- subset(map, region!="Antarctica")

bird <- read.csv("bird_map.csv")
bird <- bird[order(bird$count),]

fps <- gcIntermediate(bird[,c('c1long', 'c1lat')], bird[,c('c2long', 'c2lat')], n=100, breakAtDateLine = T, addStartEnd=T, sp = T)
fps <- as(fps, "SpatialLinesDataFrame")
fps.ff <- fortify(fps)

bird$id <-as.character(c(1:nrow(bird))) # that rts.ff$id is a char
gcir <- merge(fps.ff, bird, all.x=T, by="id") # join attributes, we keep them all, just in case

## make more lines for count > 1 

new_data <- gcir[gcir$count > 1,]

new_lines_data <- NULL
groups <- unique(new_data$group)

for(i in 1:length(groups)){
  group <- groups[i]
  data <- new_data[new_data$group == group,]
  count <- max(data$count)
  second_data <- NULL
  for(j in 1:count){
    if(j==1){
      next
    }
    
    data_2 <- data
    
    df <- data.frame(long=data_2$long,lat=data_2$lat)
    
    ## first get new latitute line
    df$lat[nrow(df)/2] <- df$lat[nrow(df)/2] + j
    
    ## create the contraints
    start <- c(0, head(df$long,1) ,head(df$lat,1))
    mid <- c(0, df$long[nrow(df)/2], df$lat[nrow(df)/2])
    end <- c(0, tail(df$long,1), tail(df$lat,1))
    cons <- rbind(start,mid,end)
    knots <- c(head(df$long,1),df$long[nrow(df)/2],tail(df$long,1))
    
    ## make like with contrained b-spline
    cob_lat <- cobs(df$long,df$lat,knots=knots,pointwise=cons)
    
    ##plot(cob_lat)
    
    ## this will be the new curved latitute line
    lat_fit <- cob_lat$fitted
    
    data_2$lat <- lat_fit
    
    data_2$group <- paste(group,".",j,sep="") 
    
    second_data <- rbind(second_data,data_2)
  }
  inter_data <- rbind(data,second_data)
  new_lines_data <- rbind(new_lines_data, inter_data)
}

gcir2 <- gcir[gcir$count == 1,]
gcir2 <- rbind(gcir2, new_lines_data)
gcir2 <- transform(gcir2, group = reorder(group, count))

bubs <- subset(bird, select = c(Unique, ulat, ulong, sum))
bubs <- data.frame(bubs[complete.cases(bubs),])

wm <- ggplot(data = map, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  geom_map(map = map, aes(map_id = region), 
           color = "black", 
           fill = "gray75", 
           lwd = .05) + 
  coord_proj("+proj=robin") + ylim(c(-86,86)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey92"),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(0,0,0,0),"lines"))

wm <- wm + geom_point(data = bubs, aes(x = ulong, y = ulat, size = sum), color = "steelblue3", alpha = .9) + 
  scale_size(breaks = c(1,10,21), label = c("1","10","21")) +
  geom_line(data = gcir2, aes(color = order, group = group), lwd = .7, alpha = .4) + 
  scale_color_distiller(palette = 'Blues', direction = 1, guide = "colorbar", name = "Flight\nPath", 
                        breaks = c(10,80), label = c("Origin" , "Destination")) +
theme(legend.key.height = unit(0.025, "in"),
      legend.position = c(0.5, 0.15),
      legend.justification = c("center", "center"),
      legend.box.just = "center",
      legend.direction = "horizontal",
      legend.background = element_rect(color = "grey20"),
      legend.title = element_blank(),
      legend.text = element_text(family = "Gill Sans MT"))

png(file = "bird_transmap.png", res=600, width = 10, height = 5, units = "in")

wm

dev.off()