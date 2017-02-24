setwd("E:/ROUTES/Year One Airport Seizure Analysis/Finalized")

library(ggplot2)
library(dplyr)
library(extrafont)
library(reshape2)

tot <- read.csv("tot.csv")

dbref <- c("Ivory", "Rhino Horn", "Reptiles", "Birds")
revdbref <- rev(dbref)

colors <- c("darkorange", "red", "green4", "steelblue3")
revcolors <- rev(colors)

sc <- c("Origin", "Transit", "Destination")
revsc <- rev(sc)

##AIRPORT TOTALS##

tot$Database <- factor(tot$Database, levels = dbref)

airports <- data.frame(table(tot$Database, tot$Location, exclude = ""))
names(airports) <- c("db","airport","Freq")

sums <- aggregate(airports$Freq, by = list(airport = airports$airport), FUN = sum)
sums <- subset(sums, x > 4)

airports <- subset(airports, airport %in% sums$airport)
airports <- dplyr::full_join(sums,airports)
airports <- transform(airports, airport = reorder(airport, x))

gg <- ggplot(data = airports, aes(x = airport, y = Freq, fill = factor(db, levels=revdbref))) + 
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() + coord_flip() +
  scale_fill_manual(values = revcolors, breaks = c("Ivory", "Rhino Horn", "Reptiles", "Birds")) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = .75),
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 12, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12, color = "black"))

ggsave("airportseizuredb_routes.pdf", gg, device = cairo_pdf, width = 11, height = 8.5, units = "in")

##TRANSIT##

trans <- read.csv("transittots.csv")
trans[trans == ""] <- NA

count <- trans %>% select(ID, Date, Database, Seizure.Country, Origin.Country1, Origin.Country2, Origin.Country3, Transit.Country1, Transit.Country2, Transit.Country3, Transit.Country4, Destination.Country1, Destination.Country2, Destination.Country3)
count <- subset(count[,4:14])
uni <- NULL

for (j in 1:length(count)){
  nat <- count[j]
  natu <- unique(nat)
  names(natu) <- c("country")
  uni <- rbind(natu, uni)
}

countries <- unique(uni)
countries <- data.frame(countries[!is.na(countries$country),])
names(countries) <- c("country")

for (i in 1:length(count)){
  data <- count[i]
  name <- names(data)
  data <- data.frame(table(data, exclude = c(NA, "")))
  names(data) <- c("country", name)
  countries <- merge(countries, data, by = "country", all=T)
}

countries$Origin <- rowSums(countries[,3:5], na.rm = T)
countries$Transit <- rowSums(countries[,6:9], na.rm = T)
countries$Destination <- rowSums(countries[10:12], na.rm = T)
countries$sums <- rowSums(countries[13:15], na.rm = T)

countries[is.na(countries)] <- 0

transitgraph <- data.frame(subset(countries, sums > 15))
transitgraph <- transform(transitgraph, country = reorder(country, sums))
transitgraph <- dplyr::select(transitgraph, country, Origin, Transit, Destination)

require(reshape2)

stack <- melt(data = transitgraph, id = "country")

require(ggplot2)

gg <- ggplot(data = stack, aes(x = country, y = value, fill = factor(variable, levels = rev(levels(variable))))) + 
  geom_bar(stat = "identity") + 
  theme_bw() + coord_flip() +
  scale_fill_brewer(palette = 'Set1', direction = -1, breaks = levels(stack$variable)) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = .75), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 15, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15, color = "black"))

ggsave("countrytrans_routes.pdf", gg, device = cairo_pdf, width = 11, height = 8.5, units = "in")

####IVORY####

#Transit#

trans <- read.csv("transittots.csv")

trans <- data.frame(subset(trans, Database == "Ivory"))

countrylist <- data.frame(trans$Unique)
countrylist <- data.frame(unique(countrylist$trans.Unique))

names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- seiz[complete.cases(seiz),]
seiz <- subset(seiz, Freq >0)
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(subset(orig, select = c("country", "Origin")))
orig <- subset(orig, Origin > 0)

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))
transit <- subset(transit, Transit > 0)

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))
dest <- subset(dest, Destination > 0)

transitgraph <- merge(merge(orig, transit, by = "country", all = T), dest, by = "country", all = T)
transitgraph[is.na(transitgraph)] <- 0
transitgraph$sum <- rowSums(transitgraph[,2:4])
transitgraph <- data.frame(subset(transitgraph, sum > 0))

transitgraphmelt <- data.frame(subset(transitgraph, sum > 5))
transitgraphmelt <- arrange(transitgraphmelt, -sum)
transitgraphmelt <- transform(transitgraphmelt, country = reorder(country, sum))

melted <- melt(data = transitgraphmelt[,1:4], id = "country")

gg <- ggplot(data = melted, aes(x = country, y = value, fill = factor(variable, levels = revsc))) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = 'Oranges', breaks = sc, direction = -1) + 
  theme_bw() + 
  coord_flip() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.major.x = element_line(size = .75))

ggsave("ivcountrytrans_routes.pdf", gg, device = cairo_pdf, width = 5, height = 4, units = "in")

##top airports## 

tot <- read.csv("tot.csv")

ivory <- data.frame(subset(tot, Database == "Ivory"))
ivory <- transform(ivory, Weight..kg. = as.numeric(as.character(Weight..kg.)))

air <- data.frame(subset(ivory, select = c("Year", "Location")))
air <- data.frame(table(air$Location))
air[air == 0] <- NA
air <- air[complete.cases(air),]
names(air) <- c("airport", "sum")
airports <- subset(air, sum>3)

gg <- ggplot(airports, aes(x = reorder(airport, sum), y = (sum))) +
  geom_bar(stat = "identity", color = "black", fill = "darkorange", width = .85, size = .4) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = .75))

ggsave("ivairportseizures_routes.pdf", gg, device = cairo_pdf, width = 5, height = 4, units = "in")

####RHINO HORN####

#Transit#

trans <- read.csv("transittots.csv")

trans <- data.frame(subset(trans, Database == "Rhino Horn"))

countrylist <- data.frame(trans$Unique)
names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- data.frame(seiz[!is.na(seiz$country),])
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(orig[!is.na(orig$country),])
orig <- data.frame(subset(orig, select = c("country", "Origin")))

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))

transitgraph <- merge(merge(orig, transit, by = "country", all = T), dest, by = "country", all = T)
transitgraph[is.na(transitgraph)] <- 0
transitgraph$sum <- rowSums(transitgraph[,2:4])
transitgraph <- data.frame(subset(transitgraph, sum > 0))

transitgraphmelt <- data.frame(subset(transitgraph, sum > 2))
transitgraphmelt <- arrange(transitgraphmelt, -sum)
transitgraphmelt <- transform(transitgraphmelt, country = reorder(country, sum))

melted <- melt(data = transitgraphmelt[,1:4], id = "country")

gg <- ggplot(data = melted, aes(x = country, y = value, fill = factor(variable, levels = revsc))) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette = 'Reds', direction = -1, breaks = sc) + 
  theme_bw() + coord_flip() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.major.x = element_line(size = .75))

ggsave("rhcountrytrans_routes.pdf", gg, device = cairo_pdf, width = 5, height = 4, units = "in")

#Airports#

tot <- read.csv("tot.csv")

tot$Weight..kg. <- as.numeric(as.character(tot$Weight..kg.))

rhino <- data.frame(subset(tot, Database == "Rhino Horn"))

air <- data.frame(subset(rhino, select = c("Year", "Location")))
air <- data.frame(table(air$Location))
air[air == 0] <- NA
names(air) <- c("airport", "sum")
air <- air[!is.na(air$sum),]
airports <- subset(air, sum>2)

gg <- ggplot(airports, aes(x = reorder(airport, sum), y = (sum))) +
  geom_bar(stat = "identity", color = "black", fill = "red", width = .85, size = .4) +
  scale_y_continuous(limits = c(0, 12), breaks = c(0,3,6,9,12)) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = .75),
        panel.grid.major.y = element_blank())

ggsave("rhairportseizures_routes.pdf", gg, device = cairo_pdf, width = 5, height = 4, units = "in")

####REPTILES####

#Transit#

trans <- read.csv("transittots.csv")

trans <- data.frame(subset(trans, Database == "Reptiles"))

countrylist <- data.frame(trans$Unique)
names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- data.frame(seiz[!is.na(seiz$country),])
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(orig[!is.na(orig$country),])
orig <- data.frame(subset(orig, select = c("country", "Origin")))

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))

transitgraph <- merge(merge(orig, transit, by = "country", all = T), dest, by = "country", all = T)
transitgraph[is.na(transitgraph)] <- 0
transitgraph$sum <- rowSums(transitgraph[,2:4])
transitgraph <- data.frame(subset(transitgraph, sum > 0))

transitgraphmelt <- data.frame(subset(transitgraph, sum > 5))
transitgraphmelt <- arrange(transitgraphmelt, -sum)
transitgraphmelt <- transform(transitgraphmelt, country = reorder(country, sum))

melted <- melt(data = transitgraphmelt[,1:4], id = "country")

gg <- ggplot(data = melted, aes(x = country, y = value, fill = factor(variable, levels = revsc))) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette = 'Greens', direction = -1, breaks = sc) + 
  theme_bw() + coord_flip() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.major.x = element_line(size = .75))

ggsave("repcountrytrans_routes.pdf", gg, device = cairo_pdf, width = 5, height = 4, units = "in")

#Airports#

tot <- read.csv("tot.csv")

rep <- data.frame(subset(tot, Database == "Reptiles"))

air <- data.frame(subset(rep, select = c("Year", "Location")))
air <- data.frame(table(air$Location))
names(air) <- c("airport", "sum")
airports <- subset(air, sum>2)
airports <- airports[!airports$airport == "",]

gg <- ggplot(airports, aes(x = reorder(airport, sum), y = (sum))) +
  geom_bar(stat = "identity", width = .85, size = .4, color = "black", fill = "green4") +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = .75))

ggsave("repairportseizures_routes.pdf", gg, device = cairo_pdf, width = 5, height = 4, units = "in")

####BIRDS####

#Transit#

trans <- read.csv("transittots.csv")

trans <- data.frame(subset(trans, Database == "Birds"))

countrylist <- data.frame(trans$Unique)
names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- data.frame(seiz[!is.na(seiz$country),])
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(orig[!is.na(orig$country),])
orig <- data.frame(subset(orig, select = c("country", "Origin")))

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))

transitgraph <- merge(merge(orig, transit, by = "country", all = T), dest, by = "country", all = T)
transitgraph[is.na(transitgraph)] <- 0
transitgraph$sum <- rowSums(transitgraph[,2:4])
transitgraph <- data.frame(subset(transitgraph, sum > 0))

transitgraphmelt <- data.frame(subset(transitgraph, sum > 3))
transitgraphmelt <- transform(transitgraphmelt, country = reorder(country, sum))

melted <- melt(data = transitgraphmelt[,1:4], id = "country")

gg <- ggplot(data = melted, aes(x = country, y = value, fill = factor(variable, levels = revsc))) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette = 'Blues', direction = -1, breaks = sc) + 
  theme_bw() + coord_flip() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        axis.title = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.major.x = element_line(size = .75),
        axis.text = element_text(family = "Gill Sans MT", size = 11))

ggsave("birdcountrytrans_routes.pdf", gg, device = cairo_pdf, width = 5, height = 4, units = "in")

#Airports#

tot <- read.csv("tot.csv")

bird <- data.frame(subset(tot, Database == "Birds"))

air <- data.frame(subset(bird, select = c("Year", "Location")))
air <- data.frame(table(air$Location))
air[air == 0] <- NA
air <- air[complete.cases(air),]
names(air) <- c("airport", "sum")
air <- subset(air, sum>2)

gg <- ggplot(air, aes(x = reorder(airport, sum), y = (sum))) +
  geom_bar(stat = "identity", width = .85, size = .4 , color = "black", fill = "steelblue3") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(limits = c(0, 15), breaks = c(0,5,10,15)) +
  theme(axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = .75))

ggsave("birdairportseizures_routes.pdf", gg, device = cairo_pdf, width = 5, height = 4, units = "in")
