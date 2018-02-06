# Corn.R
# R script to analyze combine data from Professor Al Hansen
# by Robert A. Stevens, 8/30/13

library(ggplot2) # Required for plot routines
library(maps)    # Required for correct Lon/Lat projections for ggplot2 coord_map()
library(plyr)    # Required for sorting dataframe to make sure in the correct order
library(ggmap)
library(rpart)
library(rpart.plot)
library(rattle)

# http://ryouready.wordpress.com/2009/02/17/r-good-practice-adding-footnotes-to-graphics/
###############################################################
##                                                           ##
##      R: Good practice - adding footnotes to graphics      ##
##                                                           ##
###############################################################

# basic information at the beginning of each script
sourceText <- "Source: 'Modeling and Analysis of Row Crop Harvesting Patterns by Combines' by Hansen et al."
scriptName <- "Script: Corn.R"
author <- "RAStevens"
footnote <- paste(sourceText, scriptName, format(Sys.time(), "%d %b %Y"), author, sep=" / ")

# default footnote is today's date, cex=.7 (size) and color is a kind of grey

makeFootnote <- function(footnoteText = format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

makeFootnote(footnote)

## Example ##
plot(1:10)
makeFootnote(footnote)

###############################################################


setwd("C:\\Users\\rs62172\\Documents\\Corn\\")

filename <- "Corn-harvesting-CAN.csv"
filedata <- read.csv(filename, header=TRUE, as.is = TRUE)

str(filedata)
names(filedata)

# 12013 obs. of  19 variables:
#  1. LONGITUDE
#  2. LATITUDE
#  3. Easting
#  4. Northing
#  5. TIME
#  6. STATUS
#  7. DRY_YIELD
#  8. DAYOFMONTH
#  9. HOUR
# 10. MINUTE
# 11. SECOND
# 12. SPEED
# 13. TORQUE
# 14. ENG_SPD
# 15. CURVE
# 16. FUEL
# 17. FUEL_KG.HR
# 18. GRD_SPD
# 19. MPH

# Plot each individual GPS/CAN channel
plotMtgVar <- function(MTGdata) {
  # Go through all columns and plot their data
  for (i in 1:length(MTGdata)) {
    png(paste(filename, "-Plot-", i, "-", names(MTGdata[i]), ".png", sep=""), width = 1024, height = 768)
    plot(MTGdata[, i], xlab = "", ylab = names(MTGdata[i]), 
           main = paste(filename, " : ", names(MTGdata[i]), sep=""), type = "l")
    print(paste("plot ", names(MTGdata[i]), ": ok"))
    makeFootnote(footnote)
    dev.off()
  }
}

plotMtgVar(filedata) # plot each individual GPS/CAN channel

# Plot overall path to check
plotMtgPath <- function(MTGdata) {
  # plot path with color code based on Ground Speed
  png(paste(filename, ".png", sep=""), width=1024, height=768)
  p <- qplot(LONGITUDE, LATITUDE, data=MTGdata, geom="point", color=GRD_SPD, size=I(2)) +
        scale_x_continuous('Longitude') + 
        scale_y_continuous('Latitude') + 
        coord_map() +
        labs(title = paste(filename)) +
        scale_colour_gradientn(colours = rainbow(7))
  print(p)
  makeFootnote(footnote)
  dev.off()
}

plotMtgPath(filedata) # plot path


# repeat for Combine 2

filename <- "Corn-Harvest-2-CAN.csv"
filedata <- read.csv(filename, header=TRUE, as.is = TRUE)

str(filedata)
names(filedata)

# 11824 obs. of  18 variables
# Combine 1 had 12013 obs. of 19 variables (Easting and Northing, but not TIMELAPSE):
#  1. LONGITUDE
#  2. LATITUDE
#  3. TIME
#  4. STATUS
#  5. DRY_YIELD
#  6. DAYOFMONTH
#  7. HOUR
#  8. MINUTE
#  9. SECOND
# 10. SPEED
# 11. TIMELAPSE - New!
# 12. TORQUE
# 13. ENG_SPD
# 14. CURVE
# 15. FUEL
# 16. FUEL_KG.HR
# 17. GRD_SPD
# 18. MPH

plotMtgVar(filedata) # plot each individual GPS/CAN channel
plotMtgPath(filedata) # plot path
# Removed 305 rows containing missing values (geom_point).
11824 - 11519
# 11824 obs, but CSV had 11519 rows, difference = 305 - must have read blank rows
# delete them and try again

filename <- "Corn-Harvest-2-CAN.csv"
filedata <- read.csv(filename, header=TRUE, as.is = TRUE)

str(filedata)
names(filedata)

plotMtgVar(filedata) # plot each individual GPS/CAN channel
plotMtgPath(filedata) # plot path

# Al said # 2 (?) is backwards - rows in reverse order (check the times)

head(filedata[,c(3, 6:9)])
filedata <- arrange(filedata, DAYOFMONTH, HOUR, MINUTE, SECOND) # put in the right time order
head(filedata[,c(3, 6:9)])

plotMtgVar(filedata) # plot each individual GPS/CAN channel
plotMtgPath(filedata) # plot path

filename <- "Corn-harvesting-CAN.csv"
Combine1 <- read.csv(filename, header=TRUE, as.is = TRUE)
filename <- "Corn-Harvest-2-CAN.csv"
Combine2 <- read.csv(filename, header=TRUE, as.is = TRUE)
Combine2 <- arrange(Combine2, DAYOFMONTH, HOUR, MINUTE, SECOND) # put in the right time order

Combine1 <- Combine1[ , -c(3:4)] # remove Easting and Northing
dim(Combine1)
names(Combine1)
Combine2 <- Combine2[ , -c(11)] # remove TIMELAPSE
dim(Combine2)
names(Combine2)

Combine1$Combine <- 1
Combine2$Combine <- 2

Combine12 <- rbind(Combine1, Combine2)
names(Combine12)
dim(Combine12)

# Plot overall path to check - with color variable as an input
Combine12$Combine <- as.factor(Combine12$Combine)
plotTitle <- "Combine 1 and Combine 2 Paths"
png(paste(plotTitle, ".png", sep=""), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine12, geom="point", color=Combine, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle))
makeFootnote(footnote)
dev.off()

plotTitle <- "Combine 1 and Combine 2 Paths using ggmap with zoom = 17" # changed zoom value for each plot
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
x <- mean(Combine12$LONGITUDE)
y <- mean(Combine12$LATITUDE)
map <- get_googlemap(center=c(lon=x, lat=y), zoom=17, maptype='hybrid') # changed zoom value for each plot
ggmap(map) + 
  geom_path(data=Combine12, aes(x=LONGITUDE, y=LATITUDE, color=Combine), size=I(1)) +
  labs(title = paste(plotTitle))
makeFootnote(footnote)
dev.off()


plotTitle <- "Harvest Pattern for Combine 1 and Combine 2 with MPH"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine12, geom="point", color=MPH, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine) +
  scale_colour_gradientn(colours = rainbow(7))
makeFootnote(footnote)
dev.off()

# plot histogram of Ground Speed
plotTitle <- "Histograms for MPH"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(as.numeric(MPH), data=Combine12, geom="histogram", binwidth=0.25) +
  scale_x_continuous('Ground Speed (MPH)') + 
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine)
makeFootnote(footnote)
dev.off()

plotTitle <- "Histograms for MPH Overlaid"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
ggplot(Combine12, aes(x=MPH)) + 
  geom_histogram(data=subset(Combine12, Combine == 1), fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(Combine12, Combine == 2), fill = "blue", alpha = 0.2) +
  labs(title = paste(plotTitle, "(1 = Red, 2 = Blue)")) + 
  theme_bw()
makeFootnote(footnote)
dev.off()



# Figure 5 in paper (2007)
Combine1$CURVE <- as.factor(Combine1$CURVE)
plotTitle <- "Harvest Pattern for Lead Combine 1 with CURVE"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine1, geom="point", color=CURVE, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle))
makeFootnote(footnote)
dev.off()

# Figure 6 in paper (2007)
Combine2$CURVE <- as.factor(Combine2$CURVE)
plotTitle <- "Harvest Pattern for Follow Combine 2 with CURVE"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine2, geom="point", color=CURVE, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle))
makeFootnote(footnote)
dev.off()

plotTitle <- "Harvest Pattern for Combine 1 and Combine 2 with CURVE"
Combine12$CURVE <- as.factor(Combine12$CURVE)
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine12, geom="point", color=CURVE, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine)
makeFootnote(footnote)
dev.off()


# Look at Status for Combine 1
Combine1$STATUS <- as.factor(Combine1$STATUS)
plotTitle <- "Harvest Pattern for Lead Combine 1 with STATUS"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine1, geom="point", color=STATUS, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle))
makeFootnote(footnote)
dev.off()

# Look at Status for Combine 2
Combine2$STATUS <- as.factor(Combine2$STATUS)
plotTitle <- "Harvest Pattern for Follow Combine 2 with STATUS"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine2, geom="point", color=STATUS, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle))
makeFootnote(footnote)
dev.off()

plotTitle <- "Harvest Pattern for Combine 1 and Combine 2 with STATUS"
Combine12$STATUS <- as.factor(Combine12$STATUS)
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine12, geom="point", color=STATUS, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine)
makeFootnote(footnote)
dev.off()


# plot histogram of DRY_YIELD
plotTitle <- "Histograms for DRY_YIELD"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(as.numeric(DRY_YIELD), data=Combine12, geom="histogram", binwidth=10) +
  scale_x_continuous('DRY_YIELD (bu/ac)') + 
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine)
makeFootnote(footnote)
dev.off()

plotTitle <- "Histograms for DRY_YIELD Overlaid"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
ggplot(Combine12, aes(x=DRY_YIELD)) + 
  geom_histogram(data=subset(Combine12, Combine == 1), fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(Combine12, Combine == 2), fill = "blue", alpha = 0.2) +
  labs(title = paste(plotTitle, "(1 = Red, 2 = Blue)")) + 
  theme_bw()
makeFootnote(footnote)
dev.off()

plotTitle <- "Harvest Pattern for Combine 1 and Combine 2 with DRY_YIELD"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine12, geom="point", color=DRY_YIELD, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine) +
  scale_colour_gradientn(colours = rainbow(7))
makeFootnote(footnote)
dev.off()


# plot histogram of ENG_SPD
plotTitle <- "Histograms for ENG_SPD"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(as.numeric(ENG_SPD), data=Combine12, geom="histogram", binwidth=10) +
  scale_x_continuous('ENG_SPD (rpm)') + 
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine) +
  xlim(2100, 2500)
makeFootnote(footnote)
dev.off()

plotTitle <- "Histograms for ENG_SPD Overlaid"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
ggplot(Combine12, aes(x=ENG_SPD)) + 
  geom_histogram(data=subset(Combine12, Combine == 1), fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(Combine12, Combine == 2), fill = "blue", alpha = 0.2) +
  labs(title = paste(plotTitle, "(1 = Red, 2 = Blue)")) + 
  theme_bw() +
  xlim(2100, 2500)
makeFootnote(footnote)
dev.off()

plotTitle <- "Harvest Pattern for Combine 1 and Combine 2 with ENG_SPD"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine12, geom="point", color=ENG_SPD, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine) +
  scale_colour_gradientn(colours = rainbow(7))
makeFootnote(footnote)
dev.off()


# plot histogram of TORQUE
plotTitle <- "Histograms for TORQUE"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(as.numeric(TORQUE), data=Combine12, geom="histogram", binwidth=10) +
  scale_x_continuous('TORQUE (% of torque at rated)') + 
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine)
makeFootnote(footnote)
dev.off()

plotTitle <- "Histograms for TORQUE Overlaid"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
ggplot(Combine12, aes(x=TORQUE)) + 
  geom_histogram(data=subset(Combine12, Combine == 1), fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(Combine12, Combine == 2), fill = "blue", alpha = 0.2) +
  labs(title = paste(plotTitle, "(1 = Red, 2 = Blue)")) + 
  theme_bw()
makeFootnote(footnote)
dev.off()

plotTitle <- "Harvest Pattern for Combine 1 and Combine 2 with TORQUE"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine12, geom="point", color=TORQUE, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine) +
  scale_colour_gradientn(colours = rainbow(7))
makeFootnote(footnote)
dev.off()


# Look at FUEL_KG.HR for Combine 1 - use for data mining
plotTitle <- "Harvest Pattern for Lead Combine 1 with Fuel Rate"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine1, geom="point", color=FUEL_KG.HR, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle)) +
  scale_colour_gradientn(colours = rainbow(7))
makeFootnote(footnote)
dev.off()

# Look at FUEL_KG.HR for Combine 2 - use for data mining
plotTitle <- "Harvest Pattern for Follow Combine 2 with Fuel Rate"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine2, geom="point", color=FUEL_KG.HR, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle)) +
  scale_colour_gradientn(colours = rainbow(7))
makeFootnote(footnote)
dev.off()

plotTitle <- "Harvest Pattern for Combine 1 and Combine 2 with FUEL_KG.HR"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(LONGITUDE, LATITUDE, data=Combine12, geom="point", color=FUEL_KG.HR, size=I(2)) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') + 
  coord_map() +
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine) +
  scale_colour_gradientn(colours = rainbow(7))
makeFootnote(footnote)
dev.off()

# plot histogram of FUEL_KG.HR
plotTitle <- "Histograms for FUEL_KG.HR"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(as.numeric(FUEL_KG.HR), data=Combine12, geom="histogram", binwidth=10) +
  scale_x_continuous('FUEL_KG.HR') + 
  labs(title = paste(plotTitle)) +
  facet_grid(. ~ Combine)
makeFootnote(footnote)
dev.off()

plotTitle <- "Histograms for FUEL_KG.HR Overlaid"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
ggplot(Combine12, aes(x=FUEL_KG.HR)) + 
  geom_histogram(data=subset(Combine12, Combine == 1), fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(Combine12, Combine == 2), fill = "blue", alpha = 0.2) +
  labs(title = paste(plotTitle, "(1 = Red, 2 = Blue)")) + 
  theme_bw()
makeFootnote(footnote)
dev.off()



qplot(FUEL, FUEL_KG.HR, data=Combine1) + labs(title = "Combine 1")
qplot(FUEL, FUEL_KG.HR, data=Combine2) + labs(title = "Combine 2")
qplot(FUEL, FUEL_KG.HR, data=Combine12) + facet_grid(. ~ Combine)
qplot(FUEL, FUEL_KG.HR, data=Combine12, alpha=I(1/10)) + 
  facet_grid(. ~ Combine) +
  theme_bw()
qplot(FUEL, FUEL_KG.HR, data=Combine12, alpha=I(1/20)) + 
  facet_grid(. ~ Combine) +
  theme_bw()

qplot(SPEED, GRD_SPD, data=Combine12) + 
  facet_grid(. ~ Combine) +
  theme_bw()
qplot(SPEED, MPH, data=Combine12) + 
  facet_grid(. ~ Combine) +
  theme_bw()
qplot(GRD_SPD, MPH, data=Combine12) + 
  facet_grid(. ~ Combine) +
  theme_bw()
# GRD_SPD = MPH, just different units:  GRD_SPD in km/h, MPH in mph (like SPEED)

# when done with graphs, try some data mining

FuelRate.rp <- rpart(FUEL_KG.HR ~ STATUS + DRY_YIELD + TORQUE + ENG_SPD + CURVE + MPH + Combine, 
                     data = Combine12, cp = 1e-3)

plotTitle <- "rpart cp for Fuel Rate"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
plotcp(FuelRate.rp, main = plotTitle)
makeFootnote(footnote)
dev.off()

#FuelRate.rpp <- prune(FuelRate.rp, cp = 0.015) # no need to prune
#FuelRate.rpp
FuelRate.rp

plotTitle <- "rpart Results for Fuel Rate"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
plot(FuelRate.rp, main = plotTitle)
text(FuelRate.rp, digits=4, cex=0.65)
makeFootnote(footnote)
dev.off()

plotTitle <- "rpart Results for Fuel Rate with prp"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
prp(FuelRate.rp, main = plotTitle)
makeFootnote(footnote)
dev.off()

fancyRpartPlot(FuelRate.rp) # Error: extra=104 is illegal (for method="anova")

plotTitle <- "rpart Results for Fuel Rate with prp Enhanced"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
prp(FuelRate.rp, type=2, nn=TRUE, fallen.leaves=TRUE,
    faclen=0, varlen=0, shadow.col="grey", branch.lty=3, main = plotTitle)
makeFootnote(footnote)
dev.off()

plotTitle <- "ENG_SPD versus TORQUE by CURVE"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(ENG_SPD, TORQUE, data=Combine12, color=FUEL_KG.HR, alpha=I(1/4), size=I(4)) + 
  facet_grid(. ~ CURVE) +
  labs(title = paste(plotTitle)) +
  scale_colour_gradientn(colours = rainbow(7)) +
  theme_bw()
makeFootnote(footnote)
dev.off()

plotTitle <- "ENG_SPD versus TORQUE by CURVE and Combine"
png(paste(plotTitle, "png", sep="."), width=1024, height=768)
qplot(ENG_SPD, TORQUE, data=Combine12, color=FUEL_KG.HR, alpha=I(1/4), size=I(4)) + 
  facet_grid(Combine ~ CURVE) +
  labs(title = paste(plotTitle)) +
  scale_colour_gradientn(colours = rainbow(7)) +
  theme_bw()
makeFootnote(footnote)
dev.off()

rm(list = ls()) # optional - remove objects to save space
