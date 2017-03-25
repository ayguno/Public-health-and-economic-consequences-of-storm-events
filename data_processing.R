# Downloading and reading the data:

# takes long time
fileURL = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!exists("./Course_project/storm_data.csv")){
download.file(fileURL,destfile = "./Course_project/storm_data.csv")}

storm_data <- read.csv("./Course_project/storm_data.csv", stringsAsFactors = FALSE)

##############################################
# For easier loading later on, don't put into the rmd
saveRDS(storm_data,"storm_data.rds")
storm_data <- readRDS("storm_data.rds")
##############################################



## When inspecting the EVTYPE variable, we notice that there are several redundancies
# related to inconsistent data entry.

head(levels(factor(storm_data$EVTYPE)),15)

tail(levels(factor(storm_data$EVTYPE)),15)

# We will deal with these by further cleaning the data:

storm_data$EVTYPE <- trimws(storm_data$EVTYPE, which = c("both"))
storm_data$EVTYPE <- toupper(storm_data$EVTYPE)

# I noticed that there are 889 event type combinations remained after cleaning this
# variable. 1 event type is marked with "?".

# QUESTION 1: Across the United States, which types of events
# are most harmful with respect to population health?

# Definition of harm on population health:

# This is represented by FATALITIES and INJURIES variables in the data set.

library(dplyr)
storm_events_health <- data.frame(storm_data %>% group_by(EVTYPE) %>% summarise(count= n(),
                Total_fatalities=sum(FATALITIES), 
                Total_injuries = sum(INJURIES)))
# Top 10 most frequent storm events:

library(ggplot2) 
storm_events_health$EVTYPE = factor(storm_events_health$EVTYPE) 

A <- ggplot(storm_events_health, aes(x= log10(count), y= log10(Total_fatalities)))+
        geom_point()+
        labs(y ="Total Fatalities across the U.S. (Log10)", 
             x = "The number of events observed (Log10)",  
             title = "(A) Fatalities increase with the frequency of events ")+
        geom_smooth(method = "lm")+
        theme_bw()+
        theme(plot.title = element_text(size = 10,face = "bold"),
              axis.title = element_text(size = 9))


counts <- storm_events_health %>% arrange(desc(count))
B<- ggplot(data = head(counts,10),aes(x=log10(count),
                                  y=reorder(EVTYPE,count)))+
        geom_point(size = 3) +
        labs(y ="", 
             x = "The number of events observed (Log10)",  
             title = "(B) 10 most frequent types of storm 
events observed across the U.S. ")+
        theme_bw()+
        theme(plot.title = element_text(size = 10,face = "bold"),
              axis.title = element_text(size = 9),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color='grey60', linetype='dashed'))


fatalities <- storm_events_health %>% arrange(desc(Total_fatalities)) 
C <- ggplot(data = head(fatalities,10),aes(x=Total_fatalities,
                                               y=reorder(EVTYPE,Total_fatalities)))+
        geom_point(size = 3, color = "red") +
        labs(y ="", 
             x = "Total number of fatalities",  
             title = "(C) 10 most deadly storm events 
             across the U.S. ")+
        theme_bw()+
        theme(plot.title = element_text(size = 10,face = "bold"),
              axis.title = element_text(size = 9),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color='grey60', linetype='dashed'))

injuries <- storm_events_health %>% arrange(desc(Total_injuries)) 
D<- ggplot(data = head(injuries,10),aes(x=Total_injuries,
                                               y=reorder(EVTYPE,Total_injuries)))+
        geom_point(size = 3, color= "purple") +
        labs(y ="", 
             x = "Total number of injuries",  
             title = "(D) Storm events that are causing the 
highest number of injuries across the U.S. ")+
        theme_bw()+
        theme(plot.title = element_text(size = 10,face = "bold"),
              axis.title = element_text(size = 9),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color='grey60', linetype='dashed'))



# To combine the ggplots, use the multiplot function 
# from : http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

library(lubridate)
storm_data$BGN_DATE<-mdy_hms(storm_data$BGN_DATE)
storm_data$year <- year(storm_data$BGN_DATE)
storm_tornado_timeline <- storm_data %>% filter(EVTYPE == "TORNADO") %>%
        group_by(year) %>% summarise(count=n(),
                Total_fatalities=sum(FATALITIES), 
                Total_injuries = sum(INJURIES))            

library(reshape2)
melted_tornado <- melt(storm_tornado_timeline, id= "year")
ggplot(data=melted_tornado, aes(x= year, y=value, color = variable))+
        geom_line(size=1.3)+
        theme_bw()


multiplot(A,B,C,D, cols = 2)

# Tornadoes appear as the most harmful storm event with respect to population health.

############################################################################
# QUESTION 2:Across the United States, which types of events have the greatest economic consequences?

# The economic consequences are reflected in the data set by two variables:
# 1. PROPDMG
# 2. CROPDMG

# The related PROPDMGEXP and CROPDMGEXP variables carry the abbreviations that
# note the magnitude of the monetary loss (K: thousands, M: millions, B: billions)

# Both of these variables contain entries that are not defined by the codebook


table(storm_data$PROPDMGEXP)
table(storm_data$CROPDMGEXP)

library(dplyr)

storm_data %>% 
        filter(PROPDMGEXP == " ") %>%
        summarize(property_damage = sum(PROPDMG,na.rm = TRUE))

storm_data %>% 
        filter(CROPDMGEXP == " ") %>%
        summarize(crop_damage = sum(CROPDMG,na.rm = TRUE))

# Therefore, I will clean up the data that retains only K, M, B abbreviations.
# Property damage:
library(dplyr)
storm_events_propdmg <- storm_data %>% 
        filter(PROPDMGEXP == "K" |PROPDMGEXP == "M" | PROPDMGEXP == "B") 

K <- which(storm_events_propdmg$PROPDMGEXP == "K")
M <- which(storm_events_propdmg$PROPDMGEXP == "M")
B <- which(storm_events_propdmg$PROPDMGEXP == "B")

#Calculating the exact property damage:
storm_events_propdmg$PROPD[K] <- storm_events_propdmg$PROPDMG[K]*(10^3)
storm_events_propdmg$PROPD[M] <- storm_events_propdmg$PROPDMG[M]*(10^6)
storm_events_propdmg$PROPD[B] <- storm_events_propdmg$PROPDMG[B]*(10^9)

# Summarizing the total property damage caused by each storm event
storm_events_propdmg_summary <- storm_events_propdmg %>% 
        group_by(EVTYPE) %>% 
        summarise(count = n(),Total_PROPD = sum(PROPD, na.rm= TRUE)) %>%
        arrange(desc(Total_PROPD))

storm_events_propdmg_summary$millon_damage <- (storm_events_propdmg_summary$Total_PROPD) /10^6


# Crop damage:
storm_events_cropdmg <- storm_data %>% 
        filter(CROPDMGEXP == "K" |CROPDMGEXP == "M" | CROPDMGEXP == "B") 

K <- which(storm_events_cropdmg$CROPDMGEXP == "K")
M <- which(storm_events_cropdmg$CROPDMGEXP == "M")
B <- which(storm_events_cropdmg$CROPDMGEXP == "B")

#Calculating the exact crop damage:
storm_events_cropdmg$CROPD[K] <- storm_events_cropdmg$CROPDMG[K]*(10^3)
storm_events_cropdmg$CROPD[M] <- storm_events_cropdmg$CROPDMG[M]*(10^6)
storm_events_cropdmg$CROPD[B] <- storm_events_cropdmg$CROPDMG[B]*(10^9)

# Summarizing the total property damage caused by each storm event
storm_events_cropdmg_summary <- storm_events_cropdmg %>% 
        group_by(EVTYPE) %>% summarise(count = n(), 
                Total_CROPD = sum(CROPD, na.rm = TRUE)) %>%
                arrange(desc(Total_CROPD))

storm_events_cropdmg_summary$millon_damage <- (storm_events_cropdmg_summary$Total_CROPD) /10^6

A <- ggplot(storm_events_propdmg_summary, aes(x= log10(count), y= log10(Total_PROPD)))+
        geom_point()+
        labs(y ="Total property damage across the U.S.($) (Log10)", 
             x = "The number of events observed (Log10)",  
             title = "(A) Property damage increases with the frequency of events ")+
        geom_smooth(method = "lm")+
        theme_bw()+
        theme(plot.title = element_text(size = 10,face = "bold"),
              axis.title = element_text(size = 9))

B <- ggplot(storm_events_cropdmg_summary, aes(x= log10(count), y= log10(Total_CROPD)))+
        geom_point()+
        labs(y ="Total crop damage across the U.S.($) (Log10)", 
             x = "The number of events observed (Log10)",  
             title = "(B) Crop damage increases with the frequency of events ")+
        geom_smooth(method = "lm")+
        theme_bw()+
        theme(plot.title = element_text(size = 10,face = "bold"),
              axis.title = element_text(size = 9))


C<- ggplot(data = head(storm_events_propdmg_summary,10),aes(x= millon_damage,
                                    y=reorder(EVTYPE,millon_damage)))+
        geom_point(size = 3, color= "blue") +
        labs(y ="", 
             x = "Total property damage across the U.S.(million $)",  
             title = "(C) Storm events that are causing the 
             highest property damage across the U.S. ")+
        theme_bw()+
        theme(plot.title = element_text(size = 10,face = "bold"),
              axis.title = element_text(size = 9),
              axis.text.x = element_text(hjust = TRUE, angle = 45),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color='grey60', linetype='dashed'))

D<- ggplot(data = head(storm_events_cropdmg_summary,10),aes(x=millon_damage, y=reorder(EVTYPE,millon_damage)))+
        geom_point(size = 3, color= "blue") +
        labs(y ="", 
             x = "Total crop damage across the U.S.(million $)",  
             title = "(D) Storm events that are causing the 
             highest crop damage across the U.S. ")+
        theme_bw()+
        theme(plot.title = element_text(size = 10,face = "bold"),
              axis.title = element_text(size = 9),
              axis.text.x = element_text(hjust = TRUE, angle = 45),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color='grey60', linetype='dashed'))

multiplot(A,B,C,D, cols = 2)

# Flood and Drought are the events that have the greatest economic consequences


# Spatial distribution of tornado, flood and drought-related damage

library(openintro)
library(dplyr)

storm_data$statename <- tolower(abbr2state(storm_data$STATE)) # convert state abbreviations to full names to be able to match with the "state" map data below 

tornado_states_summary <- storm_data %>% 
        filter(EVTYPE == "TORNADO" & statename %in% levels(factor(us$region))) %>%
        group_by(statename) %>% summarise(count = n(), 
                                          Total_injuries = sum(INJURIES, na.rm= T),
                                          Total_fatalities = sum (FATALITIES, na.rm= T))

storm_events_propdmg$statename <- tolower(abbr2state(storm_events_propdmg$STATE))

flood_states_summary <- storm_events_propdmg %>% 
        filter(EVTYPE == "FLOOD" & statename %in% levels(factor(us$region))) %>%
        group_by(statename) %>% summarise(count = n(), 
                                          Total_property_damage_million_USD = (sum(PROPD, na.rm= T))/10^6)


storm_events_cropdmg$statename <- tolower(abbr2state(storm_events_cropdmg$STATE))

drought_states_summary <- storm_events_cropdmg %>% 
        filter(EVTYPE == "DROUGHT" & statename %in% levels(factor(us$region))) %>%
        group_by(statename) %>% summarise(count = n(), 
                                          Total_crop_damage_million_USD = (sum(CROPD, na.rm= T))/10^6)

statenames <- data.frame(statename=levels(factor(us$region)))
drought_states_summary <- merge(drought_states_summary,statenames,
                                by="statename", all = T)




us <- map_data("state") # convert state spatial data into a ggplot map_data
# note that this contains 49 states, the matching data above has to be filtered to
# present only these 49 states (therefore the filtering: statename %in% levels(factor(us$region))))


gg <- ggplot() # make an empty plot

# first add the us map_data as the plain U.S. map frame
template <- gg + geom_map(data = us,map = us, aes(x = long, y = lat , map_id = region ),
                    fill = "#ffffff", color = "#ffffff", size = 0.15)

# then add the actual data and provide a nicely contrasted scale_fill_continuous

# Tornado-related death across the U.S
A <- template + geom_map (data = tornado_states_summary, map = us,
                     aes(fill = Total_fatalities, map_id = statename),
                     color="#ffffff", size = 0.3) +
        scale_fill_continuous(low='thistle2', high='darkred', 
                              guide='colorbar', name = "Number of\nfatalities")

# The rest is to polish the map:
A <- A + labs(x=NULL, y=NULL,
                title = "(A) Distribution of fatalities caused by tornadoes\nacross the United States")+
 coord_map("albers", lat0 = 39, lat1 = 45)+ 
 theme(panel.border = element_blank())+
 theme(panel.background = element_blank())+
 theme(axis.ticks = element_blank())+
 theme(axis.text = element_blank())+
 theme(title = element_text(face = "bold",size = 8))  

# Tornado-related injuries across the U.S
B <- template + geom_map (data = tornado_states_summary, map = us,
                          aes(fill = Total_injuries, map_id = statename),
                          color="#ffffff", size = 0.3) +
        scale_fill_continuous(low='thistle2', high='purple4', 
                              guide='colorbar', name = "Number of\ninjuries")

# The rest is to polish the map:
B <- B + labs(x=NULL, y=NULL,
              title = "(B) Distribution of injuries caused by tornadoes\nacross the United States")+
        coord_map("albers", lat0 = 39, lat1 = 45)+ 
        theme(panel.border = element_blank())+
        theme(panel.background = element_blank())+
        theme(axis.ticks = element_blank())+
        theme(axis.text = element_blank())+
        theme(title = element_text(face = "bold",size = 8)) 

# Flood-related property damage across the U.S:
C <- template + geom_map (data = flood_states_summary, map = us,
                          aes(fill = log10(Total_property_damage_million_USD), map_id = statename),
                          color="#ffffff", size = 0.3) +
        scale_fill_continuous(low='lightsteelblue2', high='navy', 
                              guide='colorbar', name = "Property damage\nMillion $ (Log10)")

# The rest is to polish the map:
C <- C + labs(x=NULL, y=NULL,
              title = "(C) Distribution of flood-related property damage\nacross the United States")+
        coord_map("albers", lat0 = 39, lat1 = 45)+ 
        theme(panel.border = element_blank())+
        theme(panel.background = element_blank())+
        theme(axis.ticks = element_blank())+
        theme(axis.text = element_blank())+
        theme(title = element_text(face = "bold",size = 8)) 

# Drought-related crop damage across the U.S:
D <- template + geom_map (data = drought_states_summary, map = us,
                          aes(fill = log10(Total_crop_damage_million_USD), map_id = statename),
                          color="#ffffff", size = 0.3) +
        scale_fill_continuous(low='peachpuff', high='darkorange2', 
                              guide='colorbar', name = "Crop damage\nMillion $")

# The rest is to polish the map:
D <- D + labs(x="Grey: no drought-related crop damage recorded", y=NULL,
              title = "(D) Distribution of drought-related crop damage\nacross the United States")+
        coord_map("albers", lat0 = 39, lat1 = 45)+ 
        theme(panel.border = element_blank())+
        theme(panel.background = element_blank())+
        theme(axis.ticks = element_blank())+
        theme(axis.text = element_blank())+
        theme(title = element_text(face = "bold",size = 8)) 



multiplot(A,C,B,D, cols = 2)