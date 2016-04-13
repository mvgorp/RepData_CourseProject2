# Load dplyr (and suppress messages)
library(dplyr)
library(ggplot2)
library(tidyr)

#
dataurl = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
datafile = 'stormdata.csv.bz2'
if(!file.exists(datafile)) { download.file(dataurl,datafile) }

#
if(!exists("data")) { 
    data = read.csv(datafile, header = TRUE, sep=",") 
}

# Select only relevant columns
subdata = data %>% select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

# Group by event
subdata_by_event = subdata %>% group_by(EVTYPE)

###
health = subdata_by_event %>% 
    summarise_each(funs(sum), FATALITIES, INJURIES) %>% 
    mutate(TOTAL = (FATALITIES + INJURIES)) %>%
    arrange(desc(TOTAL)) %>%
    top_n(10)

#
health$EVTYPE <- factor(health$EVTYPE, levels = health$EVTYPE[order(health$TOTAL)])

g = ggplot(data = health, aes(x=EVTYPE)) + xlab("Weather Event") +
    geom_bar(stat="identity", aes(y=INJURIES, fill="Injuries")) +
    geom_bar(stat="identity", aes(y=FATALITIES, fill = "Fatalities")) +
    scale_fill_discrete(name="Type") + ylab("Number of Fatalities and Injuries") +
    coord_flip() +
    ggtitle("Top 10 Weather Events with the most Fatalities and Injuries")

#print(g)


###

econom = subdata_by_event %>% 
    summarise_each(funs(sum), PROPDMG, CROPDMG) %>% 
    mutate(TOTAL = (PROPDMG + CROPDMG)) %>%
    arrange(desc(TOTAL)) %>%
    top_n(10)

library(reshape2)

#
econom$EVTYPE <- factor(econom$EVTYPE, levels = econom$EVTYPE[order(econom$TOTAL)])

#g = ggplot(data = econom, aes(x=EVTYPE)) + xlab("Weather Event") +
#    geom_bar(stat="identity", aes(y=PROPDMG, fill="Property Damage")) +
#    geom_bar(stat="identity", aes(y=CROPDMG, fill = "Crop Damage")) +
#    scale_fill_discrete(name="Type") + ylab("Damage in Dollars") +
#    coord_flip() +
#    ggtitle("Top 10 Weather Events with the most damage")

econom_long = melt(econom)
g = ggplot(econom_long, aes(x = EVTYPE, y = value, fill = variable)) + geom_bar(stat='identity')

print(g)


    
    
    