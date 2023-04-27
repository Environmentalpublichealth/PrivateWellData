# set working directory
setwd("~/Desktop/Jiali/TAMU/Susie/welldata/")

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("devtools")
library(devtools)
# install_github('arilamstein/choroplethrZip@v1.5.0')
library(readxl)
library(ggplot2)
library(choroplethrZip)
library(RColorBrewer)

# # practice with example data
# xlsx_example <- readxl_example("datasets.xls")
# 
# read_excel(xlsx_example) # read excel file
# excel_sheets(xlsx_example) # show tabs
# # specify a tab
# read_excel(xlsx_example, sheet = "chickwts")
# tab4 <- read_excel(xlsx_example, sheet = 4)
# -------------------------end practise------------------
# load response tracking table
fileName <- "Recruitment Response Rate Tracking_2022-06-05.xlsx"
excel_sheets(fileName)
Tracking <- read_excel(fileName, sheet = 1)
#County <- read_excel(fileName, sheet = 2)
#Projection <- read_excel(fileName, sheet = 3)
# load questionare table
Questionare <- read.csv("Engagement data for manuscript-questionare.csv", header = T, row.names = 2, na.strings = c("","NA"))

# clean up data tables
library(reshape2)
Tracking_filter <- Tracking[c(1:20),c(2,7,9,11,13,20)]
Tracking_filter$`Batch ID` <- c(1:20)
Tracking_melt <- melt(Tracking_filter, id = c("Batch ID","% updated"))
names(Tracking_melt)[1] <- "ID"
Tracking_melt$ID <- gsub(" ","",Tracking_melt$ID)
Tracking_melt[is.na(Tracking_melt)] <- 0
Tracking_melt$`% updated` <- as.numeric(Tracking_melt$`% updated`)
Tracking_melt$variable <- gsub("% ","", Tracking_melt$variable)
Tracking_melt$ID <- factor(Tracking_melt$ID, levels = c(1:20))
# plots
ggplot(data = Tracking_melt, aes(x = ID, y = value, group = variable))+
  geom_bar(aes(fill = variable), stat = "identity", position = "stack")+
  geom_line(aes(x = ID, y = `% updated`), stat="identity", linetype="dashed")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#F1DDBF","#525E75","#78938A","#92BA92"))+
  labs(x = "Batch number", y = "Percentage", fill = "")
ggsave("tracking stats barplot Oct2022.pdf", height = 3.5, width = 7)

# questionare plots
Questionare[is.na(Questionare)] <- "not reported" 
# county number
ggplot(data = data.frame(table(Questionare$County)), aes(x = Var1, y=Freq))+
  geom_bar(stat = "identity", fill = "#E69F00")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  labs(x="County", y ="Number of responses")
ggsave("byCounty June2022.pdf",height = 3.5, width = 4)
# geometric number
library(Hmisc)
zipData <- data.frame(table(Questionare$Zipcode))
names(zipData) <- c("region","value")

zip_choropleth(zipData,
               state_zoom = c("iowa"),
               num_colors = 5,
               legend = "Number of Responses")

zipData$value = cut2(zipData$value, cuts = c(0,2,6,10,17))
choro <- ZipChoropleth$new(zipData)
choro$set_num_colors(5)
choro$ggplot_scale = scale_fill_brewer(name = "Number of Responses", 
                                       na.value="grey")

choro$set_zoom_zip(state_zoom=c("iowa"), county_zoom=NULL, msa_zoom=NULL, zip_zoom=NULL)
choro$render()
ggsave("Wells_maps June2022.pdf", height = 5,width = 5)  

# Gender
library(scales)
gender <- data.frame(table(Questionare$What.is.your.sex.))
pie <- ggplot(gender, aes(x="",y=Freq, fill=Var1)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  theme_minimal()+
  scale_fill_brewer("Blues")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))+
  geom_text(aes( label = percent(Freq/420)), 
            position=position_stack(vjust = 0.5),size=5)
pie
ggsave("gender pie chart June2022.pdf")  

# people live in the resident
peopleNumber <- data.frame(table(Questionare$How.many.people.are.currently.living.at.this.address.))
ggplot(peopleNumber, aes(x = Var1, y = Freq))+
  geom_bar(stat = "identity", fill="#3182BD")+
  labs(x="The number of people", y = "Frequency")+
  geom_text(aes(label=Freq, y = Freq + 2), position = position_dodge(0.9),
            vjust = 0)+
  theme_classic(base_size = 14)
ggsave("people number June 2022.pdf", width = 5.5, height = 3.5)

# Degree
degree <- data.frame(table(Questionare$What.is.the.highest.grade.or.level.of.school.you.have.completed.or.the.highest.degree.you.have.received.))
degree$ptg <- round(degree$Freq/420 * 100, 1)
ggplot(degree, aes(x = Var1, y = Freq))+
  geom_bar(stat = "identity", fill="#3182BD")+
  labs(x="Highest degree recieved", y = "Frequency")+
  geom_text(aes(label=Freq, y = Freq + 5), position = position_dodge(1),
            vjust = 0)+
  theme_classic(base_size = 12)+
  coord_flip()
ggsave("degree bar.pdf June2022", height = 4, width = 7)
# Races
table(Questionare$What.race.s..do.you.consider.yourself.to.be...Please.select.all.that.apply...choice.American.Indian.or.Alaska.Native.)
table(Questionare$What.race.s..do.you.consider.yourself.to.be...Please.select.all.that.apply...choice.Asian.)
table(Questionare$What.race.s..do.you.consider.yourself.to.be...Please.select.all.that.apply...choice.Black.or.African.American.)
table(Questionare$What.race.s..do.you.consider.yourself.to.be...Please.select.all.that.apply...choice.Native.Hawaiian.or.Other.Pacific.Islander.)
table(Questionare$What.race.s..do.you.consider.yourself.to.be...Please.select.all.that.apply...choice.White.)
# Asian 1, Black or African American 1, White 276, not reported 348-2-276 = 70
races <- data.frame(Var1 = c("Asian","Black or African American","White","not reported"), Freq = c(1,1,276,70))
pie <- ggplot(races, aes(x="",y=Freq, fill=Var1)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  theme_minimal()+
  scale_fill_brewer("Blues")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  geom_text(aes( label = Freq), 
            position=position_stack(vjust = 0.5),size=5)
pie
ggsave("race pie chart.pdf") 

# occupation
occupation <- data.frame(table(Questionare$Which.of.the.following.best.describes.your.occupation..or.role..within.this.industry.))
occupation$ptg <- round(occupation$Freq/sum(occupation$Freq) * 100, 1)
# income
income <- data.frame(table(Questionare$Which.of.the.following.categories.best.describes.your.total.household.income.in.the.12.months..including.wages..salaries..Social.Security.or.retirement.benefits..help.from.relatives.and.so.forth.))
income$ptg <- round(income$Freq/sum(income$Freq) * 100,1)
# length
length_resident <- data.frame(table(Questionare$About.how.long.have.you.lived.at.this.residence.))
length_resident$ptg <- length_resident$Freq/sum(length_resident$Freq) * 100
