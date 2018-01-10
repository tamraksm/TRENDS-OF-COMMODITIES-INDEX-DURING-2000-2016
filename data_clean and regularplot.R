#load the packages
library(rvest)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(fpp2)


#########################################
############ G O L D ####################
#########################################

# read the data from html
investing_g <- read_html("C:/Users/sanjay/Desktop/project_solo/gold_weekly.html")

# make data readable into R by selecting only the necessay items
gold_1<- investing_g %>%
  html_nodes("table") %>%
  .[3:4] %>%
  html_table(fill = TRUE)

gold_weekly<- gold_1[[1]]

# change the Date to the Date,format
gold_weekly$Date<- as.Date(gold_weekly$Date,format='%B %d, %Y')

# order the data by date 
gold_weekly<- gold_weekly[order(gold_weekly$Date),]

# remove commas and set price to numeric
gold_weekly$Price<-as.numeric(gsub(",", "", gold_weekly$Price))

# remove "K" and set Vol. to numeric
gold_weekly$Vol.<-as.numeric(gsub("K","", gold_weekly$Vol.))*1000

# take only date, price and vol. for further analysis and name it gold_final
gold_final<- gold_weekly %>%
  select(Date,Price,Vol.)

#########################################
############ CRUDE ######################
#########################################

# read the data from html
investing_c <- read_html("C:/Users/sanjay/Desktop/project_solo/crude_weekly.html")

# make data readable into R by selecting only the necessay items
crude_1<- investing_c %>%
  html_nodes("table") %>%
  .[3:4] %>%
  html_table(fill = TRUE)

crude_weekly<- crude_1[[1]]

# change the Date to the Date,format
crude_weekly$Date<- as.Date(crude_weekly$Date,format='%B %d, %Y')

# order the data by date 
crude_weekly<- crude_weekly[order(crude_weekly$Date),]

# remove "K" and set Vol. to numeric
crude_weekly$Vol.<-as.numeric(gsub("K","", crude_weekly$Vol.))*1000

# take only date, price and vol. for further analysis and name it crude_final
crude_final<- crude_weekly %>%
  select(Date,Price,Vol.)

#########################################
############ DOLLAR #####################
#########################################

# read the data from html
investing_d <- read_html("C:/Users/sanjay/Desktop/project_solo/dollar_weekly.html")

# make data readable into R by selecting only the necessay items
dollar_1<- investing_d %>%
  html_nodes("table") %>%
  .[3:4] %>%
  html_table(fill = TRUE)

dollar_weekly<- dollar_1[[1]]

# change the Date to the Date,format
dollar_weekly$Date<- as.Date(dollar_weekly$Date,format='%B %d, %Y')

# order the data by date 
dollar_weekly<- dollar_weekly[order(dollar_weekly$Date),]

# remove "K" and set Vol. to numeric
dollar_weekly$Vol.<-as.numeric(gsub("K","", dollar_weekly$Vol.))*1000

# take only date, price and vol. for further analysis and name it dollar_final
dollar_final<- dollar_weekly %>%
  select(Date,Price,Vol.)

#########################################
############ SNP 500 ####################
#########################################

# read the data from html
investing_s <- read_html("C:/Users/sanjay/Desktop/project_solo/snp500_weekly.html")

# make data readable into R by selecting only the necessay items
snp_1<- investing_s %>%
  html_nodes("table") %>%
  .[3:4] %>%
  html_table(fill = TRUE)

snp_weekly<- snp_1[[1]]

# change the Date to the Date,format
snp_weekly$Date<- as.Date(snp_weekly$Date,format='%B %d, %Y')

# order the data by date 
snp_weekly<- snp_weekly[order(snp_weekly$Date),]

# remove commas and set price to numeric
snp_weekly$Price<-as.numeric(gsub(",", "", snp_weekly$Price))

# remove "K" and set Vol. to numeric
snp_weekly$Vol.<-as.numeric(gsub("K","", snp_weekly$Vol.))*1000

# take only date, price and vol. for further analysis and name it snp_final
snp_final<- snp_weekly %>%
  select(Date,Price,Vol.)


#########################***************************###################################
#########################***************************###################################

# make a simple plot with date and price


#GOLD
pl1<- ggplot()+
  geom_line(aes(x=Date,y=Price),color="blue",data=gold_final)+
  labs(title = "Weekly Gold Price from 2000 - 2016 ",
       x = "Years", y = "Price of Gold ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(limit = c(0, 2000))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
             linetype=4, colour="brown")

#CRUDE
pl2<- ggplot()+
  geom_line(aes(x=Date,y=Price),color="red",data=crude_final)+
  labs(title = "Weekly Crude Oil Price from 2000 - 2016 ",
       x = "Years", y = "Price of Crude Oil ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
             linetype=4, colour="brown")+
  scale_y_continuous(limit = c(0, 150))+
  theme(plot.title = element_text(hjust = 0.5))

#DOLLAR
pl3<-ggplot()+
  geom_line(aes(x=Date,y=Price),color="green",data=dollar_final)+
  labs(title = "Weekly US Dollar Index Price from 2000 - 2016 ",
       x = "Years", y = "Price of US Dollar Index ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
             linetype=4, colour="brown")+
  scale_y_continuous(limit = c(50, 150))+
  theme(plot.title = element_text(hjust = 0.5))

#SNP 500
pl4<- ggplot()+
  geom_line(aes(x=Date,y=Price),color="orange",data=snp_final)+
  labs(title = "Weekly SNP 500 Index from 2000 - 2016 ",
       x = "Years", y = "Price of SNP 500 Index ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
             linetype=4, colour="brown")+
  scale_y_continuous(limit = c(500, 2500))+
  theme(plot.title = element_text(hjust = 0.5))


#########################************GOLD*****###################################
#########################***********CRUDE***********#############################


library(gtable)
library(grid)


p1<- ggplot()+
  geom_line(aes(x=Date,y=Price),color="blue",data=gold_final)+
  labs(title = "Weekly Price of Gold and Crude from 2000 - 2016 ",
       x = "Years", y = "Price") +
  geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
             linetype=4, colour="brown")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(limit = c(0, 2000))+
  theme(plot.title = element_text(hjust = 0.5))

#CRUDE
f1<- ggplot()+
  geom_line(aes(x=Date,y=Price),color="red",data=crude_final)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(limit = c(0, 150))+
  theme(plot.title = element_text(hjust = 0.5))%+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(pl1))
g2 <- ggplot_gtable(ggplot_build(f1))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
two_gc<- grid.draw(g)



