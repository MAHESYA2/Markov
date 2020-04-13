path = "C:/Users/MAHESYA2/OneDrive - Novartis Pharma AG/Commercial Use Case Repo"
setwd(path)

seg=read.csv("dataset.csv")
##'markovchain' is not available (for R version 3.5.3)
##'ChannelAttribution' is not available (for R version 3.5.3)
install.packages("ChannelAttribution")
install.packages("markovchain")
install.packages("plotly")

library("ChannelAttribution")
library("ggplot2")
library("reshape")
library("dplyr")
library("plyr")
library("reshape2")
library("markovchain")
library("plotly")
library("sqldf")

channel = read.csv("dataset.csv", header = T)
channel = sqldf('select a.*, b.seg
                from channel  a
                inner join
                cluster_data_out  b
                on a.Physician_ID=b.Physician_ID  ')


channel = channel %>% select(-Physician_ID)

for(row in 1:nrow(channel))
{
  if(50 %in% channel[row,]){channel$convert[row] = 1}
}


column = colnames(channel)
channel$path = do.call(paste, c(channel[column], sep = " > "))
head(channel$path)

for(row in 1:nrow(channel))
{
  channel$path[row] = strsplit(channel$path[row], " > 50")[[1]][1]
}


for(row in 1:nrow(channel))
{
  channel$path[row] = strsplit(channel$path[row], " > NA")[[1]][1]
}

channel = channel[!(channel$COL1==50),]

channel_fin = channel[,c(66,65)]

channel_fin = ddply(channel_fin,~path,summarise, conversion= sum(convert))

Data = channel_fin
H <- heuristic_models(Data, 'path', 'conversion', var_value='conversion')
H

M <- markov_model(Data, 'path', 'conversion', var_value='conversion', order = 4)
M

R <- merge(H, M, by='channel_name')
R1 <- R[, (colnames(R) %in% c('channel_name', 'first_touch_conversions', 'last_touch_conversions', 
                              'linear_touch_conversions', 'total_conversion'))]

R1 <- melt(R1, id='channel_name')

R1 = R1[!(R1$channel_name=='NA'),]

R1$channel = ifelse(R1$channel_name==25,"In App Activity",
            ifelse(R1$channel_name==32,"Banner Impressions",
                   ifelse(R1$channel_name==35,"Samples",
                          ifelse(R1$channel_name==42,"Calls",
                                 ifelse(R1$channel_name==37,"Emails",
                                        ifelse(R1$channel_name==39,"Direct Mail",
                                               ifelse(R1$channel_name==28,"Digital Clicks","Banner Email Clicks")))))))


ggplot(R1, aes(channel, value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('TOTAL CONVERSIONS') +
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title=element_text(size = 20)) +
  ylab("")
