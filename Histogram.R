library(tidyverse)
library(ggplot2)
library(dplyr)
library(forcats)
library(forcats)
data = read.csv(file.choose())
view(data)

data$rank_cat = cut(data$rank,
                    breaks = c(0,100,200,300,500),
                    labels=c('High','Medium-High','Medium','Low'))
view(data$rank_cat)

#------------HistroGram-1--------------

histogram_plot_1 = data %>% 
  ggplot(aes(x=studentFacultyRatio))+
  geom_histogram(aes(y=..density..),col='black',fill='red')+
  labs(x="Student-Teacher-Ratio",y="Denstiy",title='Histogram of Student-Teacher Ratio')+
  theme(axis.text.x = element_text(angle=25, size=8,color="maroon"),
        axis.text.y = element_text(angle=25,size=8, color='maroon'))+
  theme(text=element_text(size=15,color='darkgreen'))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  xlim(c(0,40))+
  theme(legend.position = 'right')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(.25,"cm"))+
  theme(legend.text = element_text(size=7,color='white'))+
  theme(plot.title = element_text(hjust=.5,size=10,
                                  color='darkblue'))+
  theme(strip.text.x = element_text(size=8,color='green'))

histogram_plot_1


#-----------------HistroGram-1.1------------------

histogram_plot_11 = data %>% 
  ggplot(aes(x=studentFacultyRatio))+
  geom_histogram(aes(y=..density..),col='black',fill='red')+
  labs(x="Student-Teacher-Ratio",y="Denstiy",title='Histogram of Student-Teacher Ratio')+
  theme(axis.text.x = element_text(angle=25, size=8,color="maroon"),
        axis.text.y = element_text(angle=25,size=8, color='maroon'))+
  theme(text=element_text(size=15,color='darkgreen'))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  xlim(c(0,40))+
  theme(legend.position = 'right')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(.25,"cm"))+
  theme(legend.text = element_text(size=7,color='white'))+
  theme(plot.title = element_text(hjust=.5,size=10,
                                  color='darkblue'))+
  theme(strip.text.x = element_text(size=8,color='green'))+
  annotate("text",x=27,y=0.1,label=paste("Median=",median(data$studentFacultyRatio, na.rm=TRUE)),
           col="black",size=3)+
  annotate("text",x=27,y=0.08,label=paste("Mean",round(mean(data$studentFacultyRatio,na.rm=TRUE),2)),
           col='blue',size=3)

histogram_plot_11

#-----------HistroGram Plot 2----------------

histogramPlot_2 <- data %>% 
  ggplot(aes(x=data$studentFacultyRatio))+
  geom_histogram(aes(y=..density..),col='blue',fill='blue',binwidth=1)+
  #facet_wrap(.~rank_cat,nrow=2, ncol=2)+
  labs(x="Student Faculty Ratio",y="Density",
       title='Histogram of Student-Teacher-Ratio')+
  theme(axis.text.x = element_text(angle=25, size=8,color="maroon"),
        axis.text.y = element_text(angle=25,size=8, color='maroon'))+
  theme(text=element_text(size=15,color='darkgreen'))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  xlim(c(0,40))+
  theme(legend.position = 'right')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(.25,"cm"))+
  theme(legend.text = element_text(size=7,color='white'))+
  theme(plot.title = element_text(hjust=.5,size=10,
                                  color='darkblue'))+
  theme(strip.text.x= element_text(size=8,color='green'))+
  annotate("text",x=27,y=.08,label=paste("Mean=",round(mean(data$studentFacultyRatio,na.rm = TRUE),2)),
                                         col="maroon",size=3)+
  annotate("text",x=27,y=0.075,label=paste("Median=",round(median(data$studentFacultyRatio,na.rm=TRUE),2)),
           col='maroon',size=3)
  

histogramPlot_2

#----------------HistoGram-3--------------------

mean <- data %>% 
  group_by(rank_cat) %>% 
  summarise(mean_val=mean(studentFacultyRatio))
print(mean)

median <- data %>% 
  group_by(rank_cat) %>% 
  summarise(median_val =median(studentFacultyRatio))
print(median)

histogramPlot_4 <- data %>% 
  ggplot(aes(x=studentFacultyRatio))+
  geom_histogram(aes(y=..density..),col='red',fill='red',binwidth=1)+
  facet_wrap(.~rank_cat, nrow=2,ncol=2)+
  labs(x = "Student-Teacher Ratio", y = "Density", title = 'Histogram of Student-Teacher Ratio by University Ranking')+
  scale_x_continuous(limits=c(0,40),expand=c(0,0))+
  scale_y_continuous(limits=c(0,.15), expand=c(0,0))+
  theme(axis.text.x=element_text(angle = 0, size = 7, color = 'black'), axis.text.y=element_text(angle=360, size = 7, color = 'white'))+
  theme(text=element_text(size = 9, color = 'blue'))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(legend.position = 'none')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'blue'))+
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = 'blue'))+
  theme(strip.text.x = element_text(size=8, color = 'blue'))+
  geom_vline(data=median, aes(xintercept = median_val),
             color='darkgreen')+
  geom_vline(data=mean,aes(xintercept = mean_val),
             color='darkblue')+
  annotate("text",x = 27,y = 0.10,label = paste("Mean=",round(mean(data$studentFacultyRatio, na.rm=TRUE),2)),
           col = "maroon",size = 3)+
  annotate("text",x = 27, y = 0.12,label = paste("Median = ",round(median(data$studentFacultyRatio,na.rm=TRUE),2)),
           col = 'darkred',size = 3)+
  theme(panel.spacing = unit(0.4, "cm"))


histogramPlot_4
