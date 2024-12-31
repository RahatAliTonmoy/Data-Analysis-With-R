library(tidyverse)
library(ggplot2)
library(pacman)
library(forcats)

data <- read.csv(file.choose())
view(data)

data <- data %>% 
  select(-c(longitude,latitude,city,website,phoneNumber,
            state.1,region,yearFounded,stateCode))
names(data)

dim(data)
glimpse(data)



# check if there is any NA values
dim(data[!complete.cases(data),])
data[!complete.cases(data),]
table(data[!complete.cases(data),])

#check Carnegie Classification and campus factor
table(unique(data$carnegieClassification))

dim(data_updated[!complete.cases(data_updated),])
unique(table(data$campusSetting))


# Factorization using old method 
data$campusSetting = factor(data$campusSetting)
data$campusSetting = factor(data$campusSetting,
                            c("Urban","Suburban","Rural"),
                            labels= c(1,2,3))
data$campusSetting = factor(data$campusSetting,
                            c(1,2,3),
                            labels=c("Urban","Suburban","Rural"))

data$campusSettingNo = factor(data$campusSetting,
                              c("Urban","Suburban","Rural"),
                              labels=c(1,2,3))
view(data)

# Change the names 
unique(data$collegeType)
data$collegeType = factor(data$collegeType)
data$collegeType = data$collegeType %>% 
  fct_recode(Private ="Private not-for-profit")
unique(table(data$collegeType))

# check if there is any NA values 

dim(data[!complete.cases(data),])

# Factorization of string data using tidyverse method 
data = data %>% 
  select(-campusSettingNo) %>% 
  mutate(campus_factor = as.factor(campusSetting),
         campus_category = as.numeric(campus_factor))
view(data)


# Home task: collapse 11 categories of Carnegie classification 
# into 4 classification: Doctoral, Master, Bachelor, Other 

unique(data$carnegieClassification)
data$carnegieClassification = data$carnegieClassification %>% 
  fct_collapse("Doctoral" =c("Doctoral Universities: Very High Research Activity",
                             "Doctoral Universities: High Research Activity",
                             "Doctoral/Professional Universities"),
               "Masters" =c("Masters Colleges & Universities: Larger Programs",
                            "Masters Colleges & Universities: Small Programs",
                            "Masters Colleges & Universities: Medium Programs"),
               "Bachelor"=c("Baccalaureate Colleges: Arts & Sciences Focus",
                            "Baccalaureate Colleges: Diverse Fields"),
               "Others"=c("Special Focus Four-Year: Arts, Music & Design Schools",
                          "Special Focus Four-Year: Engineering Schools",
                          "Special Focus Four-Year: Other Technology-Related Schools"))

#barplot Classic Method

barplot_1 <- ggplot(data,aes(x=collegeType))+
  geom_bar(aes(fill=collegeType),position ="dodge",
           width = .8)+
  labs(x="College Type",y="Frequency",title = "Bar Plot By College Type")+
  scale_y_continuous(limits =c(0,280))+
  scale_fill_manual(values = c('dark blue','sky blue'))+
  theme(axis.text.x = element_text(angle=25, size = 10.5,
                                   color='maroon'),
        axis.text.y = element_text(angle=35, size=10.5, color='maroon'))+
  theme(text = element_text(color='blue',size =10))+
  theme(legend.position = 'right')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(.25,"cm"))+
  theme(legend.text = element_text(size =8, color='blue'))+
  theme(plot.title = element_text(hjust= 0.5, size=10,
                                  color='brown4'))

barplot_1

#Bar Plot Tidyverse Method
barPlot_2 <- data %>% 
  ggplot(aes(x=campusSetting))+
  geom_bar(aes(fill = campusSetting),position = "dodge",width=.7)+
  labs(x="College Type",y="Frequency",title = "Bar Plot By Campus-settig")+
  scale_y_continuous(limits=c(0,300))+
  scale_fill_manual(values= c('maroon','red','dark blue'))+
  theme(axis.text.x = element_text(angle=35,
                                   size=10,color='darkred'),
        axis.text.y = element_text(angle=25,size=10,
                                   color='darkred'))+
  theme(text=element_text(color='darkgreen',size=10))+
  theme(legend.position = 'right')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(.2,"cm"))+
  theme(plot.title = element_text(hjust=0.5,size=12,
                                  color='deepskyblue'))

barPlot_2

ggsave(barPlot_2, width = 6, height=4,dpi=800,
       filename("barplot.png"))

ggsave("BarPlot.png",barPlot_2, width=6, height=4)

getwd()

view(data)

#Another Kind of Bar Plot
max(data$rank)
data$rank_category = cut(data$rank, breaks=c(0,100,200,300,400,500),
                         labels=c('High','Medium-High','Medium','Medium-Low','Low'))
count(data,data$rank_category)

#Barplot

barplot_3 = data %>% 
  ggplot(aes(x=rank_category))+
  geom_bar(aes(fill=rank_category),stat='count',
          position=position_stack(reverse = TRUE),width = .8)+
  labs(x='Rank',y='Frequency',title='Grouped Bar Plot by Region')+
  scale_y_continuous(limits=c(0,100))+
  scale_fill_manual(values = c('darkblue','green',
                               'maroon','darkred','skyblue'))+
  facet_wrap(.~campus_factor, nrow=2, ncol=2)+
  theme(axis.text.x = element_text(angle=20,size=6,color='darkblue'),
        axis.text.y=element_text(angle=360, size = 6, color = 'black'))+
  theme(text=element_text(size=8,color='blue'))+
  theme(plot.margin = unit(c(.5,.5,.5,.5),"cm"))+
  theme(legend.position = 'none')+
  theme(legend.title = element_blank())+
  theme(legend.key.size= unit(.25,"cm"))+
  theme(legend.text = element_text(size=7,color='black'))+
  theme(plot.title = element_text(hjust=.5,size=8, color='black'))+
  theme(strip.text.x = element_text(size=7, color = 'blue'))
        
        
barplot_3




