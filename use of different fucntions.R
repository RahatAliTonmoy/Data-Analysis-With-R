library(ggplot2)
data("diamonds")
View(diamonds)
library(tidyverse)
library(dplyr)

#Q-1 Check the unique categories in the column cut,color, clarity
data <- diamonds
view(data)
names(data)

#Unique Cut
unique(data$cut)
class(data$cut)

#Unique Color
unique(data$color)
class(data$color)

#Unique Clarity
unique(data$clarity)
class(data$clarity)

#Unique categories using Pipe

data_1 <- data %>% 
  group_by(cut,color,clarity) %>% 
  distinct(cut,color,clarity) %>% 
  view()

data_2 <- data %>% 
  select(cut,color,clarity) %>% 
  distinct() %>% 
  view()

data_3 <- data %>% 
  select(cut,color,clarity) %>% 
  unique() %>% 
  view()

#Unique Using single vector

data_4 <- data %>% 
  select(cut) %>% 
  unique() %>% 
  view()

data_5 <- data %>% 
  select(color) %>% 
  unique() %>% 
  view()

data_5 <- data %>% 
  select(clarity) %>% 
  unique() %>% 
  view()

#Unique Number of values

data_6 <- data %>% 
  select(cut,color,clarity) %>% 
  unique() %>% 
  view()

#Q-2 Calculate the means for the column price,depth,x using the function
#colMeans only

V_mean = colMeans(data[,c("depth","price","x")])
V_mean

#Calculate Using Apply FUnctions
apply(data[,c("price","depth","x")],2,mean)

#Q-3 Calculate the means of price on the basis of the category cut using
#the function tapply only.
tapply(data$price, data$cut, mean)

#Using Pipe

data %>% 
  select(cut,price) %>% 
  group_by(cut) %>% 
  mutate(price_mean = mean(price)) %>% 
  view()

#Q-4 Now repeat the same task as in (c) but add one more category color
#along with cut still using the function tapply

tapply(data$price, data[,c("cut","color")],mean)

#Using Pipe

data %>% 
  select(cut,color,price) %>% 
  group_by(cut,color) %>% 
  summarise(mean(price)) %>% 
  view()
#Q-05 Use the classical or ordinary way of filtering or subsetting the data
#so that you get the data only for very good cut category of diamonds
data %>% 
  select(everything()) %>% 
  filter(cut=="Very Good") %>% 
  view()

#Another Method

filter_data = filter(data, data$cut =="Very Good")
filter_data

#Q-06 Now repeat the above task of filtering or subsetting the data but for
#three categories very good,Good, Premium
data %>% 
  select(everything()) %>% 
  filter(data$cut== c("Very Good","Good","Premium")) %>% 
  view()

#Another Way

filter_cut = filter(data, data$cut == c("Very Good", "Good","Premium"))
filter_cut

#Q-07 Now filter the data on the condition that price is greater than or
#equal to 335
data %>% 
  select(everything()) %>% 
  filter(data$price >=335) %>% 
  view()


#Another Way
filter_price = filter(data, data$price >= 335)
filter_price


#Q-08 Now filter the data on the condition that price is more than or equal
#to 330 but less than or equal to 335.
data %>% 
  select(everything()) %>% 
  filter(data$price >= 330 & data$price <=335) %>% 
  view()

#Another Way

filter_price2 <- filter(data, between(price,330,335))
filter_price2

filter_price3 <-filter(data,data$price %in% c(330:335))
filter_price3

filter_price4<- filter(data, data$price >= 330 & data$price <=335)
filter_price4


#Q-09 Calculate the means of price on the basis of the category cut using
#the pipe generating method shown in the class.

data %>% 
  group_by(cut) %>% 
  summarise(mean(price)) %>% 
  view()

#Q-10 Repeat the same task in (a) but adding one more category color
#still using the pipe generating method.

data %>% 
  group_by(cut, color) %>% 
  summarise(mean(price)) %>% 
  view()

#Q-11 ow you want to add variance, maximum, and minimum of price along with means
#for price of the diamond. Compute them along with mean of the price.
data %>% 
  select(everything()) %>% 
  summarise(var(price), max(price),min(price),mean(price)) %>% 
  view()

#Q-12 Now you want to compute the mean of price,x,y,z on the basis of cut
#category. Use the tidyverse method to compute this.

data %>% 
  group_by(cut) %>% 
  summarise(mean(price), mean(x),
            mean(y),mean(z)) %>% 
  view()

#another way
data %>% 
  group_by(cut) %>% 
  summarise(across(c(price,x,y,z),~mean(.,na.rm=T))) %>% 
  view()

#Q-13 filter data for three categoris Very Good, Good, Premium
#and showcase those data whose price are greater than 335

data %>% 
  select(everything()) %>% 
  filter(data=c("Good","Very Good",
                "Premium")& price>=335)) %>%
  view()

data %>% 
  filter(cut ==c("Good","Very Good","Premium") & price >=2800) %>% 
  view()

#Q-14 Create three different subsets of data: First contain cut and price
#only, second contains color and price, and third contains clarity and price.
#To do subsetting use the tidyverse method

sub_data_1 = data %>% 
  select(cut,price) %>% 
  view()
head(sub_data_1,5)
tail(sub_data_1,5)

#2nd subset

sub_data_2 <- data %>% 
  select(color,price) %>% 
  view()
head(sub_data_2,5)
tail(sub_data_2,5)

#3rd subset

sub_data_3 = data %>% 
  select(clarity,price) %>% 
  view()
head(sub_data_3,5)
tail(sub_data_3,5)

#Q-15 Now transform all three subsets of the data into wider format us-
#ing the tidyverse command shown in the class. Don’t forget to use both
#methods.

wide_data <- sub_data_1 %>% 
  pivot_wider(names_from = "cut",
              values_from = "price") %>% 
  view()


























