# R project code

library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(tibble)
df<- read.csv(file = 'Apps_Top_500_new.csv')
summary(df)

# separating size column to size value column and size units column
library(tidyr)
# Creation of Size_value column
df$Size_value = as.numeric(gsub(".*?([0-9]+).*", "\\1", df$Size)) 

#Seperating Reviews column to review value and review units column
pattern1 <- "([A-Za-z]+)([0-9]*)"
df$Reviews_Value<- as.numeric(sub(pattern = pattern1, replacement = "\\2", x = df$Reviews))

pattern2 <- "([0-9]+([A-Za-z]*))"
df$Reviews_units<- sub(pattern = pattern2, replacement = "\\2", x = df$Reviews)

#Seperatingdownloads  column to downloads  value and download  units column
pattern3 <- "([A-Za-z+]+)([0-9]*)"
df$Downloads_Value<- as.numeric(sub(pattern = pattern3, replacement = "\\2",x = df$Downloads))

pattern4 <- "([0-9]+([A-Za-z]*))"
df$Downloads_units<- sub(pattern = pattern4, replacement = "\\2", x = df$Downloads)

# Creation of age column from Rated for column
df$Age = as.numeric(gsub(".*?([0-9]+).*", "\\1", df$Rated.for)) 
summary(df)

# calculation of no_reviews and no_downloads
df$No_reviews<-ifelse(df$Reviews_units == "L", df$Reviews_Value * 100000,
                      ifelse(df$Reviews_units == "Cr",df$Reviews_Value * 10000000,
                             df$Reviews_Value * 1000))
df$No_downloads<-ifelse(df$Downloads_units == "L+", df$Downloads_Value * 100000,
                        ifelse(df$Downloads_units == "Cr+",df$Downloads_Value * 10000000,
                               df$Downloads_Value * 1000))
summary(df)


# count plot of category

df %>% 
  count(Category) %>% 
  mutate(prop = n) %>% 
  ggplot(aes(x = Category, y = prop)) +
  geom_col(fill = "#FF7F24") +
  geom_text(aes(label = prop, vjust = -1)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10))

hist(df$Size_value,xlab = "Size_value",col = "green",border = "Blue",main="Memory Size of App")
hist(df$No_reviews,xlab = "No_reviews",col = "Maroon",border = "Gold",main="No of reviews")
df_reviews<-as.data.frame(df[,16])


myfun<- function(x)
{
  if (x >=1 && x<=10000) {z<-"10Th"}
  else if (x >=10001 && x<=100000) {z<-"1Lac"}
  else if (x >=100001 && x<=10000000) {z<-"10Lac"}
  else if (x >=1000001 && x<=100000000) {z<-"1Cr"}
  else if (x >=10000001 && x<=1000000000) {z<-"10Cr"}
  else if (x>= 100000001 && x<=1000000000) {z<-"100Cr"}
  else if (x >=1000000001 && x<=10000000000) {z<-"1000Cr"}
  else  {z<-"NA"}
  
}

df_reviews$typeNew<- apply(df_reviews,1,myfun)

df_reviews %>% 
  count(typeNew) %>% 
  mutate(prop = n) %>% 
  ggplot(aes(x = typeNew, y = prop)) +
  geom_col(fill = "#FF7F24") +
  geom_text(aes(label = prop, vjust = -1)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10))


hist(df$No_downloads,xlab = "No of downloads",col = "Blue",border = "Green",main="No of downloads")
df_downloads<-as.data.frame(df[,17])
df_downloads$typeNew<- apply(df_downloads,1,myfun)


df_downloads %>% 
  count(typeNew) %>% 
  mutate(prop = n) %>% 
  ggplot(aes(x = typeNew, y = prop)) +
  geom_col(fill = "#228B22") +
  geom_text(aes(label = prop, vjust = -1)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10))



# count plot of Rated for

df %>% 
  count(Rated.for) %>% 
  mutate(prop = n) %>% 
  ggplot(aes(x = prop, y = Rated.for)) +
  geom_col(fill = "#FF99FF") +
  geom_text(aes(label = prop, hjust = -0.1)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust=1, vjust = 1),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) 

boxplot(df$Star.Rating)

df_cat<-as.data.frame(df %>% group_by(Category) %>%
                        summarize(No_reviews_th = sum(No_reviews)/1000))


df_order<-df_cat[order(df_cat$No_reviews_th, decreasing = TRUE), ]  
df_order

bp<- ggplot(df_cat, aes(x="", y=No_reviews_th, fill=Category))+
  geom_bar(width = 1, stat = "identity",color="Dark Blue")
bp

df_cat<-as.data.frame(df %>% group_by(Category) %>%
                        summarize(No_downloads_th = sum(No_downloads)/1000))


df_order<-df_cat[order(df_cat$No_downloads_th, decreasing = TRUE), ]  
df_order

piepercent<- round(100 * df_cat$No_downloads_th / sum(df_cat$No_downloads_th), 1)

pieless3<-ifelse(piepercent<3 ,'',as.character(piepercent))


ggplot(df_cat, aes(x="", y=No_downloads_th, fill=Category)) +geom_bar(width = 0.5, stat = "identity") +
  coord_polar("y", start=0) +theme_void()  +
  geom_text(aes(label = paste(pieless3)), position = position_stack(vjust = 0.5)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#FF0000"))+
  ggtitle("Sum of no of Downloads ")


plot(x = df$Star.Rating, y = df$No_downloads,
     xlab = "Star Rating",
     ylab = "No of Downloads",   
     main = "Star ratings  vs No of downloads"
)

plot(x = df$Star.Rating, y = df$No_reviews,
     xlab = "Star Rating",
     ylab = "No of Reviews",
     main = "Star ratings  vs No of Reviews"
)

plot(x = df$Star.Rating, y = df$Size_value,
     xlab = "Star Rating",
     ylab = "Size(MB) ",
     main = "Star ratings  vs Memory Size (MB)"
)

plot(x = df$Star.Rating, y = df$Age,
     xlab = "Star Rating",
     ylab = "Rated For ",
     main = "Star ratings  vsRated For "
)


# extracting subset of dataframe for Category =  Tools

df_tools<- select(filter(df, Category  == "Tools"), c(Size_value, No_reviews, No_downloads,Age,Star.Rating))
plot(x = df_tools$Size_value, y = df_tools$No_reviews,
     xlab = "App Size",
     ylab = "No of Reviews",        
     main = "appsize vs no of reviews"
)

plot(x = df_tools$Size_value, y = df_tools$No_downloads,
     xlab = "App Size",
     ylab = "No of Downloads",     
     main = "appsize vs no of downloads"
)

plot(x = df_tools$No_reviews, y = df_tools$No_downloads,
     xlab = "No of Reviews",
     ylab = "No of Downloads",     
     main = "no of reviews vs no of downloads"
)

plot(x = df_tools$Age, y = df_tools$No_downloads,
     xlab = "Age",
     ylab = "No of Downloads",
     main = "Age vs no of downloads"
)

# Correlation matrix

res<-cor(df_tools)
round(res, 2)

# plot a heat map
library(ggplot2)
library(reshape2)
data1<-melt(res)

ggplot(data1,aes(x = Var1, y = Var2,fill = value))+
  geom_tile() + scale_fill_distiller(palette = "Spectral")



# create a facet wrap
ggplot(df, aes(No_reviews, No_downloads)) +
  geom_point() +
  facet_wrap(vars(Rated.for))

