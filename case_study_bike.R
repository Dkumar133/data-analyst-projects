install.packages("tidyverse")
install.packages("janitor")
install.packages("scales")
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(scales)
library(janitor)

df1 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202109-divvy-tripdata.csv")
df2 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202110-divvy-tripdata.csv")
df3 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202111-divvy-tripdata.csv")
df4 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202112-divvy-tripdata.csv")
df5 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202201-divvy-tripdata.csv")
df6 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202202-divvy-tripdata.csv")
df7 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202203-divvy-tripdata.csv")
df8 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202204-divvy-tripdata.csv")
df9 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202205-divvy-tripdata.csv")
df10 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202206-divvy-tripdata.csv")
df11 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202207-divvy-tripdata.csv")
df12 <-read.csv("C:/Users/pavilion/OneDrive/Desktop/case study 1/bike data set/202208-divvy-tripdata.csv")


###### step 1: Data cleaning // prepare #######

#### combine all data in one data frame ##########




bind_dataframe <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)
                      
colnames(bind_dataframe)
View(bind_dataframe)
dim(bind_dataframe)



#### a) remove duplicate rows and columns ####


bind_dataframe <- remove_empty(bind_dataframe, which=c("cols","rows"), quiet = TRUE)
 


colnames(bind_dataframe)
dim(bind_dataframe)

############# b) remove unnecessary columns - which we do not need for our analysis #############

bind_df1<- bind_dataframe %>% select(-c(end_lat,end_lng,start_lng,start_lat))



#  dimensions(rows and columns) of Data frame 

dim(bind_df1)


#  glimpse of our Data frame 
glimpse(bind_df1)

 ####### c)  omitting NA values in the entire dataframe ####
bind_df2 <- na.omit(bind_df1)

dim(bind_df2)


# d) returns unique values of the variable passed ###########
bind_df1 %>% 
  count(start_station_name)



# e)  missing values need to check ###


count(filter(bind_df2, start_station_name==''), start_station_name, member_casual,sort=TRUE)


##### year and time data should be in standard format using as.POSIXct ########

bind_df2$started_at <- as.POSIXct(bind_df2$started_at,"%Y-%m-%d %H:%M:%S")
bind_df2$ended_at <- as.POSIXct(bind_df2$ended_at,"%Y-%m-%d %H:%M:%S")

glimpse(bind_df2)

####  f)   eliminate  duplicates in  bike's ride_id ###



bind_df3 <- bind_df2[!duplicated(bind_df2$ride_id), ]
print(nrow(bind_df3))


######## step 2 : Data Process / organizing data  ######

# organizing data to understand data better 

# a) adding riding_time in data frame to get differance  of start and end riding time###

clean_df <- bind_df3
clean_df <- clean_df %>% 
  mutate(riding_time = as.numeric(ended_at-started_at)/60)

head(clean_df)

# b) find Weekday of end time
clean_df <- clean_df %>% 
  mutate(weekday=strftime(clean_df$ended_at, "%A"))

head(clean_df)

# c) start_hour
clean_df <- clean_df %>% 
  mutate(start_hour=strftime(clean_df$ended_at, format = "%H",tz = "UTC"))

unique(clean_df$start_hour)


# d) find starting month and year 
clean_df <- clean_df %>% 
  mutate(year_and_month=paste(strftime(clean_df$started_at, "%Y"), "-",
                          strftime(clean_df$started_at, "%m"),
                          paste("(", strftime(clean_df$started_at, "%B"),")",sep="")))



head(clean_df)

# Summary of resultant clean data frame
glimpse(clean_df)
View(clean_df)


# step :3 - Data Analysis ##

# a) check percentage number of members and casual riders

final_df <- clean_df
final_df %>% 
  group_by(member_casual) %>% 
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(final_df))*100)

ggplot(final_df, aes(member_casual, fill=member_casual))+
  geom_bar()+
  labs(title="Chart-1 Member vs Casual distribution")+
  scale_y_continuous(labels=comma)

# b)check  monthly distribution of  member and casual riders

final_df%>% 
  group_by(year_month) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(final_df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100) %>% 
  arrange(year_month)

ggplot(final_df, aes(year_month, fill=member_casual))+
  geom_bar()+
  coord_flip()+
  scale_y_continuous(labels=comma)

# c)   start_hour vs member_casual
final_df %>% 
  group_by(start_hour) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(final_df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100) %>% 
  arrange(start_hour)

ggplot(final_df, aes(start_hour, fill=member_casual))+
  geom_bar()+
  facet_wrap(~weekday)+
  scale_y_continuous(labels=comma)
View(final_df)

# d) Splitting the start_hour into morning, afternoon, and evening
final_df <- mutate(final_df, hour_of_the_day=ifelse(final_df$start_hour<12, "Morning",
                                        ifelse(final_df$start_hour>=12 & final_df$start_hour<19, "Afternoon", "Evening")))

ggplot(final_df, aes(start_hour, fill=member_casual))+
  geom_bar()+
  facet_wrap(~hour_of_the_day, scales = "free")+
  scale_y_continuous(labels=comma)+
  coord_flip()

# e) weekday vs member_casual
final_df%>% 
  group_by(weekday) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(final_df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100)

ggplot(final_df, aes(weekday, fill=member_casual))+
  geom_bar()+
  scale_y_continuous(labels=comma)

# f) rideable_type vs member_casual
final_df%>% 
  group_by(rideable_type) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(final_df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100)

ggplot(final_df, aes(rideable_type, fill=member_casual))+
  geom_bar()+
  scale_y_continuous(labels=comma)+
  facet_wrap(~weekday)
