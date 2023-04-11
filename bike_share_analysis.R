library(tidyverse)
library(geosphere)
library(stats)
library(mapview)
library(RColorBrewer)
##load each month's data
month01 <- read_csv("202204-divvy-tripdata.csv", 
                   col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                          "casual"))))

month02 <- read_csv("202205-divvy-tripdata.csv", 
                   col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                          "casual"))))

month03 <- read_csv("202206-divvy-tripdata.csv", 
                   col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                          "casual"))))

month04 <- read_csv("202207-divvy-tripdata.csv", 
                   col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                          "casual"))))

month05 <- read_csv("202208-divvy-tripdata.csv", 
                   col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                          "casual"))))

month06 <- read_csv("202209-divvy-publictripdata.csv", 
                    col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                           "casual"))))


month07 <- read_csv("202210-divvy-tripdata.csv", 
                   col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                          "casual"))))

month08 <- read_csv("202211-divvy-tripdata.csv", 
                   col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                          "casual"))))

month09 <- read_csv("202212-divvy-tripdata.csv", 
                              col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                                     "casual"))))

month10 <- read_csv("202301-divvy-tripdata.csv", 
                    col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                           "casual"))))

month11 <- read_csv("202302-divvy-tripdata.csv", 
                   col_types = cols(member_casual = col_factor(levels = c("member", 
                                                                          "casual"))))
test<-month12 <- read_csv("202303-divvy-tripdata.csv", 
                        col_types = cols(member_casual = col_factor(levels = c("casual", 
                                                                               "member"))))
#checked manually with colnames(x)==colnames(y) that they all had the same
#column names

all_trips = bind_rows(month01, month02, month03, month04, month05, month06, month07,
                      month08, month09, month10, month11, month12)

rm(month01, month02, month03, month04, month05, month06, month07, month08,
   month09, month10, month11, month12)

head(test)
View(test)
glimpse(test)
str(test)


#---------------------basic cleaning checks---------------------------
#check to see if each station ID corresponds to a station name
all_trips %>% group_by(start_station_name) %>% summarise(n_distinct(start_station_id)) %>% arrange(-`n_distinct(start_station_id)`)
#check in the opposite direction, some id's have 2 different names
all_trips %>% group_by(start_station_id) %>% summarise(n_distinct(start_station_name)) %>% arrange(-`n_distinct(start_station_name)`)

#are there just a few long/lat's listed for each street name?
all_trips %>% group_by(start_station_name) %>% summarise(n_distinct(start_lat)) %>% arrange(-`n_distinct(start_lat)`)
#nope, there are a whole lot of them.

#see how imprecisely located a station ID is. Resuts in meters
all_trips %>% group_by(start_station_id) %>% 
  summarise(max_geo_error = distm(c(max(start_lng),max(start_lat)), c(min(start_lng), min(start_lat)), fun=distHaversine))%>%
  arrange(-max_geo_error)
#the worst ones are miles off! Remove NA and the one with TEST in the name
all_trips<-all_trips %>% filter(!is.na(start_station_id)&start_station_id!='Hubbard Bike-checking (LBS-WH-TEST)')

#see how imprecisely located stations names are
all_trips %>% group_by(start_station_name) %>% 
  summarise(max_geo_error = distm(c(max(start_lng),max(start_lat)), c(min(start_lng), min(start_lat)), fun=distHaversine))%>%
  arrange(-max_geo_error)
#they are also very imprecise! If using distance need to filter out all of these.


#check to see if each long and lat specifies a specific station. Spoiler, the resolution isn't good enough
all_trips %>% group_by(start_lat, start_lng) %>% arrange(start_lat, start_lng)

#----------calculate some intermediate quantities-------------------------

#calculate distance of the rides, in m (but don't trust the precision very far!)
#This takes awhile to run, and needs a for loop or it runs out of memory
all_trips <- mutate(all_trips,ride_dist = 1)

pb <- txtProgressBar(min = 0, max = length(all_trips$ride_id))
for(i in 1:nrow(subtest)) {       # for-loop over rows
  all_trips[i, 'ride_dist'] <- distm(x = all_trips[i,c("start_lng", "start_lat")],
                                y = all_trips[i,c("end_lng", "end_lat")],
                                fun =  distHaversine)
  setTxtProgressBar(pb, i)
}
close(pb)

#calculate the amount of time a ride lasts
all_trips <- mutate(all_trips, ride_duration = ended_at-started_at)

min(all_trips$ride_duration)
#filter out negative and zero duration trips
all_trips<-all_trips %>% filter(ride_duration>0) %>% arrange(ride_duration)

#longest rental of 28 days, reasonable-ish
max(all_trips$ride_duration)/60/60/24

#lots of 1 and 2 second rides, filter at least 1 min
all_trips <- filter(all_trips, ride_duration>60)

#extract the day of the week, month, and hour for easier aggregation
all_trips <- mutate(all_trips, week_day = wday(started_at, label=TRUE))
all_trips <-mutate(all_trips, month = month(started_at, label=TRUE))
all_trips <-mutate(all_trips, hour = hour(started_at))

#add a category for round_trip (if trips end at the same station they began) or one_way
#A significant fraction of the data is missing an end_station_id, listed NA. Could fake something
#with start and end lat/long, but I don't trust the precision.
all_trips <- mutate(all_trips, round_trip = ifelse(start_station_id==end_station_id, 'round_trip', 'one_way'))

#yeah, there are a lot of (6k/258k) with a non-zero trip distance. average is 
#pretty low, at 16 m, with a mak of ~1 km. Could proxy some data with a distance
#cutoff if the missing data was a big problem.
all_trips %>% filter(round_trip=='round_trip') %>% 
  filter(ride_dist != 0) %>% 
  summarise(n_distinct(ride_id), mean(ride_dist))
all_trips %>% filter(round_trip=='round_trip') %>% 
  filter(ride_dist!= 0) %>% 
  arrange(-ride_dist) %>% View()


max(all_trips$ride_dist, na.rm=TRUE)
#22 km is reasonable
min(all_trips$ride_dist, na.rm=TRUE)
#nothing under zero is good. Net distance of zero is expected.





#-------------analysis and plots---------------------------
#some formatting definitions
member_labeller<- as_labeller(c(`casual`='Non-Members', `member`='Members'))
no_y_ticks <-theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
pct_y_ticks <- scale_y_continuous(labels = function(x) paste0(x, "%"))

#Aggregate by discrete variables so the plot functions run in finite time.
aggregate_df <- all_trips  %>% 
  group_by(member_casual, rideable_type, round_trip, month, week_day, hour) %>%
  summarize(rides = n_distinct(ride_id))

#calculate the total number of rides by members or non-members
tot_member_casual<- aggregate_df %>% group_by(member_casual) %>% summarise(tot_rides = sum(rides))
tot_casual<- as.numeric(tot_member_casual[2,'tot_rides'])
tot_member<- as.numeric(tot_member_casual[1,'tot_rides'])
#create a normalized variable ride_pct, such that the total for members and
#casual each add up to a hundred. Only works if you include all rides, so
#e.g. only rides on member rides on monday won't sum to 100.
aggregate_df$ride_pct <- ifelse(aggregate_df$member_casual=='member',
       aggregate_df$rides/tot_member*100,
       aggregate_df$rides/tot_casual*100)
#check to see that the ride percentatges sum to 100%
aggregate_df %>% group_by(member_casual) %>% summarise(sum(ride_pct))

#By Day of the Week
aggregate_df  %>% group_by(member_casual, week_day) %>%
  summarise(ride_pct=sum(ride_pct)) %>% 
  ggplot(aes(x=week_day, y=ride_pct, fill=member_casual))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = 'Rides by Day of the Week', x = "Day of the Week", y = "Percentage of Rides")+
  theme_bw()+
  theme(legend.title=element_blank())+
  pct_y_ticks+
  scale_fill_discrete(labels = c("Members", "Non-Members"))

#By month
aggregate_df  %>% group_by(member_casual, month) %>%
  summarise(ride_pct=sum(ride_pct)) %>% 
  ggplot(aes(x=month, y=ride_pct, fill=member_casual))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = 'Rides by Month', x = "Month", y = "Percentage of Rides")+
  theme_bw()+
  theme(legend.title=element_blank())+
  pct_y_ticks+
  scale_fill_discrete(labels = c("Members", "Non-Members"))

#By time of Day (bar)
aggregate_df  %>% 
  ggplot()+geom_col(mapping=aes(x=hour, y=ride_pct))+
  facet_wrap(~member_casual, labeller=member_labeller)+
  labs(title = 'Rides by Time of Day', x = "Time", y = "Percentage of Rides")

#By time of Day (line)
aggregate_df %>% group_by(hour, member_casual) %>%
  summarise(sum_pct = sum(ride_pct)) %>% 
  ggplot()+geom_line(mapping=aes(x=hour, y=sum_pct, color=member_casual), size=1.5)+
  labs(title = 'Rides by Time of Day', x = "Hour of the Day", y = "Percentage of Rides")+
  scale_x_continuous(breaks = 0:23)+
  theme_bw()+
  theme(legend.title=element_blank())+
  scale_color_discrete(labels = c("Members", "Non-Members"))+
  pct_y_ticks

#by bike type
aggregate_df %>% group_by(rideable_type, member_casual) %>%
  summarise(sum_pct = sum(ride_pct)) %>% 
  filter(member_casual=='member') %>% 
  ggplot(aes(x="", y=sum_pct, alpha= rideable_type)) +
  geom_bar(stat="identity", width=1, fill= "#F8766D") +
  coord_polar("y", start=0)+
  scale_alpha_discrete(labels=c('Classic', 'Electric'))+
  theme_void()+
  theme(legend.title=element_blank())+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  labs(title="Member Bike Choice")+
  theme(plot.title = element_text(hjust = 0.5))

aggregate_df %>% group_by(rideable_type, member_casual) %>%
  summarise(sum_pct = sum(ride_pct)) %>% 
  filter(member_casual=='casual') %>% 
  ggplot(aes(x="", y=sum_pct, alpha= rideable_type)) +
  geom_bar(stat="identity", width=1, fill= "#00BFC4") +
  coord_polar("y", start=0)+
  scale_alpha_discrete(labels=c('Classic', 'Docked', 'Electric'))+
  theme_void()+
  theme(legend.title=element_blank())+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  labs(title="Non-Member Bike Choice")+
  theme(plot.title = element_text(hjust = 0.5))




#Member vs casual in whether trips end at the same place they started
#Round trip pct for members
aggregate_df %>% 
  drop_na(round_trip) %>%
  filter(member_casual=='member') %>% 
  group_by(round_trip) %>% summarize(tot_rides = sum(rides)) %>% 
  ggplot(aes(x="", y=tot_rides, alpha=round_trip)) +
  geom_bar(stat="identity", width=1, fill= "#F8766D") +
  coord_polar("y", start=0)+
  scale_alpha_discrete(labels=c('One Way', 'Round Trip'))+
  theme_void()+
  theme(legend.title=element_blank())+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  labs(title="Member Trips Returing to Start") +
  theme(plot.title = element_text(hjust = 0.5))

#Round trip percentage for non-members
aggregate_df %>% 
  drop_na(round_trip) %>%
  filter(member_casual=='casual') %>% 
  group_by(round_trip) %>% summarize(tot_rides = sum(rides)) %>% 
  ggplot(aes(x="", y=tot_rides, alpha=round_trip)) +
  geom_bar(stat="identity", width=1, fill= "#00BFC4") +
  coord_polar("y", start=0)+
  scale_alpha_discrete(labels=c('One Way', 'Round Trip'))+
  theme_void()+
  theme(legend.title=element_blank())+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  labs(title="Non-Member Trips Returing to Start") +
  theme(plot.title = element_text(hjust = 0.5))



#in number form, member vs casual
aggregate_df %>% 
  drop_na(round_trip) %>% 
  group_by(member_casual, round_trip) %>% 
  summarise(sub_tot = sum(rides)) %>% 
  summarise(round_pct = 100*sub_tot[round_trip=="round_trip"]/(sub_tot[round_trip=="round_trip"]+sub_tot[round_trip=="one_way"]),
            one_pct = 100*sub_tot[round_trip=="one_way"]/(sub_tot[round_trip=="round_trip"]+sub_tot[round_trip=="one_way"]))

#Looking just at the heart of rush hour, 8am on weekdays
aggregate_df %>% 
  drop_na(round_trip) %>% 
  filter(hour==8 & !grepl("^S", week_day)) %>% 
  group_by(member_casual, round_trip) %>% 
  summarise(sub_tot = sum(rides)) %>% 
  summarise(round_pct = 100*sub_tot[round_trip=="round_trip"]/(sub_tot[round_trip=="round_trip"]+sub_tot[round_trip=="one_way"]),
            one_pct = 100*sub_tot[round_trip=="one_way"]/(sub_tot[round_trip=="round_trip"]+sub_tot[round_trip=="one_way"]))


#looking at the least commuter-y time
aggregate_df %>% 
  drop_na(round_trip) %>% 
  filter(hour==17 & week_day=='Sun') %>% 
  group_by(member_casual, round_trip) %>% 
  summarise(sub_tot = sum(rides))  %>%
  summarise(round_pct = 100*sub_tot[round_trip=="round_trip"]/(sub_tot[round_trip=="round_trip"]+sub_tot[round_trip=="one_way"]),
            one_pct = 100*sub_tot[round_trip=="one_way"]/(sub_tot[round_trip=="round_trip"]+sub_tot[round_trip=="one_way"]))

#--Duration--
all_trips %>% filter(ride_duration>60) %>% group_by(member_casual) %>%
  summarise(avg_duration = median(ride_duration)/60) %>% 
  ggplot() +
  geom_col(mapping = aes(x=member_casual, y=avg_duration, fill=member_casual), show.legend = FALSE) +
  labs(title = 'Average Ride Duration', x = '', y = "Duration (min.)")+
  theme_bw()+
  scale_x_discrete(labels=c("Members", "Non-Members"))+
  theme(axis.text.x=element_text(size=11))
#Check the distribution, but not super interesting.
all_trips %>% filter(ride_duration>60&ride_duration<60*60*1) %>% mutate(num_duration = as.double(ride_duration)/60) %>% 
ggplot() +
  geom_histogram(aes(num_duration, fill=member_casual), show.legend=FALSE)+
  facet_wrap(~member_casual, scales = "free_y", labeller=member_labeller)+
  theme_bw()+
  labs(title = 'Ride Duration Distribution', x = 'Duration (min.)', y = "Rides")+
  no_y_ticks

#--------------Distance-----------------
#group by station pairs so we only need to calculate each distance once.
station_pairs<-all_trips %>% filter(round_trip=='one_way', !is.na(end_station_id)) %>%
  group_by(member_casual, start_station_id, end_station_id) %>% 
  summarise(start_lat = mean(start_lat), start_lng = mean(start_lng),
            end_lat = mean(end_lat), end_lng = mean(end_lng))

station_pairs$ride_dist <- 0 
tot_i<-length(station_pairs$member_casual)
for(i in 1:nrow(station_pairs)) {       # for-loop over rows
  station_pairs[i, 'ride_dist'] <- distm(x = station_pairs[i,c("start_lng", "start_lat")],
                                     y = station_pairs[i,c("end_lng", "end_lat")],
                                     fun =  distHaversine)
  if(i%%1000==0){print(100*i/tot_i)}
}

station_pairs  %>% group_by(member_casual) %>%
  summarise(avg_distance = mean(ride_dist)/1000) %>% 
  ggplot() +
  geom_col(mapping = aes(x=member_casual, y=avg_distance, fill=member_casual), show.legend = FALSE) +
  labs(title = 'Average Ride Distance', x = '', y = "Distance (km)")+
  theme_bw()+
  scale_x_discrete(labels=c("Members","Non-Members"))+
  theme(axis.text.x=element_text(size=11))
#Check the distribution, but not super interesting.
all_trips %>% filter(round_trip=='one_way'&ride_duration>60&ride_dist<10000) %>% mutate(num_dist = as.double(ride_dist)/1000) %>% 
  ggplot() +
  geom_histogram(aes(num_dist, fill=member_casual), show.legend=FALSE)+
  facet_wrap(~member_casual, scales = "free_y", labeller=member_labeller)+
  theme_bw()+
  labs(title = 'Ride Duration Distribution', x = 'Distance (km)', y = "Rides")+
  no_y_ticks

#--let's look at some weird speed proxy--
all_trips %>% filter(round_trip=='one_way'&ride_duration>60&ride_dist<10000&ride_duration<60*60*1) %>%
  ggplot()+
  geom_point(mapping=aes(x=ride_duration, y=ride_dist, color=member_casual), alpha=.1, size=.02)+
  facet_wrap(~member_casual)

#The numbers are pretty similar, and the distance measurement is too noisy to
#have any confidence
all_trips %>% filter(round_trip=='one_way'&ride_duration>60&ride_dist<10000&ride_duration<60*60*1) %>%
  mutate(speed = ride_dist/as.numeric(ride_duration)) %>% 
  group_by(member_casual) %>% 
  summarise(mean_speed = mean(speed), var_speed = sd(speed),
            slowest_1pct = quantile(speed, .01), 
            fasall_trips_1pct= quantile(speed, .99))

#----------------Maps-------------------------
library(terra)
library(mapview)


map_df<-all_trips %>% drop_na(start_station_id, start_lng, start_lat) %>% group_by(start_station_id, member_casual) %>%
  summarise(long = mean(start_lng), lat = mean(start_lat), rides = n_distinct(ride_id),
            log_rides = log10(n_distinct(ride_id)))

tot_map<- map_df %>% group_by(member_casual) %>% summarise(tot_rides = sum(rides))
tot_map_member<- as.numeric(tot_map[1,'tot_rides'])
tot_map_casual<- as.numeric(tot_map[2,'tot_rides'])

map_df$ride_pct <- ifelse(map_df$member_casual=='member',
                                map_df$rides/tot_map_member*100,
                                map_df$rides/tot_map_casual*100)
map_df<- mutate(map_df, log_ride_pct = log10(ride_pct))
#check to make sure percentages sum to 100
map_df %>% group_by(member_casual) %>%  summarise(sum(ride_pct))

map_df %>% 
  filter(member_casual=="member") %>% 
  mapview(xcol = "long", ycol = "lat", zcol="log_ride_pct", crs = 4269, grid = FALSE, cex=2)

map_df %>% 
  filter(member_casual=="casual") %>% 
  mapview(xcol = "long", ycol = "lat", zcol="log_ride_pct", crs = 4269, grid = FALSE, cex=2)


geo_diff<-pivot_wider(map_df, names_from=member_casual, values_from=ride_pct) %>% group_by(start_station_id) %>% 
  summarise(casual_pct=sum(casual, na.rm=TRUE), member_pct=sum(member, na.rm=TRUE), diff_pct = member_pct-casual_pct,
            norm_diff=plogis(diff_pct)-0.5, long=mean(long), lat=mean(lat))
#check to see that the sum of the differences is zero (up to rounding)
geo_diff %>% summarise(sum(diff_pct))
#the norm won't sum to zero, depending on the relative skews.
geo_diff %>% summarise(sum(norm_diff))

#data's there, but the scale is not very useful
geo_diff  %>% 
  mapview(xcol = "long", ycol = "lat", zcol="diff_pct", crs = 4269, grid = FALSE, cex=2)


#export to use with tableau
geo_diff %>% write_csv('geo_diff.csv')





