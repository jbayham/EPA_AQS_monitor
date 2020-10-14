


library(data.table)
library(scales)

#Plotting daiurnal patterns of ozone by month
pollution.data[[1]] %>% 
  filter(zip %in% sample(unique(.$zip),5)) %>%
  mutate(month=lubridate::month(date,label=T)) %>%
  ggplot(aes(x=time,y=Ozone_nearest,color=zip)) +
  geom_smooth() +
  facet_wrap(~month) +
  scale_x_time(name = "Hour of Day",labels = time_format("%H")) +
  ylab("Measurement") +
  theme_minimal()

ggsave(filename = "projects/chess_diurnal_month_ozone.png",
       height = 7, width = 10, units = "in")


#melting to long then plotting
diurnal.patterns <- pollution.all.data %>%
  select(zip,date,time,ends_with("nearest")) %>%
  data.table() %>%
  melt(.,id.vars = c("zip","date","time"),
       variable.name = "pollutant",value.name = "measurement") %>%
  .[!is.na(measurement)] %>%
  as_tibble() %>%
  ggplot(aes(x=time,y=measurement)) +
  geom_smooth() +
  scale_x_time(name = "Hour of Day",labels = time_format("%H")) +
  ylab("Measurement") +
  facet_wrap(~pollutant,scales = "free") +
  theme_minimal()

ggsave(filename = "projects/chess_diurnal.png",plot = diurnal.patterns,
       height = 7, width = 10, units = "in")



library(GGally)
my_bin <- function(data, mapping,...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha=.03,stroke=0) +
    geom_smooth()
}

pollution.all.data %>%
  select(ends_with("nearest")) %>%
  select(-contains("speciated")) %>%
  drop_na() %>%
  sample_frac(.001) %>%
  ggpairs(lower = list(continuous = my_bin)) +
  theme_minimal()

ggsave(filename = "projects/chess_pollutant_corr.png",
       height = 7, width = 10, units = "in")



#Distribution of distances in meters - we should probably throw some of these out
pollution.data[[1]] %>%
  distinct(zip,Ozone_distance) %>% 
  arrange(zip) %>%
  ggplot(aes(Ozone_distance)) +
  geom_histogram() +
  xlab("") +
  ylab("") +
  ggtitle("Histogram of distance between zip \ncentroid and monitor") +
  theme_minimal()
ggsave(filename = "projects/distances.png")
