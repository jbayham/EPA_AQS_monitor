#This script pulls the data to merge with the chess project


db <- dbConnect(SQLite(), dbname="database/epa.sqlite")


#####################################################
#Implementing specific function
# name.list <- tibble(db = code.ref$name[1:3],
#                     var = c("ozone","s02","co"))

pollution.data <- map(1:nrow(code.ref[1:9,]),
                      ~pollution.nearest(db.prefix = code.ref$name[.],var.prefix = code.ref$name[.],start.date = "2015-01-01",end.date = "2019-12-31"))



#Caching intermediate data
save(pollution.data,
     file = "projects/chess_pollution_data.Rdata")

pollution.all.data <- reduce(pollution.data,full_join,by=c("zip","date","time"),.dir = "forward")

haven::write_dta(pollution.all.data %>% select(-ends_with("id")),
                 path = "projects/chess_pollution_zipcode.dta")

dbDisconnect(db)


#####################################

#Check similarity across data construction methods
test <- inner_join(ozone.agg,
                   ozone.nearest,
                   by=c("zip","date","time"))

test %>%
  sample_frac(.05) %>%
  ggplot(aes(x=ozone,y=ozone_nearest,color=distance)) +
  geom_point(alpha=.3)