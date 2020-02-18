#This script extracts EPA air quality hourly data from monitors nearest to
#the farms and within some distance.  

#######################################################
#######################################################
db <- dbConnect(SQLite(), dbname="build/cache/epa.sqlite")



#####################################################
#Implementing specific function
ozone.out <- pollution.metrics(db.prefix = "Ozone",var.prefix = "ozone",radius = 60000)

pm10.out <- pollution.metrics(db.prefix = "PM10_mass",var.prefix = "pm10",radius = 60000)

pm25.out <- pollution.metrics(db.prefix = "PM25_frm",var.prefix = "pm25",radius = 60000)

N02.out <- pollution.metrics(db.prefix = "N02",var.prefix = "n02",radius = 60000)

#Caching intermediate data
save(ozone.out,
     pm10.out,
     pm25.out,
     N02.out,
     file = "build/cache/pollution_monitor_data.Rdata")


#####################################################
#Function to output to stata
stata.out <- function(df.out){
  pollutant.var.nearest <- names(df.out$data) %>%
    str_subset("nearest")
  
  #Write out to stata
  df.out$data %>%
    select(name,date,time,!!pollutant.var.nearest) %>%
    mutate(time=format(time,"%H"),
           time=str_c("hour_",str_sub(time,1,2))) %>%
    spread(time,!!pollutant.var.nearest) %>%
    write_dta(.,path = str_c("build/cache/pollution_monitor_",pollutant.var.nearest,".dta"))

  pollutant.var.radius <- names(df.out$data) %>%
    str_subset("k$")
  
  df.out$data %>%
    select(name,date,time,!!pollutant.var.radius) %>%
    mutate(time=format(time,"%H"),
           time=str_c("hour_",str_sub(time,1,2))) %>%
    spread(time,!!pollutant.var.radius) %>%
    write_dta(.,path = str_c("build/cache/pollution_monitor_",pollutant.var.radius,".dta"))
  
  }


map(list(ozone.out,pm10.out,pm25.out,N02.out),
    stata.out)




