#These functions help extract data from the EPA sqlite db




##############################################################################
##############################################################################
##############################################################################

pollution.aggregation <- function(db.prefix,new.var.name,start.date,end.date){
  #Associating monitor subset with polygons 
  
  #Writing a function to calculate pollution metrics.
  #Note this is not a general function and is built assuming many objects in the
  #workspace so it should be used here only.
  # db.prefix <- "Ozone"
  # new.var.name <- "ozone"
  # start.date <- "2015-01-01"  #something convertible by lubridate::as_date()
  # end.date <- lubridate::today() or a date in format of start.date
  ####################################################################
  
  #Defining julian dates for sql query
  start.date <- as_date(start.date) %>% julian.Date()
  end.date <- as_date(end.date) %>% julian.Date()
  
  load("projects/zip_vector.Rdata")
  
  #Load zipcodes (cached from tigris)
  us_zip <- read_sf("projects/us_zipcodes/us_zipcodes.shp") %>%
    st_set_crs(4269) %>%
    filter(geoid10 %in% zip_vector)  #filter only monitors in zipcodes of interest
  
  
  #Calling monitor table from DB and intersecting monitor points and zipcode polygons
  monitor.index <- tbl(db,str_c(db.prefix,"_monitor")) %>%
    select(id,year,latitude,longitude) %>%
    arrange(id,desc(year)) %>%
    collect() %>%
    distinct(id,.keep_all = T) %>%
    st_as_sf(coords=c("longitude","latitude")) %>%
    st_set_crs(4269) %>%
    st_intersection(.,us_zip %>% select(zip=geoid10)) %>%   #need to associate zipcodes with monitors - should associate geos when writing db
    st_set_geometry(NULL)
  
  
  
  #Querying data table for all data from monitor subset
  data.subset <- tbl(db,str_c(db.prefix,"_data")) %>%
    select(id,
           date=date_local,
           time=time_local,
           value=sample_measurement) %>%
    filter(id %in% !!monitor.index$id,between(date,start.date,end.date)) %>%
    collect() %>%
    mutate(date=as_date(date),  #confirmed that these are the correct conversions
           time=hms::as.hms(time)) %>%
    inner_join(.,
               monitor.index %>% select(-year),
               by="id")
  

  #Calculating means within zips
  data.out <- data.table(data.subset) %>%
    .[, list(value = mean(value, na.rm = T)),
      by = list(zip, date, time)] %>%
    as_tibble() %>%
    rename(!!new.var.name := value) 
    
  
  return(data.out)
}

#unit test
#test <- pollution.aggregation("CO","co","2015-01-01",today())


##################################################################

pollution.nearest <- function(db.prefix,var.prefix,start.date,end.date){
  #Associating monitor subset with farms: nearest, radius km mean (inv distance weight) 
  
  #Writing a function to calculate pollution metrics.
  #Note this is not a general function and is built assuming many objects in the
  #workspace so it should be used here only.
  # db.prefix <- "Ozone"
  # var.prefix <- "ozone"
  # start.date <- "2015-01-01"  #something convertible by lubridate::as_date()
  # end.date <- "2019-12-31"  lubridate::today() or a date in format of start.date
  
  require(progress)
  
  message(str_c("Merging ",var.prefix," with nearest monitor by day"))
  #######################################################

  #Defining julian dates for sql query
  start.date <- as_date(start.date) %>% julian.Date()
  end.date <- as_date(end.date) %>% julian.Date()
  
  load("projects/zip_vector.Rdata")
  
  #Load zipcodes (cached from tigris)
  us_zips <- read_sf("projects/us_zipcodes/us_zipcodes.shp") %>%
    st_set_crs(4269) 
  
  selected_zips <- us_zips %>%
    filter(geoid10 %in% zip_vector) %>%  #filter only monitors in zipcodes of interest
    st_centroid()
  
  
  ############
  #Doing the following operations by day because not all monitors report each day 
  date.list <- seq(as_date(start.date),as_date(end.date),1)
  pb <- progress_bar$new(
    format = "  [:bar] :percent eta: :eta",
    total = length(date.list), clear = FALSE, width= 60)
  system.time({
  data.out <- 
    map_dfr(date.list,
            function(x) {
              pb$tick()
              #Calling monitor table from DB and intersecting monitor points and zipcode polygons
              yr.filter <- year(x)
              doy.filter <- julian.Date(x)
              
              monitor.index <- inner_join(tbl(db,str_c(db.prefix,"_monitor")) %>% filter(year==yr.filter),
                                          tbl(db,str_c(db.prefix,"_data")) %>% filter(date_local==doy.filter),
                                          by="id") %>% 
                select(id,latitude,longitude) %>%
                distinct() %>%
                collect() %>%
                st_as_sf(coords=c("longitude","latitude")) %>%
                st_set_crs(4269) 
              
              #############################
              #Finding nearest monitor by year
              nearest <- suppressMessages(st_nearest_feature(selected_zips,monitor.index)) %>%
                monitor.index[.,] %>%
                add_column(zip=selected_zips$geoid10)
              
              #Calculating distance between zip centroid and nearest monitor
              nearest <- nearest %>%
                mutate(!!str_c(var.prefix,"_distance") := as.numeric(st_distance(nearest,selected_zips,by_element = T))) %>%   #distance in meters
                st_set_geometry(NULL)
              
              #Querying data table for nearest monitor data and joining with monitor
              data.nearest <- tbl(db,str_c(db.prefix,"_data")) %>%
                select(id,
                       date=date_local,
                       time=time_local,
                       value=sample_measurement) %>%
                filter(id %in% !!nearest$id,
                       date==doy.filter) %>% 
                collect() %>%
                mutate(date=as_date(date),  #confirmed that these are the correct conversions
                       time=hms::as.hms(time))  %>% 
                inner_join(.,
                           nearest,
                           by="id") %>%
                rename(!!str_c(var.prefix,"_id") := id) %>%
                rename(!!str_c(var.prefix,"_nearest") := value)
              
              return(data.nearest)
              
            })
  
  })
    
  
  
  return(data.out)
}

#test <- pollution.nearest("Ozone","ozone","2015-01-01",today())
