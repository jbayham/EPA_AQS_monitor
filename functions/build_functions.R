#These functions assist with the downloading and populating of the pollution
#monitor data and also include functions to extract data

##############################################################################

epa.importer <- function(file.name){
  #This function imports data from zip files downloaded from the EPA AQS data
  #mart annual files
  
  #The only argument is the file name
  #
  col.spec <- c("cccccddccDtDtdcccccicccD")
  import.temp <- read_csv(file.name,
                          #n_max = 1000,
                          col_types = col.spec) %>%
    rename_all(~str_to_lower(str_replace_all(.," ","_"))) %>% 
    mutate(id=str_c(state_code,"-",county_code,"-",site_num,"-",parameter_code,"-",poc,"-",method_code))
  
  monitor.temp <- import.temp %>%
    mutate(year=year(date_local)) %>%
    select(-sample_measurement,-date_local,-time_local,-date_gmt,-time_gmt,-qualifier) %>%
    distinct()
  
  data.temp <- import.temp %>%
    select(id,sample_measurement,date_local,time_local,date_gmt,time_gmt,qualifier)
  
  return(list(monitor=monitor.temp,
              data=data.temp))
}

#test <- epa.importer(file.list[1])





#############################################################################

pollution.metrics <- function(db.prefix,var.prefix,radius=20000){
  #Associating monitor subset with farms: nearest, radius km mean (inv distance weight) 

  #Writing a function to calculate pollution metrics.
  #Note this is not a general function and is built assuming many objects in the
  #workspace so it should be used here only.
  # db.prefix <- "Ozone"
  # var.prefix <- "ozone"
  # radius=60000
  
  #######################################################
  #Creating name for nearest variable
  new.var.nearest <- str_c(var.prefix,"_nearest")
  #Creating name for within 20k variable
  new.var.radius <- str_c(var.prefix,"_",radius/1000,"k")
  
  #Calling monitor table from DB
  g.pollution <- tbl(db,str_c(db.prefix,"_monitor")) %>%
    #select(id,latitude,longitude) %>%
    collect() %>%
    distinct(id,.keep_all = T) %>%
    st_as_sf(coords=c("longitude","latitude")) %>%
    mutate(fips=str_replace_all(str_sub(id,1,6),"-","")) %>%
    st_set_crs(4326) %>%
    st_transform(project.epsg)
  
  #############################
  #Finding nearest monitor
  nearest <- st_nearest_feature(farm.loc,g.pollution) %>%
    g.pollution[.,] %>%
    select(id,fips) %>%
    st_set_geometry(NULL) %>%
    bind_cols(farm.loc %>% st_set_geometry(NULL)) 
  
  #Querying data table for nearest monitor data and joining with monitor
  data.nearest <- tbl(db,str_c(db.prefix,"_data")) %>%
    select(id,
           date=date_local,
           time=time_local,
           value=sample_measurement) %>%
    filter(id %in% !!nearest$id) %>%
    collect() %>%
    mutate(date=as_date(date),  #confirmed that these are the correct conversions
           time=hms::as.hms(time)) %>%
    rename(!!new.var.nearest := value) %>% 
    inner_join(.,
               nearest,
               by="id")
  
  ##############################
  #Finding monitors within range 
  
  distances <- split(farm.loc,farm.loc$name) %>%
    map_dfr(.,function(x){
      dist.df <- st_join(x,
                         g.pollution,
                         join = st_is_within_distance,
                         dist=radius) 
      
      #Calculating the distance between all monitors and farms
      distances <- dist.df %>%
        mutate(dist=as.numeric(st_distance(x,g.pollution %>% filter(id %in% dist.df$id))),
               type="within_radius") %>%
        st_set_geometry(NULL)
      
    })
  
  
  #Querying the data table and calculating the inverse weighted pollution measure
  dist.df <- tbl(db,str_c(db.prefix,"_data")) %>%
    select(id,
           date=date_local,
           time=time_local,
           value=sample_measurement) %>%
    filter(id %in% !!distances$id) %>%
    collect() %>%
    mutate(date=as_date(date),  #confirmed that these are the correct conversions
           time=hms::as.hms(time)) %>%
    inner_join(.,
               distances %>% mutate(inv.dist=1/dist) %>% select(id,name,inv.dist),
               by="id") %>%
    group_by(name,date,time) %>%
    summarise(!!new.var.radius :=sum(inv.dist*value,na.rm = T)/sum(inv.dist,na.rm = T)) %>%
    ungroup()
  
  nearest.dist.temp <- inner_join(g.pollution,
                                  nearest %>% select(id,name),
                                  by="id") 
  
  distances <- nearest.dist.temp %>%
    mutate(dist=apply(st_distance(farm.loc,nearest.dist.temp),2,min),
           type="nearest") %>%
    st_set_geometry(NULL) %>%
    bind_rows(distances)
  
  
  #Combining into a single df
  df.out <- full_join(data.nearest,
                      dist.df,
                      by=c("name","date","time")) 
  
  return(list(data=df.out,distances=distances))
}