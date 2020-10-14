
#######################################################
#Populate sqlite db



# if(file.exists("database/epa.sqlite")) {
#   file.remove("database/epa.sqlite")
# }

#Creating SQLite DB in the ATUS folder
db <- dbConnect(SQLite(), dbname="database/epa.sqlite")



#map over coderef and nest other maps
#poll=5
map(1:nrow(code.ref),  #all=1:nrow(code.ref)
    function(poll){

      #Unzip files and read in data
      file.list <- dir("raw_data/",full.names = T,pattern = code.ref$code[poll]) 
      
      #Create monitor table name for db
      m.table.name <- str_c(code.ref$name[poll],"_monitor")
      
      #Create data table name for db
      d.table.name <- str_c(code.ref$name[poll],"_data")
      
      #############
      if(db_has_table(db,m.table.name)){
        #Only writing new data
        #If table exists, append new data ###may need to check if data exists or wipe table before writing
        existing.data <- tbl(db,d.table.name) %>%
          distinct(date_local) %>% 
          collect() %>%
          mutate(year=year(as_date(date_local))) %>%
          distinct(year)

        #Exclude existing years - ugly code but works
        file.list <- file.list[!(file.list %in% str_subset(file.list,pattern = str_c(existing.data$year,collapse = "|")))] 
      }
      
        
      
      
      #################################################
      #start loop over files
      map(file.list,
          function(x){
            pollution.temp <- epa.importer(file.name = x)
            
            
            ##################################################################
            #Monitor references
            
            if(!db_has_table(db,m.table.name)){
              #If table does not exist, write monitor table to DB
              dbWriteTable(conn = db,
                           name = m.table.name,
                           value = pollution.temp$monitor,
                           row.names = FALSE,
                           overwrite = T)
            } else {
              #If table exists, append new data 
              ###may need to check if data exists or wipe table before writing
              #Update: storing reported monitor for each year since they may change over time.
              #Later, I can take the last update or look at variability over time
              #The following code is deprecated
              # existing.ids <- tbl(db,m.table.name) %>%
              #   select(id) %>%
              #   distinct() %>%
              #   collect()
              # 
              # to.write <- pollution.temp$monitor %>%
              #   anti_join(.,
              #             existing.ids,
              #             by = "id")
              
              
              dbWriteTable(
                conn = db,
                name = m.table.name,
                value = pollution.temp$monitor,
                row.names = FALSE,
                append = T
              )
              
              
            }
            
            
            
            
            ##################################################################
            #Monitor data table
            
            if(!db_has_table(db,d.table.name)){
              #If table does not exist, write monitor table to DB
              dbWriteTable(conn = db,
                           name = d.table.name,
                           value = pollution.temp$data,
                           row.names = FALSE,
                           overwrite = T)
            } else {
              
              dbWriteTable(
                conn = db,
                name = d.table.name,
                value = pollution.temp$data,
                row.names = FALSE,
                append = T
              )
              
              
            }
          })
      
      
      
      
      
      
      #Creating indices for the id and the lat and lons
      db_create_index(con = db,
                      table = m.table.name,
                      columns = c("id"),
                      name = str_c("uid_",m.table.name),
                      unique = F)
      
      db_create_index(con = db,
                      table = m.table.name,
                      columns = c("latitude","longitude"),
                      name = str_c("lat_lon_",m.table.name),
                      unique = F)
      
      #######################
      
      
      #Creating indices for the id 
      db_create_index(con = db,
                      table = d.table.name,
                      columns = c("id","date_local","time_local"),
                      name = str_c("uid_",d.table.name),
                      unique = T)
      
      return(NULL)
      
    })


dbDisconnect(db)

#To clear tables
# db_drop_table(db,m.table.name)
# db_drop_table(db,d.table.name)
# dbExecute(db,"VACUUM;")