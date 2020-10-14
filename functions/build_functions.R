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

