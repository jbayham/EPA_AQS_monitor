#This script downloads the epa data and populates a local sqlite db.


#########################################################
#Download data

#EPA AQS datamart site
url.temp <- "https://aqs.epa.gov/aqsweb/airdata/"

#Extract all links to zip files
links.temp <- str_c(url.temp,"download_files.html") %>%
  read_html() %>%
  html_nodes(xpath = "//a/@href[contains(., '.zip')]") %>%
  html_text() 

#Loop over files and download
links.temp %>%
  str_subset("hourly") %>%
  str_subset(str_c(seq.int(1990,2019),collapse = "|")) %>%
  str_subset(str_c(c(44201,42401,42101,42602,88101,88502,81102,"SPEC","PM10","HAPS","VOCs","NONO"),collapse = "|")) %>%
  map(.,
      function(x){
        file.temp.name <- str_c("raw_data/",x)
        
        if(!file.exists(file.temp.name)){
          message(str_c("Downloading: ",file.temp.name))
          download.file(url = str_c(url.temp,x),
                        destfile = file.temp.name,
                        quiet = T)
        } else {
          message(str_c(file.temp.name," exists"))
        }
        
        
      })




