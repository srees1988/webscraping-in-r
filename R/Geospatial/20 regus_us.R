debug_message_l2("Regus US Website Data")
debug_message_end("20 <<")

options(warn=-1)
#extracting regus location api-------------------------

location_data_final <- data.frame()

url <- "https://www.regus.com/api/search/centres"


resp <- GET(url)


http_type(resp)

jsonRespText<-content(resp,as="text") 

jsonRespParsed<-content(resp,as="parsed") 

regus_locations <-fromJSON(jsonRespText) 

names(regus_locations)

regus_locations <- regus_locations %>%  select (CenterNumber
                                                ,CenterName
                                                ,Latitude
                                                ,Longitude
                                                ,FormattedAddress
                                                ,CenterCity
)


# Reverse Geocoding -------------------------------------------------------

reverse_geocode<- revgeo(longitude=regus_locations$Longitude, latitude=regus_locations$Latitude, output='frame')

regus_locations_full <- cbind(regus_locations, reverse_geocode) %>%  select (
CenterNumber
,CenterName
,Latitude
,Longitude
,FormattedAddress
,CenterCity
,state
,zip
,country
) %>%  rename(
city = CenterCity
,postcode = zip
,address = FormattedAddress
)


regus_locations_full  <- regus_locations %>%  inner_join (reverse_geocode, by = c("CenterNumber"))


regus_us_locations <- regus_locations_full %>%  filter(country == "United States of America")





# Private Office------------------------------------------------------------------



coworking <- data.frame()

for (i in 1: as.integer(regus_us_locations %>% summarise(n())))
  
{
  
  #Status Code 500 Path
  
  url <- paste("https://www.regus.com/ecommercevo/price?productType=platinumPlus&daysLength=30&monthLength=1&startDate=", Sys.Date(), 
               "&countryCode=US&centreNumber=", regus_us_locations$CenterNumber[i], sep ="")
  
  
  resp <- GET(url)
  
  
  if(resp$status_code == 500)
  {
    
    
    url <- paste("https://www.regus.com/ecommercevo/price?productType=platinumPlus&monthLength=1&startDate=", Sys.Date(), 
                 "&countryCode=US&centreNumber=", "&_=",as.integer(as.numeric(Sys.time())),924,sep ="")
    
    
    resp <- GET(url)
    
    jsonRespText<-content(resp,as="text") 
    
    vo_list <-fromJSON(jsonRespText)
    
    
    if(!is.null(vo_list$Price)){
      au_vo_temp <-  as_data_frame( vo_list$Price) %>% rename(private_office_price =value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    if(is.null(vo_list$Price)){
      
      au_vo_temp <-  as_data_frame( "$0" ) %>% rename(private_office_price =value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    coworking <- rbind(coworking, au_vo_temp)
    
  }
  
  else if (resp$status_code == 200)
    
  {
    
    
    jsonRespText<-content(resp,as="text") 
    
    vo_list <-fromJSON(jsonRespText)
    
    
    if(!is.null(vo_list$Price)){
      au_vo_temp <-  as_data_frame( vo_list$Price) %>% rename(private_office_price=value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    if(is.null(vo_list$Price)){
      
      au_vo_temp <-  as_data_frame( "$0" ) %>% rename(private_office_price =value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    coworking <- rbind(coworking, au_vo_temp)
    
  }
}




regus_us_locations <- regus_us_locations %>%  left_join(coworking, by = c("CenterNumber" = "center_number"))




# Dedicated Desk-----------------------------------------------------------------


coworking <- data.frame()

for (i in 1: as.integer(regus_us_locations %>% summarise(n())))
  
{
  
  #Status Code 500 Path
  
  url <- paste("https://www.regus.com/ecommercevo/price?productType=platinum&daysLength=30&monthLength=1&startDate=", Sys.Date(), 
               "&countryCode=US&centreNumber=", regus_us_locations$CenterNumber[i], sep ="")
  
  
  
  resp <- GET(url)
  
  
  if(resp$status_code == 500)
  {
    
    
    url <- paste("https://www.regus.com/ecommercevo/price?productType=platinum&monthLength=1&startDate=", Sys.Date(), 
                 "&countryCode=US&centreNumber=", "&_=",as.integer(as.numeric(Sys.time())),924,sep ="")
    
    
    resp <- GET(url)
    
    jsonRespText<-content(resp,as="text") 
    
    vo_list <-fromJSON(jsonRespText)
    
    
    if(!is.null(vo_list$Price)){
      au_vo_temp <-  as_data_frame( vo_list$Price) %>% rename(dedicated_desk_price=value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    if(is.null(vo_list$Price)){
      
      au_vo_temp <-  as_data_frame( "$0" ) %>% rename(dedicated_desk_price =value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    coworking <- rbind(coworking, au_vo_temp)
    
  }
  
  else if (resp$status_code == 200)
    
  {
    
    
    jsonRespText<-content(resp,as="text") 
    
    vo_list <-fromJSON(jsonRespText)
    
    
    if(!is.null(vo_list$Price)){
      au_vo_temp <-  as_data_frame( vo_list$Price) %>% rename(dedicated_desk_price=value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    if(is.null(vo_list$Price)){
      
      au_vo_temp <-  as_data_frame( "$0" ) %>% rename(dedicated_desk_price =value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    coworking <- rbind(coworking, au_vo_temp)
    
  }
}




regus_us_locations <- regus_us_locations %>%  left_join(coworking, by = c("CenterNumber" = "center_number"))


# Hot Desk-----------------------------------------------------------------


coworking <- data.frame()

for (i in 1: as.integer(regus_us_locations %>% summarise(n())))
  
{
  
  #Status Code 500 Path
  
  url <- paste("https://www.regus.com/ecommercevo/price?productType=gold&daysLength=5&monthLength=1&startDate=", Sys.Date(), 
               "&countryCode=US&centreNumber=", regus_us_locations$CenterNumber[i], sep ="")
  
  
  
  resp <- GET(url)
  
  
  if(resp$status_code == 500)
  {
    
    
    url <- paste("https://www.regus.com/ecommercevo/price?productType=gold&monthLength=1&startDate=", Sys.Date(), 
                 "&countryCode=US&centreNumber=", "&_=",as.integer(as.numeric(Sys.time())),924,sep ="")
    
    
    resp <- GET(url)
    
    jsonRespText<-content(resp,as="text") 
    
    vo_list <-fromJSON(jsonRespText)
    
    
    if(!is.null(vo_list$Price)){
      au_vo_temp <-  as_data_frame( vo_list$Price) %>% rename(hot_desk_price=value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    if(is.null(vo_list$Price)){
      
      au_vo_temp <-  as_data_frame( "$0" ) %>% rename(hot_desk_price =value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    coworking <- rbind(coworking, au_vo_temp)
    
  }
  
  else if (resp$status_code == 200)
    
  {
    
    
    jsonRespText<-content(resp,as="text") 
    
    vo_list <-fromJSON(jsonRespText)
    
    
    if(!is.null(vo_list$Price)){
      au_vo_temp <-  as_data_frame( vo_list$Price) %>% rename(hot_desk_price=value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    if(is.null(vo_list$Price)){
      
      au_vo_temp <-  as_data_frame( "$0" ) %>% rename(hot_desk_price=value)
      au_vo_temp$center_number <- as.integer (regus_us_locations$CenterNumber[i])
    }
    coworking <- rbind(coworking, au_vo_temp)
    
  }
}




regus_us_locations <- regus_us_locations %>%  left_join(coworking, by = c("CenterNumber" = "center_number"))








# Final Dataset (wip)-----------------------------------------------------------


regus_us_locations <- regus_us_locations %>% select (
  
  "CenterNumber"
  ,"CenterName.x" 
  ,"Latitude.x" 
  ,"Longitude.x"
  ,"address"
  ,"city"  
  ,"state"
  ,"postcode"
  ,"country" 
  ,"vo_price"             
  ,"address_price"       
  ,"private_office_price" 
  ,"dedicated_desk_price"
  ,"hot_desk_price"     
  ,"meeting_room_price" 
  ,"postcode"
  
) %>% rename(
  
  building_id = CenterNumber
  ,building_name = CenterName.x
  ,lat = Latitude.x
  ,lng = Longitude.x
  ,site_name = CenterName.x
  ,street_name = address
  ,city = city
  ,state = state
  ,country = country
  ,postcode = postcode
  ,building_slug = CenterNumber
  ,building_path = CenterName.x
  ,hot_desk_price_latest = hot_desk_price
  ,dedicated_desk_price_latest = dedicated_desk_price
  ,private_office_price_latest = private_office_price
  ,meeting_rm_hr_price_latest = meeting_room_price
  ,address_price_latest = address_price
  ,vo_price_latest = vo_price
  
) %>%  mutate(
  date_time_Stamp = Sys.Date()
  ,company = "Regus"
  ,currency_name = "US Dollar"
  ,currency_short = "USD"
  
)

regus_us_locations  <-  regus_us_locations %>% mutate (
  hot_desk_price_w_symb = currency(hot_desk_price_latest, "\U0024"),
  private_office_price_w_symb = currency(private_office_price_latest, "\U0024"),
  dedicated_desk_price_w_symb = currency(dedicated_desk_price_latest, "\U0024"),  
  meeting_rm_hr_price_latest_w_symb  = currency(meeting_rm_hr_price_latest, "\U0024"),
  address_price_latest_w_symb = currency(address_price_latest, "\U0024"),
  vo_price_latest_w_symb = currency(vo_price_latest, "\U0024")
  
)


regus_us_locations <-  regus_us_locations %>% mutate (
  hot_desk_price_w_symb = paste(currency_short, "", hot_desk_price_w_symb ," per month" ),
  private_office_price_w_symb  = paste(currency_short, "", private_office_price_w_symb," per month" ),
  dedicated_desk_price_w_symb  = paste(currency_short, "", dedicated_desk_price_w_symb ," per month" ),
  meeting_rm_hr_price_latest_w_symb  = paste(currency_short, "", meeting_rm_hr_price_latest_w_symb  ," per hour" ),
  address_price_latest_w_symb = paste(currency_short, "", address_price_latest_w_symb  ," per month" ),
  vo_price_latest_w_symb = paste(currency_short, "", vo_price_latest_w_symb  ," per month" )
) %>%  mutate(
  hot_desk_price_latest     =   replace_na(hot_desk_price_latest, "0"),
  dedicated_desk_price_latest = replace_na(dedicated_desk_price_latest, "0"),
  private_office_price_latest = replace_na(private_office_price_latest, "0"),
  meeting_rm_hr_price_latest  = replace_na(meeting_rm_hr_price_latest, "0"),
  address_price_latest = replace_na(address_price_latest, "0"),
  vo_price_latest = replace_na(vo_price_latest, "0")
  
)  %>%  rename(
  
  hot_desk_price_latest_w_symb  = hot_desk_price_w_symb
  ,private_office_price_latest_w_symb = private_office_price_w_symb
  ,dedicated_desk_price_latest_w_symb = dedicated_desk_price_w_symb
  ,site_name = building_path
)


regus_pricing <- regus_us_locations

regus_pricing$hot_desk_price_latest <- sub("$","",regus_pricing$hot_desk_price_latest,ignore.case = FALSE, fixed=TRUE)
regus_pricing$dedicated_desk_price_latest <- sub("$","",regus_pricing$dedicated_desk_price_latest,ignore.case = FALSE, fixed=TRUE)
regus_pricing$private_office_price_latest <- sub("$","",regus_pricing$private_office_price_latest,ignore.case = FALSE, fixed=TRUE)
regus_pricing$address_price_latest <- sub("$","",regus_pricing$address_price_latest,ignore.case = FALSE, fixed=TRUE)
regus_pricing$vo_price_latest <- sub("$","",regus_pricing$vo_price_latest,ignore.case = FALSE, fixed=TRUE)


regus_pricing <- regus_pricing %>%  filter(hot_desk_price_latest !=0 | dedicated_desk_price_latest !=0 | private_office_price_latest !=0 | address_price_latest !=0 | meeting_rm_hr_price_latest !=0 | vo_price_latest !=0)


for(jj in 1: ncol(regus_pricing)){
  
  regus_pricing[ ,jj]<-   str_replace(regus_pricing[ ,jj], coll("USD  NA  per month"), "")
  
  
  
}

for(jj in 1: ncol(regus_pricing)){
  
  regus_pricing[ ,jj]<-   str_replace(regus_pricing[ ,jj], coll("USD  NA  per hour"), "")
  
  
  
}


regus_pricing$building_path <- regus_pricing$site_name

# web url build -----------------------------------------------------------

regus_pricing$web_url <- paste("https://www.regus.com/offices/united-states/", regus_pricing$city,sep ="")



# Remove temporary datasets -----------------------------------------------

regus_pricing_us <- regus_pricing

regus_pricing_us <- regus_pricing_us %>% rename( building_id = building_slug) %>% mutate(building_id = as.character(building_id))

rm("coworking"
   ,"au_vo_temp"
   ,"jsonRespParsed"
   ,"meeting_room_au"
   ,"meeting_room_full"
   ,"meeting_room_list"
   ,"meeting_room_price"
   ,"regus_us_locations"
   ,"regus_locations"
   ,"regus_locations_full"
   ,"resp"
   ,"reverse_geocode"
   ,"vo_list"
   ,"regus_pricing"
   ,"location_data_final"
   
   
)

#removing temp vectors

rm(
  
  "i"
  ,"jj"
  ,"jsonRespText"
  ,"time"
  ,"url"
)


regus_pricing_us <- regus_pricing_us  %>% mutate(
  
                     building_id = as.character(building_id)
                    ,lat = as.character(lat)
                    ,lng = as.character(lng)
                    ,vo_price_latest = as.character(vo_price_latest)
                    ,address_price_latest = as.character(address_price_latest)
                    ,private_office_price_latest = as.character(private_office_price_latest)
                    ,dedicated_desk_price_latest = as.character (dedicated_desk_price_latest)
                    ,meeting_rm_hr_price_latest = as.character (meeting_rm_hr_price_latest)
                    ,hot_desk_price_latest = as.character (hot_desk_price_latest)
                    
                    )

regus_pricing_us$country <- ifelse(regus_pricing_us$country== "United States of America", "United States", regus_pricing_us$country)


debug_message_end("20 <<")

