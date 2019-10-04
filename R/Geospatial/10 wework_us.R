debug_message_start("10 >>")

debug_message_l2("Wework US Website Data")


#Wework US-------------------------------------

#list of wework url's to extract the content-------------------------------------

url <- c("https://www.wework.com/l/united-states")

time <- Sys.time()

#extracting we work location info-------------------------

location_data_final <- data.frame()


#wework_1 (site full name dataset)---------------------------------------------------------

for(i in url) 
{
  
  webpage <- read_html(i)
  location_data <- 
    html_nodes(webpage, ".building-card__address") %>%
    html_text() %>%
    enframe()
  location_data$urlid <- i
  location_data_final <- rbind(location_data_final,location_data)
  
}

wework_1<- as_data_frame(location_data_final$value) %>%  select (value) %>%  mutate(
  value_1 = value
) %>% rename( site_name= value, building_name = value_1)

#wework_2 (site short name dataset)---------------------------------------------------------

location_data_final <- data.frame()

for(i in url) 
{
  
  webpage <- read_html(i)
  location_data <- 
    html_nodes(webpage, ".mb0") %>%
    html_text() %>%
    enframe()
  location_data$urlid <- i
  location_data_final <- rbind(location_data_final,location_data)
  
}

wework_2 <- location_data_final %>%  select (value, urlid)%>%  rename( site_name_short= value, web_url=urlid )


#wework_3 (Office Price dataset)---------------------------------------------------------

location_data_final <- data.frame()


for(i in url) 
{
  
  webpage <- read_html(i)
  location_data <- 
    html_nodes(webpage, ".office-pricing") %>%
    html_text() %>%
    enframe()
  location_data$urlid <- i
  location_data_final <- rbind(location_data_final,location_data)
  
}

wework_3 <- as_data_frame(location_data_final$value) %>%  select (value) %>%  rename ( private_office_price = value)


#wework_4 (Dedicated Desk Price dataset)---------------------------------------------------------

location_data_final <- data.frame()


for(i in url) 
{
  
  webpage <- read_html(i)
  location_data <- 
    html_nodes(webpage, ".dedicated-desk-pricing") %>%
    html_text() %>%
    enframe()
  location_data$urlid <- i
  location_data_final <- rbind(location_data_final,location_data)
  
}

wework_4 <- as_data_frame(location_data_final$value) %>%  select (value) %>%  rename ( dedicated_desk_price = value)



#wework_5 (Hot Desk Price dataset)---------------------------------------------------------

location_data_final <- data.frame()


for(i in url) 
{
  
  webpage <- read_html(i)
  location_data <- 
    html_nodes(webpage, ".hot-desk-pricing") %>%
    html_text() %>%
    enframe()
  location_data$urlid <- i
  location_data_final <- rbind(location_data_final,location_data)
  
}

wework_5 <- as_data_frame(location_data_final$value) %>%  select (value) %>%  rename ( hot_desk_price = value)


#wework_6 (Full Info dataset)---------------------------------------------------------


location_data_final <- data.frame()


for(i in url) 
{
  
  webpage <- read_html(i)
  location_data <- 
    html_nodes(webpage, ".mb0, .ray-card__content") %>%
    html_text() %>%
    enframe()
  location_data$urlid <- i
  location_data_final <- rbind(location_data_final,location_data)
  
}


wework_6.a <- location_data_final %>%  select (value) %>%  mutate (
  row_num = row_number(),
  filter_check = row_num%%2==0 
) %>%  filter(
  filter_check == "TRUE"
)%>%  select (
  -filter_check
)%>%  select(
  -row_num 
) %>%  rename(
  site_name_short = value
)


wework_6.b <- location_data_final %>%  select (value) %>%  mutate (
  row_num = row_number(),
  filter_check = row_num%%2!=0 
) %>%  filter(
  filter_check == "TRUE"
)%>%  select (
  -filter_check
)%>%  select(
  -row_num 
) 

wework_6 <- cbind(wework_6.a, wework_6.b)




wework_6 <- wework_6 %>%  rename ( full_info = value) %>% mutate(
  full_info = str_squish (full_info)
)



wework_6 <- sqldf("select * from wework_6 where full_info not like '%Pricing for this location is not yet available%'")

wework_6 <- sqldf("select * from wework_6 where full_info not like '%Move in ahead of the curve with special pre-opening rates%'")


wework_6.1 <- str_split_fixed(wework_6$full_info, "Starting prices", 2)
wework_6.1 <- as.data.frame(wework_6.1)


wework_6.2 <- as.data.frame(wework_6.1$V2) %>%  rename (value = `wework_6.1$V2`)

wework_6.2 <- separate(wework_6.2, value, c("Private Office", "Price"), sep = "Private Office")

wework_6.2 <- separate(wework_6.2, Price, c("private_office_price", "Price"), sep = "Dedicated Desk") 

wework_6.2 <- separate(wework_6.2, Price, c("dedicated_desk_price", "hot_desk_price"), sep = "Hot Desk")

wework_interim <- cbind(wework_6, wework_6.1, wework_6.2) %>%  select(
  -full_info
  ,-V2
) %>%  rename(
  site_name = V1
) 


wordcount_final <- data.frame()
for(i in 1: nrow(wework_interim)){
  
  wordcount_temp <-  enframe ( wordcount(wework_interim$site_name_short[i]) ) %>%  select (value)
  
  wordcount_final  <- rbind(wordcount_final, wordcount_temp)
  
  
  
}


wework_pricing <- cbind(wework_interim, wordcount_final) %>%  rename (
  word_count= value
) %>%  select (
  -`Private Office`) %>%  mutate(
    building_name = word(site_name, word_count+1, -1)
  ) %>%  select(
    -word_count
  ) %>% mutate(
    site_name = building_name
    ,date_time_Stamp = format(Sys.time(),  "%d-%m-20%y")
    ,country = "United States"
    ,company =  "wework"
    , currency_name = "US Dollar"
    , currency_short = "USD"
    , web_url = "https://www.wework.com/l/united-states"
  )


wework_pricing$private_office_price <- str_replace(wework_pricing$private_office_price, "/mo", "")
wework_pricing$private_office_price <- str_replace(wework_pricing$private_office_price, ",", "")
wework_pricing$private_office_price <- str_replace(wework_pricing$private_office_price, "Unavailable", "")
wework_pricing$private_office_price  <- ifelse(is.na(wework_pricing$private_office_price), "", wework_pricing$private_office_price )


wework_pricing$dedicated_desk_price <- str_replace(wework_pricing$dedicated_desk_price, "/mo", "")
wework_pricing$dedicated_desk_price <- str_replace(wework_pricing$dedicated_desk_price, ",", "")
wework_pricing$dedicated_desk_price <- str_replace(wework_pricing$dedicated_desk_price, "Unavailable", "")
wework_pricing$dedicated_desk_price <- ifelse(is.na(wework_pricing$dedicated_desk_price), "", wework_pricing$dedicated_desk_price)


wework_pricing$hot_desk_price <- str_replace(wework_pricing$hot_desk_price, "/mo", "")
wework_pricing$hot_desk_price <- str_replace(wework_pricing$hot_desk_price, ",", "")
wework_pricing$hot_desk_price <- str_replace(wework_pricing$hot_desk_price, "Unavailable", "")
wework_pricing$hot_desk_price <- ifelse(is.na(wework_pricing$hot_desk_price), "", wework_pricing$hot_desk_price)


#lat long corordinates ------------------------------------------------------------------

output_final_lat  <- data.frame()
output_final_lng <- data.frame()
output_final_state  <- data.frame()


for (i in 1:length(wework_pricing$site_name)) {
  
  output_temp <- opencage_forward(placename = wework_pricing$site_name[i], key = "d3f30e7282414d52ba36461e84613c34" )
  output_final_lat  <- bind_rows (output_final_lat, enframe( output_temp$results$geometry.lat[[1]] ))
  output_final_lng  <- bind_rows (output_final_lng, enframe( output_temp$results$geometry.lng[[1]] ))
  output_final_state  <- bind_rows (output_final_state, enframe( output_temp$results$components.state[[1]]))
  
}


wework_pricing$lat <- output_final_lat$value
wework_pricing$lng <- output_final_lng$value
wework_pricing$state <- output_final_state$value 

# Reverse Geocoding -------------------------------------------------------

reverse_geocode<- revgeo(longitude=wework_pricing$lng, latitude=wework_pricing$lat, output='frame')

wework_pricing <- wework_pricing %>% mutate(
  street_name = word(site_name, 2, 3),
  city        = reverse_geocode$city,
  postcode    = reverse_geocode$zip ) 


# web_url_build ---------------------------------------------------------------

url <- "https://api-proxy.wework.com/locations/api/v1/geogroupings"

resp <- GET(url)

http_type(resp)

jsonRespText<-content(resp,as="text") 

jsonRespParsed<-content(resp,as="parsed") 

web_api <-fromJSON(jsonRespText)

buildings <- web_api$buildings

geogroupings <- web_api$geogroupings



#removing list data type columns from buildings data frame

buildings_df <- buildings %>% select (-geogroupings, -slugs)


#geogroupings id's from buildings --as a data frame

geogroupings_array<-do.call("rbind", buildings$geogroupings)

geo_groupings_df <- as.data.frame.array(geogroupings_array)

#combine buildings and geogroupings dataframe columns for US records using geogrouping id for 'United States'

us_wework_buildings <- cbind(buildings_df, geo_groupings_df) %>% filter (V1 == "a4620adc-1ea7-11e6-ab4a-0a488af3e541" | 
                                                                           V2 ==  "a4620adc-1ea7-11e6-ab4a-0a488af3e541" |
                                                                           V3 ==  "a4620adc-1ea7-11e6-ab4a-0a488af3e541" |
                                                                           V4 ==  "a4620adc-1ea7-11e6-ab4a-0a488af3e541" |
                                                                           V5==  "a4620adc-1ea7-11e6-ab4a-0a488af3e541" |
                                                                           V6 ==  "a4620adc-1ea7-11e6-ab4a-0a488af3e541" |
                                                                           V7 ==  "a4620adc-1ea7-11e6-ab4a-0a488af3e541"  ) %>%  select(
                                                                             id, slug, path, name 
                                                                           ) %>%  rename (
                                                                             
                                                                             building_id = id
                                                                             ,building_slug = slug
                                                                             ,building_path = path
                                                                             ,building_name = name
                                                                             
                                                                           )


wework_pricing <- wework_pricing %>% left_join(
  us_wework_buildings, by = c( "site_name_short" = "building_name")
) %>% filter(
  !is.na(building_path)
) %>% mutate(
  
  web_url = paste("https://www.wework.com", building_path, sep ="")  
)


# Manual Fixes---------------------------------------------------------------- 

# postcode fix ----------------------------------------------------------------
wework_pricing$postcode <- as.character (wework_pricing$postcode)
wework_pricing <- wework_pricing %>% mutate (postcode = ifelse (( str_length(postcode)) <= 5, postcode, str_sub(postcode, start = 1, end = 5)))

wework_pricing$postcode <- as.character (wework_pricing$postcode)


# Country fix -------------------------------------------------------------

wework_pricing$country <- ifelse( wework_pricing$country=="united-states", "United States", wework_pricing$country)


# Currency Fix ------------------------------------------------------------

wework_pricing <-  wework_pricing %>% mutate (
  hot_desk_price_w_symb = currency(hot_desk_price, "\U0024"),
  private_office_price_w_symb = currency(private_office_price, "\U0024"),
  dedicated_desk_price_w_symb = currency(dedicated_desk_price, "\U0024")  )

wework_pricing <-  wework_pricing %>% mutate (
  hot_desk_price_w_symb = paste(currency_short, " ", hot_desk_price_w_symb ," per month" ),
  private_office_price_w_symb  = paste(currency_short, " ", private_office_price_w_symb," per month" ),
  dedicated_desk_price_w_symb  = paste(currency_short, " ", dedicated_desk_price_w_symb ," per month" ) )

# Outside US locations Fix ------------------------------------------------
wework_pricing <- wework_pricing %>%  filter (city !="Brisbane " , city != "Toronto")

# wework Final Data Model Selection----------------------------------------------

wework_pricing_us <- wework_pricing %>%  select("company" 
                                                ,"building_id"
                                                ,"building_name"
                                                ,"building_slug"
                                                ,"building_path"
                                                ,"site_name" 
                                                ,"site_name_short" 
                                                ,"street_name"
                                                ,"city"
                                                ,"state"
                                                ,"country" 
                                                ,"postcode"  
                                                ,"lat"
                                                ,"lng" 
                                                ,"web_url"
                                                ,"currency_name" 
                                                ,"currency_short"             
                                                ,"hot_desk_price"              
                                                ,"dedicated_desk_price"       
                                                ,"private_office_price"        
                                                ,"hot_desk_price_w_symb"      
                                                ,"dedicated_desk_price_w_symb" 
                                                ,"private_office_price_w_symb"
                                                ,"date_time_Stamp" )

# Dropping all temp tables/ objects ------------------------------------------------

#removing temp dataframe objects

rm (
  "buildings"
  ,"buildings_df"
  ,"geo_groupings_df"
  ,"geogroupings"
  ,"geogroupings_array"
  ,"jsonRespParsed"
  ,"location_data"
  ,"location_data_final"
  ,"output_final_lat"
  ,"output_final_lng"
  ,"output_final_state"
  ,"output_temp"
  ,"reverse_geocode"
  ,"resp"
  ,"us_wework_buildings"
  ,"web_api"
  ,"webpage"                
  ,"wework_1"                
  ,"wework_2"
  ,"wework_3"
  ,"wework_4"
  ,"wework_5"
  ,"wework_6"
  ,"wework_6.1"
  ,"wework_6.2"
  ,"wework_6.a"
  ,"wework_6.b"
  ,"wework_interim"
  ,"wordcount_final"
  ,"wordcount_temp"
  ,"wework_pricing"          
  
  
) 

#removing temp vectors

rm(
  
  "i"
  ,"jsonRespText"
  ,"time"
  ,"url"
)





# Cosmetic Final Data Fix (Part 1)-------------------------------------------------


wework_pricing_us <- wework_pricing_us %>% mutate(
  
  coworking_url = web_url
  ,hot_desk_price_latest = hot_desk_price
  ,hot_desk_price_orgnl = hot_desk_price
  ,hot_desk_price_latest_w_symb = hot_desk_price_w_symb
  ,hot_desk_price_orgnl_w_symb = hot_desk_price_w_symb
  ,hot_desk_price_spl_offer_check = "FALSE"
  ,dedicated_desk_price_latest = dedicated_desk_price
  ,dedicated_desk_price_orgnl = dedicated_desk_price
  ,dedicated_desk_price_latest_w_symb = dedicated_desk_price_w_symb
  ,dedicated_desk_price_orgnl_w_symb = dedicated_desk_price_w_symb
  ,dedicated_desk_price_spl_offer_check = "FALSE"
  ,private_office_price_latest = private_office_price
  ,private_office_price_orgnl = private_office_price
  ,private_office_price_latest_w_symb = private_office_price_w_symb
  ,private_office_price_orgnl_w_symb = private_office_price_w_symb
  ,private_office_price_spl_offer_check = "FALSE"
  ,coworking_check =  "TRUE"
  
)


wework_pricing_us <-  wework_pricing_us %>%  select (
  
  date_time_Stamp
  ,company
  ,building_id
  ,building_name
  ,building_slug
  ,building_path
  ,site_name
  ,street_name
  ,city
  ,state
  ,country
  ,postcode
  ,lat
  ,lng
  ,coworking_url
  ,web_url
  ,currency_name
  ,currency_short
  ,hot_desk_price_latest
  ,hot_desk_price_orgnl
  ,hot_desk_price_w_symb
  ,hot_desk_price_latest_w_symb
  ,hot_desk_price_orgnl_w_symb
  ,hot_desk_price_spl_offer_check
  ,dedicated_desk_price_latest
  ,dedicated_desk_price_orgnl
  ,dedicated_desk_price_w_symb
  ,dedicated_desk_price_latest_w_symb
  ,dedicated_desk_price_orgnl_w_symb
  ,dedicated_desk_price_spl_offer_check
  ,private_office_price_latest
  ,private_office_price_orgnl
  ,private_office_price_w_symb
  ,private_office_price_latest_w_symb
  ,private_office_price_orgnl_w_symb
  ,private_office_price_spl_offer_check
  ,coworking_check
  
  
)




for(jj in 1: ncol(wework_pricing_us)){
  
  wework_pricing_us[ ,jj]<-   str_replace(wework_pricing_us[ ,jj], ",", "")
  
  
  
}


rm("jj")


# Cosmetic Final Data Fix  (Part 2)-------------------------------------------------


wework_pricing_us <- wework_pricing_us %>% mutate (
  
  hot_desk_price_latest_w_symb =  str_replace (hot_desk_price_latest_w_symb, "USD   ", "USD ")
  ,hot_desk_price_latest_w_symb =  str_replace (hot_desk_price_latest_w_symb, "  per", " per")
  
  ,hot_desk_price_orgnl_w_symb =  str_replace (hot_desk_price_orgnl_w_symb, "USD   ", "USD ")
  ,hot_desk_price_orgnl_w_symb =  str_replace (hot_desk_price_orgnl_w_symb, "  per", " per")
  
  
  
  ,dedicated_desk_price_latest_w_symb =  str_replace (dedicated_desk_price_latest_w_symb, "USD   ", "USD ")
  ,dedicated_desk_price_latest_w_symb =  str_replace (dedicated_desk_price_latest_w_symb, "  per", " per")
  
  ,dedicated_desk_price_orgnl_w_symb =  str_replace (dedicated_desk_price_orgnl_w_symb, "USD   ", "USD ")
  ,dedicated_desk_price_orgnl_w_symb =  str_replace (dedicated_desk_price_orgnl_w_symb, "  per", " per")
  
  
  ,private_office_price_latest_w_symb =  str_replace (private_office_price_latest_w_symb, "USD   ", "USD ")
  ,private_office_price_latest_w_symb =  str_replace (private_office_price_latest_w_symb, "  per", " per")
  
  ,private_office_price_orgnl_w_symb =  str_replace (private_office_price_orgnl_w_symb, "USD   ", "USD ")
  ,private_office_price_orgnl_w_symb =  str_replace (private_office_price_orgnl_w_symb, "  per", " per")
  
  
)



for(jj in 1: ncol(wework_pricing_us)){
  
  wework_pricing_us[ ,jj]<-   str_replace(wework_pricing_us[ ,jj], coll("USD NA per month"), "doesn't exist")
  
  
  
}



for(jj in 1: ncol(wework_pricing_us)){
  
  wework_pricing_us[ ,jj]<-   str_replace(wework_pricing_us[ ,jj], coll("USD $0 per month"), "doesn't exist")
  
  
  
}





rm("jj"
)


wework_pricing_us$country <- ifelse(wework_pricing_us$site_name =="310 Edward Street Brisbane City QLD 4000", "Australia", wework_pricing_us$country )



debug_message_end("10 <<")





