
# combining final datasets ---------------------------------------------------

geospatial_data <-  dplyr::bind_rows (
    
          wework_pricing_us

          ,regus_pricing_us
          
)

# Removing temp data frame objects and functions --------------------------


rm(
  
   "wework_pricing_us"
  
  ,"regus_pricing_us"
  
  
)


# Publishing output -------------------------------------------------------


write.csv( geospatial_data, "Output/geospatial_data.csv", row.names=FALSE, na="")
