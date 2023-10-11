#### Assignment 6 - Iterations ####

### Problem 2 ----

# Creating a function to transform the Vegvesenet dataset
transform_metadata_to_df <- 
  function(df) {
    df[[1]] %>% 
      map(as_tibble) %>% 
      bind_rows() %>% 
      mutate(latestData = map_chr(latestData, 
                                  1, 
                                  .default = NA_character_)) %>% 
      mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
      unnest_wider(location) %>% 
      unnest_wider(latLon)
  }

# Creating a function to extract an ISO8601 time (with an offset, if applicable)
to_iso8601 <- 
  function(dttm, offset_days = 0) {
    adjusted_for_offset <- as_datetime(dttm) + days(offset_days)
    adjusted_for_iso <- iso8601(anytime(adjusted_for_offset, tz = "UTC"))
    return(paste0(adjusted_for_iso, "Z"))
  }

to_iso8601("2023-10-10 15:20:00") # Testing with today's date
to_iso8601("2016-09-01 10:11:12", -4) # Testing with an offset
