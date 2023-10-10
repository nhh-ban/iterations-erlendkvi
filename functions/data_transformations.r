#### Assignment 6 - Iterations ####

### Problem 2 ----

# Creating a function to transform the Vegvesenet dataset

transform_metadata_to_df <- 
  function(md) {
    md[[1]] %>% 
      map(as_tibble) %>% 
      bind_rows() %>% 
      mutate(latestData = map_chr(latestData, 
                                  1, 
                                  .default = NA_character_)) %>% 
      mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>% 
      unnest_wider(location) %>% 
      unnest_wider(latLon)
  }