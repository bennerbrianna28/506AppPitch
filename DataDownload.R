library(dataRetrieval)
library(tidyverse)

gauges <- c(
  "USGS-12213100",
  "USGS-12211200",
  "USGS-12210700",
  "USGS-12210000",
  "USGS-12208000",
  "USGS-12205000"
)

params <- c("00060", "00065")

# loop through the sites and parameters
dat_list <- list()
i <- 1

for (g in gauges) {
  for (p in params) {
    
    cat("Downloading", g, p, "\n")
    
    dat_list[[i]] <- read_waterdata_daily(
      monitoring_location_id = g,
      parameter_code = p,
      time = c("1990-10-01", as.character(Sys.Date()))
    )
    
    i <- i + 1
  }
}

dat <- bind_rows(dat_list)


# save dat
saveRDS(dat, "AppPitch/usgs_daily_data.rds")
