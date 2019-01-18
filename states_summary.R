# load dplyr
library(dplyr)

# calculate number of all gun incidents in each state as a data frame
state_gun_incidents <- function(gun_data){
  gun_data$Incidents <- 1
  state_obs <- aggregate(Incidents ~ state, FUN=sum, data = gun_data)
  state_obs <- state_obs %>% filter(state != "District of Columbia")
  state_obs$code <- state.abb
  state_obs
}
