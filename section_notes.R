# Notes from Discussion Section 5/21

# load packages
library(tidyverse)
library(purrr)

# read in data
warming_data <- read.csv("warming.csv") # this is the base data. for part 3 of the HW, need to use the dataset showing the losses each year

# create a function to calculate net present value
npv <- function(discount, value, time) {
  result <- value / (1 + discount)^time
  return(result)
}

# calculate npv for each year
damages_pv <- data %>% # replace with correct dataset
  mutate(pv = npv(0.05, loss, year - 2015)) # inserts the necessary parameter values

# make a graph

# what is the scc?
  # this is the simple case
damages_pv %>%
  mutate(scc = sum(pv, na.rm = TRUE))

# need to do for a variety of interest rates ...

### SENSITIVITY ANAYSIS
# create a sequence of interest rates
r <- seq(0.01, 0.08, 0.01) # from 0.01 to 0.08 in increments of 0.01

# METHOD 1: For loop

damages_pv3 <- data.frame()
# loop over the interest rate values
for (rate in r) {
  # compute the NPV for each discount rate
  pv <- npv(rate, damages_pv$loss, damages_pv$year - 2015)

  # create a temporary data frame to hold the results
  temp_df <- data.frame(
    r = rate,
    pv = pv,
    year = damages_pv$year
  )

  # bind the temporary data frame to the main data frame
  damages_pv3 <- rbind(damages_pv3, temp_df)
}


# make another graph using the scc data

# METHOD 2: Use purrr map
# see code posted to canvas (couldn't type fast enough)


