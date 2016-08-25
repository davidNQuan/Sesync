## Tidy data concept

counts_df <- data.frame(
  day = c("Monday", "Tuesday", "Wednesday"),
          wolf = c(2, 1, 3),
          hare = c(20,25,30),
          fox = c(4,4,4)
  )

## Reshaping multiple columns in category/value pairs

library(tidyr)
counts_gather <- gather(counts_df, 
                        key = 'species',
                        value = 'count',
                        wolf:fox)

counts_spread <- spread(counts_gather,
                        key = species,
                        value = count)

## Exercise 1

counts_gather <-counts_gather[-8,]

## Read comma-separated-value (CSV) files

surveys <- read.csv("data/surveys.csv")
## Subsetting and sorting

library(dplyr)
surveys_1990_winter <- filter(surveys,
                              year ==1990,
                              month %in% 1:3)

surveys_1990_winter <- select(surveys_1990_winter, -year)
##surveys_1990_winter <- select(surveys_1990_winter, -year)

sorted <- arrange(surveys1990_winter, desc(species_id), weight)

## Exercise 2
RO_indiv <- filter(surveys, 
                   species_id == "RO")
RO_indiv_select <-select(RO_indiv, 
                         record_id, 
                         sex, 
                         weight)

...

## Grouping and aggregation

surveys_1990_winter_gb <- group_by(surveys_1990_winter, species_id)

counts_1990_winter <- summarize(surveys_1990_winter_gb, count = n())
head(counts)
## Exercise 3

DM_indiv <- filter(surveys, 
                   species_id == "DM")
DM_indiv_gb  <- group_by(DM_indiv, month)
DM_indiv_gb_summary  <-summarize(DM_indiv_gb, 
                                 avg_wht = mean(weight, na.rm = TRUE), 
                                 avg_hftLgth = mean(hindfoot_length, na.rm = TRUE))

## Transformation of variables

prop_1990_winter <- mutate(counts_1990_winter, prop = count / sum(count))

## Exercise 4

# we use group_by with summarze, but can also apply filter and mutate operations on groups
#  make a list of records with minimum weight for each species
surveys_specID  <- group_by(surveys_1990_winter, species_id)
surv_filt_min <- filter(surveys_specID, 
                  weight == min(weight, na.rm = T))
# create a new column assigning rank within each species according to hindfoot length
mutate(surveys_specID, rank = row_number(hindfoot_length))


## Chainning with pipes

prop_1990_winter_piped <- surveys %>%
  filter(year == 1990, month %in% 1:3) %>%
  select(-year) %>% # select all columns but year
  group_by (species_id) %>% # group by species_id
  summarize (counts = n()) %>% # summarize with counts
  mutate (prop = count/sum(count)) # mutate into proportions
