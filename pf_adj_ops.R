library(tidyverse)
library(baseballr)
library(splitstackshape)
library(broom)
library(rvest)
library(janitor)
library(htmlTable)
library(zoo)
library(stringr)
library(kableExtra)
library(multiUS)
library(ggthemes)
library(RColorBrewer)
library(Lahman)
library(gam)
library(readxl)
library(lubridate)
library(rpart)
library(tree)
library(caTools)
library(caret)
library(cluster)
library(factoextra)
library(mclust)
library(NbClust)


# Load and prepare data
data = read_csv("https://huggingface.co/spaces/rkarthur/sabr3evaluation/raw/main/data/SABR3_data_for_assignment.csv")
data3 = read_csv("https://huggingface.co/spaces/rkarthur/sabr3evaluation/raw/main/data/SABR3_data_for_assignment.csv")

write_csv(data, "testing_data.csv")

batting <- Lahman::battingStats() |>
  filter(yearID > 2016) |>
  group_by(yearID, playerID) |>
  summarise(BABIP = weighted.mean(BABIP, PA),
            OPS = weighted.mean(OPS, PA),
            BA = weighted.mean(BA, PA))

batting2 <- Lahman::battingStats() |>
  filter(yearID > 2016 & yearID < 2022) |>
  group_by(yearID, playerID) |>
  summarize(H = sum(H),
            X2B = sum(X2B),
            X3B = sum(X3B),
            HR = sum(HR),
            BB = sum(BB),
            SO = sum(SO),
            PA = sum(PA))

batting <- left_join(batting2, batting)   

# Pull player features
feats <- Lahman::People |>
  select(playerID, bats, throws, weight, height)

# Player positions by year
positions <- Lahman::Fielding |>
  select(playerID, yearID, POS, G) |>
  filter(yearID > 2015 & POS != 'P') |>
  unique()

# Join positions with features
feats <- left_join(positions, feats)

# Determine if multiple positions are played
multiple_pos <- feats |>
  group_by(playerID, yearID) |>
  summarize(n = n())

multiple_pos <- multiple_pos |>
  mutate(multi = ifelse(n > 1, 'yes', 'no'))

# Identifying the position most played for analytical purposes
side <- feats |>
  select(playerID, yearID, G) |>
  group_by(playerID, yearID) |>
  slice_max(G) 

# Join to multi pos
multiple_pos <- multiple_pos |>
  select(playerID, yearID, multi)

feats <- left_join(feats, multiple_pos)

# Join batting and feature dfs
batting <- left_join(batting, feats, by = join_by(playerID, yearID))

# Removing duplicates
batting2 <- batting |>
  select(-POS, -G) |>
  unique()

batting3 <- batting |>
  group_by(yearID, playerID, POS) |>
  summarize(G = sum(G)) |>
  slice_max(G)

# Merging players that have two positions with equal num of games
batting3 <- batting3 |>
  distinct(yearID, playerID, G, .keep_all = TRUE)

# Joining batting2 and batting3 together to make batting
batting <- left_join(batting2, batting3)

# Identify DHs. Set 70% of PA based on games played at one position
batting <- batting |>
  mutate(POS = ifelse(multi == 'no' & PA - (G * 3.5) > (.7 * PA), 'DH', POS))

# Limiting to players with 100 PA from 2017-2020
batting2020 <- batting |>
  filter(yearID < 2021) |>
  filter(PA >= 100)

# Creating 2021 df
batting2021 <- batting |>
  filter(yearID > 2020) 

# Adding additional age columns to data
data <- data |>
  mutate(age2020 = Age - 1,
         age2019 = age2020 - 1,
         age2018 = age2020 - 2,
         age2017 = age2020 - 3
  ) |>
  rename(age2021 = Age)

# Preparing data column for joining with batting
data <- data |>
  select(bbref_id, age2021:age2017) |>
  rename(playerID = bbref_id)

# Preparing batting df for pivot_wider, namely filling in missing NA
batting <- batting |>
  select(-G)

batting <- batting |>
  group_by(playerID) |>
  arrange(yearID) |>
  fill(c(bats, throws, weight, height), .direction = 'down')

batting$POS <- ifelse(is.na(batting$POS), 'DH', batting$POS)
batting$multi <- ifelse(is.na(batting$multi), 'no', batting$multi)

# pivot_wider on batting to be in the same format as data
batting <- batting |>
  pivot_wider(names_from = yearID, values_from = c(H:BA, multi, POS))

# left_join removes all the unneeded players
data <- left_join(data, batting)
#get rid of players that didn't play in 2021
data <- data[!is.na(data$POS_2021), ]
#fill in 0 if a player had an empty stat (ex: HR)
data[is.na(data)] <- 0

#Load in Park Factors
park_factors <- read_excel("park_factors.xlsx")

#load in all regular hitters 2017-2020
bats <- read_excel("all_bats.xlsx")

#load in 2021 team for players
team2021 <- read_excel("2021_OD.xlsx")

#join team2021 to df
bats <- left_join(bats, team2021)

#some players did not get a 2021 team that should have in my initial
#data pull from fangraphs
#below are the manual team adjustments for those players
bats <- bats %>%
  mutate(
    Team2021 = case_when(
      Name == "Nick Ahmed" ~ "ARI",
      Name == "Josh Bell" ~ "WSN",
      Name == "Kole Calhoun" ~ "ARI",
      Name == "Delino DeShields" ~ "CIN",
      Name == "Todd Frazier" ~ "PIT",
      Name == "Yan Gomes" ~ "WSN",
      Name == "Brian Goodwin" ~ "CHW",
      Name == "Yuli Gourriel" ~ "HOU",
      Name == "Tommy LaStella" ~ "SFG",
      Name == "Rougned Odor" ~ "NYY",
      Name == "Jose Peraza" ~ "NYM",
      Name == "Thomas Pham" ~ "SDP",
      Name == "Josh Reddick" ~ "ARI",
      Name == "Austin Romine" ~ "CHC",
      Name == "Kyle Schwarber" ~ "WSN",
      Name == "George Springer" ~ "TOR",
      Name == "Luke Voit" ~ "NYY",
      # Add more conditions as needed
      TRUE ~ Team2021  # Keep existing Team2021 values if no match
    )
  )



#remove players who didn't play all 4 years
bats <- na.omit(bats)

#park factor adjust batter OPS

#join bats and park factors
bats <- left_join(bats, park_factors, by = "Team")

#have to make Name in same format as data$playerID

# Remove periods from bats$Name
bats <- bats %>%
  mutate(Name = gsub("\\.", "", Name)) %>%  # Remove periods
  separate(Name, into = c("FirstName", "LastName"), sep = " ")
#error indicates "Jrs" and such were removed


# Split bats$Name into First and Last names
bats <- bats %>%
  mutate(
    last_part = substr(LastName, 1, 5),
    first_part = substr(FirstName, 1, 2),
    NameID = tolower(paste0(last_part, first_part)
    ))

# Split data$playerID into letters and numbers
# Use regex to split the playerID into letters and numbers
data <- data %>%
  mutate(
    NameID = gsub("[0-9]", "", playerID),  # Remove the digits to get the name part
    NumberPart = gsub("[^0-9]", "", playerID)  # Remove non-digits to get the number part
  )

# Join based on NameID
merged_data <- bats %>%
  left_join(data, by = "NameID")

# Add the numbers back
merged_data <- merged_data %>%
  mutate(
    playerID_with_number = paste0(NameID, NumberPart)
  )

#remove NA players who don't meet the 100 PA requirements (mostly pitchers)
merged_data <- na.omit(merged_data)

#remove everything from data dataframe
bats <- merged_data |>
  select(Season, playerID, bats, Team2021, POS_2021, Team:LAVG)

#create specific park value for each batter each year
bats <- bats |>
  mutate(park_factor = case_when(
    Season == 2017 & bats == "L" ~ LPF_2017,
    Season == 2018 & bats == "L" ~ LPF_2018,
    Season == 2019 & bats == "L" ~ LPF_2019,
    Season == 2020 & bats == "L" ~ LPF_2020, 
    Season == 2017 & bats == "R" ~ RPF_2017,
    Season == 2018 & bats == "R" ~ RPF_2018,
    Season == 2019 & bats == "R" ~ RPF_2019,
    Season == 2020 & bats == "R" ~ RPF_2020,
    Season == 2017 & bats == "B" ~ (.72 * LPF_2017) + (.28 * RPF_2017),
    Season == 2018 & bats == "B" ~ (.72 * LPF_2018) + (.28 * RPF_2018),
    Season == 2019 & bats == "B" ~ (.72 * LPF_2019) + (.28 * RPF_2019),
    Season == 2020 & bats == "B" ~ (.72 * LPF_2020) + (.28 * RPF_2020)
  ))

#Adjust OPS by park factor
bats <- bats |>
  mutate(aOPS = OPS/((1+(park_factor/100))/2)
  )

#weighted OPS for players who had more than one team in the year
weighted_bats <- bats |>
  group_by(playerID, Season) |>
  summarise(
    Total_PA = sum(PA),  # Sum of plate appearances
    wOPS = sum(OPS * PA) / Total_PA  # Weighted OPS
  )

#weighted wRCplus for players who had more than one team in the year
wwrc_bats <- bats |>
  group_by(playerID, Season) |>
  summarise(
    Total_PA = sum(PA),  # Sum of plate appearances
    wwRCplus = sum(wRCplus * PA) / Total_PA  # Weighted OPS
  )

#clean bats and join wOPS and wwRCplus
bats <- bats |>
  select(Season:Team, Team2021)

bats <- left_join(bats, weighted_bats)
bats <- left_join(bats, wwrc_bats)

bats <- bats |> select(-Total_PA)

#sort for proper pivot wider
bats <- bats |>
  arrange(playerID, -Season)

#Team isn't necessary since we only care about 2021 and have weighted
#everything else
bats <- bats |>
  select(-Team)

#this gets rid of the duplicate names
bats <- unique(bats)

#pivot wider, leave playerID, bats, Team2021
bats <- bats |>
  pivot_wider(names_from = Season, values_from = c(wOPS, wwRCplus))

#create data2 to join with bats
data2 <- data |>
  select(playerID:PA4, age2020:age2017)

#Join with bats
data2 <- left_join(data2, bats)

#bring in the predictive park factor based on 3-year rolling avg
#Toronto played in Buffalo 2020, Rangers new stadium debuted 2021,
#so those park factors aren't perfect
pf2 <- park_factors |>
  select(Team, LAVG, RAVG)

data2 <- left_join(data2, pf2, by = c("Team2021" = "Team"))

#performing OPS prediction

#first, MARCEL
data <- data |>
  mutate(avg_ops = ((5*OPSY1) + (4*OPSY2) + (3*OPSY3))/12,
         reliability = (PA1 + PA2 + PA3)/((PA1 + PA2 + PA3) + 1200),
         M_regressed_ops = (reliability * avg_ops) + ((1-reliability) * .720),
         age_adjustment = ifelse(age2021 > 29, ((age2021 - 29) * -.003), 0),
         age_adjustment = ifelse(age2021 < 29, ((29 - age2021) * .006), age_adjustment),
         MARCEL_OPS = (1 + age_adjustment) * M_regressed_ops
  )

#separate regressed OPS calculation for dOPS (better than Marcel)
data2 <- data2 |>
  mutate(avg_ops = ((5*wOPS_2020) + (4*wOPS_2019) + (3*wOPS_2018))/12,
         reliability = (PA1 + PA2 + PA3)/((PA1 + PA2 + PA3) + 1200),
         D_regressed_ops = (reliability * avg_ops) + ((1-reliability) * .720),
         age_adjustment = ifelse(age2021 > 29, ((age2021 - 29) * -.003), 0),
         age_adjustment = ifelse(age2021 < 29, ((29 - age2021) * .006), age_adjustment),
         MARCEL_OPS = (1 + age_adjustment) * D_regressed_ops
  )

#create specific park value for each batter 2021
data2 <- data2 |>
  mutate(park_factor = case_when(
    bats == "L" ~ LAVG,
    bats == "R" ~ RAVG,
    bats == "B" ~ (.72 * LAVG) + (.28 * RAVG)
  ))

#weighted average wRCplus
data2 <- data2 |>
  mutate(avg_wRCplus = ((5*wwRCplus_2020) + (4*wwRCplus_2019) + (3*wwRCplus_2018))/12,
         wRC_reliability = (PA1 + PA2 + PA3)/((PA1 + PA2 + PA3) + 1200),
         D_regressed_wRCplus = (reliability * avg_wRCplus) + ((1-reliability) * 100)
  )


#Adjust MARCEL_OPS by park factor
data2 <- data2 |>
  mutate(dOPS = MARCEL_OPS*((1+(park_factor/100))/2),
         dOPS = ifelse(D_regressed_wRCplus > 100, dOPS + ((((D_regressed_wRCplus/100)*.72)-.72)/5), dOPS),
         dOPS = ifelse(D_regressed_wRCplus < 100, dOPS + ((((D_regressed_wRCplus/100)*.72)-.72)/5), dOPS)
  )

#get the players used in the original dataset

#data is separated if played for multiple teams, this compiles the stats
batting <- Lahman::battingStats() |>
  filter(yearID > 2016) |>
  group_by(yearID, playerID) |>
  summarise(BABIP2 = weighted.mean(BABIP, PA),
            OPS2 = weighted.mean(OPS, PA),
            BA2 = weighted.mean(BA, PA))

batting2 <- Lahman::battingStats() |>
  filter(yearID > 2017) |>
  group_by(yearID, playerID) |>
  summarize(H = sum(H),
            X2B = sum(X2B),
            X3B = sum(X3B),
            HR = sum(HR),
            BB = sum(BB),
            SO = sum(SO))

#gives the proper year-long stats
batting <- left_join(batting2, batting)           

batting <- batting |>
  arrange(playerID)

#rename variables
batting <- batting |>
  rename(OPS = OPS2,
         BABIP = BABIP2,
         BA = BA2) |>
  filter(yearID == 2021)

#only players that are interested in remain
data2 <- left_join(data2, batting) |>
  na.omit()

#rough error calculations
our_rmse = sqrt(mean((data2$dOPS-data2$OPS)**2))
our_mae = mean(abs(data2$dOPS-data2$OPS))
our_corr = cor(data2$dOPS, data2$OPS)
cat(our_rmse, our_mae, our_corr)

#scatter plot with lm drawn
data2 |>
  ggplot(aes(dOPS, OPS)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_bw() +
  xlab("Predicted OPS") +
  ylab("Actual OPS")

#renaming
data2 <- data2 |>
  rename(BETTER_THAN_MARCEL_OPS = dOPS,
         bbref_id = playerID)

#create final df that removes unneccessary columns, adds the data$MARCEL_OPS to 
#to final df

#trimming
final <- data2 |>
  select(bbref_id:PA4,bats:BETTER_THAN_MARCEL_OPS)

#remove preliminary OPS
final <- final |> select(-MARCEL_OPS)

#create marcel df that has brredif and marcel ops only
marcel <- data |>
  select(playerID, MARCEL_OPS)

#join marcel to final
final <- left_join(final, marcel, by = c("bbref_id" = "playerID"))

#save excel file
write.csv(final, "C:/Users/dresi/Documents/SABRlvl3.csv", row.names = FALSE)
