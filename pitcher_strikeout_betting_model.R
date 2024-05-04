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
library(tidyr)

#load dataframes
library(readxl)
all2020 <- read_excel("all2020.xlsx")
all2021 <- read_excel("all2021.xlsx")
all2022 <- read_excel("all2022.xlsx")
all2023 <- read_excel("all2023.xlsx")
info2020 <- read_excel("info2020.xlsx")
info2021 <- read_excel("info2021.xlsx")
info2022 <- read_excel("info2022.xlsx")
info2023 <- read_excel("info2023.xlsx")

#add game descriptions to the play by play data
df2020 <- left_join(all2020, info2020, by = "game_id")

df2021 <- left_join(all2021, info2021, by = "game_id")

df2022 <- left_join(all2022, info2022, by = "game_id")

df2023 <- left_join(all2023, info2023, by = "game_id")

#### Creating Game number for each team, each season
#2020

#get all games for Angels
ana <- df2020 |> filter(home_team_id == "ANA")
#total rows shows how many games played
ana <- ana |> group_by(game_id) |>summarize(n_games=(n()/2))
#adds game number of season column
ana <- ana |> mutate(game = c(1:31))
#removes unnecessary column
ana <- ana |> select(-n_games)

ARI <- df2020 |> filter(home_team_id == "ANA")
ARI <- ARI |> group_by(game_id) |>summarize(n_games=(n()/2))
ARI <- ARI |> mutate(game = c(1:31))
ARI <- ARI |> select(-n_games)

ATL <- df2020 |> filter(home_team_id == "ATL")
ATL <- ATL |> group_by(game_id) |>summarize(n_games=(n()/2))
ATL <- ATL |> mutate(game = c(1:30))
ATL <- ATL |> select(-n_games)

BAL <- df2020 |> filter(home_team_id == "BAL")
BAL <- BAL |> group_by(game_id) |>summarize(n_games=(n()/2))
BAL <- BAL |> mutate(game = c(1:33))
BAL <- BAL |> select(-n_games)

BOS <- df2020 |> filter(home_team_id == "BOS")
BOS <- BOS |> group_by(game_id) |>summarize(n_games=(n()/2))
BOS <- BOS |> mutate(game = c(1:31))
BOS <- BOS |> select(-n_games)

CHA <- df2020 |> filter(home_team_id == "CHA")
CHA <- CHA |> group_by(game_id) |>summarize(n_games=(n()/2))
CHA <- CHA |> mutate(game = c(1:30))
CHA <- CHA |> select(-n_games)

CHN <- df2020 |> filter(home_team_id == "CHN")
CHN <- CHN |> group_by(game_id) |>summarize(n_games=(n()/2))
CHN <- CHN |> mutate(game = c(1:33))
CHN <- CHN |> select(-n_games)

CIN <- df2020 |> filter(home_team_id == "CIN")
CIN <- CIN |> group_by(game_id) |>summarize(n_games=(n()/2))
CIN <- CIN |> mutate(game = c(1:29))
CIN <- CIN |> select(-n_games)

CLE <- df2020 |> filter(home_team_id == "CLE")
CLE <- CLE |> group_by(game_id) |>summarize(n_games=(n()/2))
CLE <- CLE |> mutate(game = c(1:30))
CLE <- CLE |> select(-n_games)

COL <- df2020 |> filter(home_team_id == "COL")
COL <- COL |> group_by(game_id) |>summarize(n_games=(n()/2))
COL <- COL |> mutate(game = c(1:30))
COL <- COL |> select(-n_games)

DET <- df2020 |> filter(home_team_id == "DET")
DET <- DET |> group_by(game_id) |>summarize(n_games=(n()/2))
DET <- DET |> mutate(game = c(1:27))
DET <- DET |> select(-n_games)

HOU <- df2020 |> filter(home_team_id == "HOU")
HOU <- HOU |> group_by(game_id) |>summarize(n_games=(n()/2))
HOU <- HOU |> mutate(game = c(1:28))
HOU <- HOU |> select(-n_games)

KCA <- df2020 |> filter(home_team_id == "KCA")
KCA <- KCA |> group_by(game_id) |>summarize(n_games=(n()/2))
KCA <- KCA |> mutate(game = c(1:30))
KCA <- KCA |> select(-n_games)

LAN <- df2020 |> filter(home_team_id == "LAN")
LAN <- LAN |> group_by(game_id) |>summarize(n_games=(n()/2))
LAN <- LAN |> mutate(game = c(1:30))
LAN <- LAN |> select(-n_games)

MIA <- df2020 |> filter(home_team_id == "MIA")
MIA <- MIA |> group_by(game_id) |>summarize(n_games=(n()/2))
MIA <- MIA |> mutate(game = c(1:26))
MIA <- MIA |> select(-n_games)

MIL <- df2020 |> filter(home_team_id == "MIL")
MIL <- MIL |> group_by(game_id) |>summarize(n_games=(n()/2))
MIL <- MIL |> mutate(game = c(1:29))
MIL <- MIL |> select(-n_games)

MIN <- df2020 |> filter(home_team_id == "MIN")
MIN <- MIN |> group_by(game_id) |>summarize(n_games=(n()/2))
MIN <- MIN |> mutate(game = c(1:31))
MIN <- MIN |> select(-n_games)

NYA <- df2020 |> filter(home_team_id == "NYA")
NYA <- NYA |> group_by(game_id) |>summarize(n_games=(n()/2))
NYA <- NYA |> mutate(game = c(1:31))
NYA <- NYA |> select(-n_games)

NYN <- df2020 |> filter(home_team_id == "NYN")
NYN <- NYN |> group_by(game_id) |>summarize(n_games=(n()/2))
NYN <- NYN |> mutate(game = c(1:29))
NYN <- NYN |> select(-n_games)

OAK <- df2020 |> filter(home_team_id == "OAK")
OAK <- OAK |> group_by(game_id) |>summarize(n_games=(n()/2))
OAK <- OAK |> mutate(game = c(1:32))
OAK <- OAK |> select(-n_games)

PHI <- df2020 |> filter(home_team_id == "PHI")
PHI <- PHI |> group_by(game_id) |>summarize(n_games=(n()/2))
PHI <- PHI |> mutate(game = c(1:32))
PHI <- PHI |> select(-n_games)

PIT <- df2020 |> filter(home_team_id == "PIT")
PIT <- PIT |> group_by(game_id) |>summarize(n_games=(n()/2))
PIT <- PIT |> mutate(game = c(1:32))
PIT <- PIT |> select(-n_games)

SDN <- df2020 |> filter(home_team_id == "SDN")
SDN <- SDN |> group_by(game_id) |>summarize(n_games=(n()/2))
SDN <- SDN |> mutate(game = c(1:32))
SDN <- SDN |> select(-n_games)

SEA <- df2020 |> filter(home_team_id == "SEA")
SEA <- SEA |> group_by(game_id) |>summarize(n_games=(n()/2))
SEA <- SEA |> mutate(game = c(1:24))
SEA <- SEA |> select(-n_games)

SFN <- df2020 |> filter(home_team_id == "SFN")
SFN <- SFN |> group_by(game_id) |>summarize(n_games=(n()/2))
SFN <- SFN |> mutate(game = c(1:33))
SFN <- SFN |> select(-n_games)

SLN <- df2020 |> filter(home_team_id == "SLN")
SLN <- SLN |> group_by(game_id) |>summarize(n_games=(n()/2))
SLN <- SLN |> mutate(game = c(1:27))
SLN <- SLN |> select(-n_games)

TBA <- df2020 |> filter(home_team_id == "TBA")
TBA <- TBA |> group_by(game_id) |>summarize(n_games=(n()/2))
TBA <- TBA |> mutate(game = c(1:29))
TBA <- TBA |> select(-n_games)

TEX <- df2020 |> filter(home_team_id == "TEX")
TEX <- TEX |> group_by(game_id) |>summarize(n_games=(n()/2))
TEX <- TEX |> mutate(game = c(1:30))
TEX <- TEX |> select(-n_games)

TOR <- df2020 |> filter(home_team_id == "TOR")
TOR <- TOR |> group_by(game_id) |>summarize(n_games=(n()/2))
TOR <- TOR |> mutate(game = c(1:26))
TOR <- TOR |> select(-n_games)

WAS <- df2020 |> filter(home_team_id == "WAS")
WAS <- WAS |> group_by(game_id) |>summarize(n_games=(n()/2))
WAS <- WAS |> mutate(game = c(1:33))
WAS <- WAS |> select(-n_games)

#create df that adds all game numbers and IDs
team2020 <- bind_rows(ana, ARI, ATL, BAL, BOS, CHA, CHN, CIN, CLE, COL, DET, 
                      HOU, KCA, LAN, MIA, MIL, MIN, NYA, NYN, OAK, PHI, PIT,
                      SDN, SEA, SFN, SLN, TBA, TEX, TOR, WAS)

#adds game number columns to primary df, then removes all duplicates
df2020 <- merge(df2020, team2020, by = "game_id")
df2020 <- df2020 |> unique()

#2021
ana <- df2021 |> filter(home_team_id == "ANA")
ana <- ana |> group_by(game_id) |>summarize(n_games=(n()/2))
ana <- ana |> mutate(game = c(1:82))
ana <- ana |> select(-n_games)

ARI <- df2021 |> filter(home_team_id == "ANA")
ARI <- ARI |> group_by(game_id) |>summarize(n_games=(n()/2))
ARI <- ARI |> mutate(game = c(1:82))
ARI <- ARI |> select(-n_games)

ATL <- df2021 |> filter(home_team_id == "ATL")
ATL <- ATL |> group_by(game_id) |>summarize(n_games=(n()/2))
ATL <- ATL |> mutate(game = c(1:80))
ATL <- ATL |> select(-n_games)

BAL <- df2021 |> filter(home_team_id == "BAL")
BAL <- BAL |> group_by(game_id) |>summarize(n_games=(n()/2))
BAL <- BAL |> mutate(game = c(1:81))
BAL <- BAL |> select(-n_games)

BOS <- df2021 |> filter(home_team_id == "BOS")
BOS <- BOS |> group_by(game_id) |>summarize(n_games=(n()/2))
BOS <- BOS |> mutate(game = c(1:81))
BOS <- BOS |> select(-n_games)

CHA <- df2021 |> filter(home_team_id == "CHA")
CHA <- CHA |> group_by(game_id) |>summarize(n_games=(n()/2))
CHA <- CHA |> mutate(game = c(1:81))
CHA <- CHA |> select(-n_games)

CHN <- df2021 |> filter(home_team_id == "CHN")
CHN <- CHN |> group_by(game_id) |>summarize(n_games=(n()/2))
CHN <- CHN |> mutate(game = c(1:81))
CHN <- CHN |> select(-n_games)

CIN <- df2021 |> filter(home_team_id == "CIN")
CIN <- CIN |> group_by(game_id) |>summarize(n_games=(n()/2))
CIN <- CIN |> mutate(game = c(1:81))
CIN <- CIN |> select(-n_games)

CLE <- df2021 |> filter(home_team_id == "CLE")
CLE <- CLE |> group_by(game_id) |>summarize(n_games=(n()/2))
CLE <- CLE |> mutate(game = c(1:81))
CLE <- CLE |> select(-n_games)

COL <- df2021 |> filter(home_team_id == "COL")
COL <- COL |> group_by(game_id) |>summarize(n_games=(n()/2))
COL <- COL |> mutate(game = c(1:81))
COL <- COL |> select(-n_games)

DET <- df2021 |> filter(home_team_id == "DET")
DET <- DET |> group_by(game_id) |>summarize(n_games=(n()/2))
DET <- DET |> mutate(game = c(1:81))
DET <- DET |> select(-n_games)

HOU <- df2021 |> filter(home_team_id == "HOU")
HOU <- HOU |> group_by(game_id) |>summarize(n_games=(n()/2))
HOU <- HOU |> mutate(game = c(1:81))
HOU <- HOU |> select(-n_games)

KCA <- df2021 |> filter(home_team_id == "KCA")
KCA <- KCA |> group_by(game_id) |>summarize(n_games=(n()/2))
KCA <- KCA |> mutate(game = c(1:81))
KCA <- KCA |> select(-n_games)

LAN <- df2021 |> filter(home_team_id == "LAN")
LAN <- LAN |> group_by(game_id) |>summarize(n_games=(n()/2))
LAN <- LAN |> mutate(game = c(1:81))
LAN <- LAN |> select(-n_games)

MIA <- df2021 |> filter(home_team_id == "MIA")
MIA <- MIA |> group_by(game_id) |>summarize(n_games=(n()/2))
MIA <- MIA |> mutate(game = c(1:81))
MIA <- MIA |> select(-n_games)

MIL <- df2021 |> filter(home_team_id == "MIL")
MIL <- MIL |> group_by(game_id) |>summarize(n_games=(n()/2))
MIL <- MIL |> mutate(game = c(1:81))
MIL <- MIL |> select(-n_games)

MIN <- df2021 |> filter(home_team_id == "MIN")
MIN <- MIN |> group_by(game_id) |>summarize(n_games=(n()/2))
MIN <- MIN |> mutate(game = c(1:81))
MIN <- MIN |> select(-n_games)

NYA <- df2021 |> filter(home_team_id == "NYA")
NYA <- NYA |> group_by(game_id) |>summarize(n_games=(n()/2))
NYA <- NYA |> mutate(game = c(1:81))
NYA <- NYA |> select(-n_games)

NYN <- df2021 |> filter(home_team_id == "NYN")
NYN <- NYN |> group_by(game_id) |>summarize(n_games=(n()/2))
NYN <- NYN |> mutate(game = c(1:81))
NYN <- NYN |> select(-n_games)

OAK <- df2021 |> filter(home_team_id == "OAK")
OAK <- OAK |> group_by(game_id) |>summarize(n_games=(n()/2))
OAK <- OAK |> mutate(game = c(1:81))
OAK <- OAK |> select(-n_games)

PHI <- df2021 |> filter(home_team_id == "PHI")
PHI <- PHI |> group_by(game_id) |>summarize(n_games=(n()/2))
PHI <- PHI |> mutate(game = c(1:81))
PHI <- PHI |> select(-n_games)

PIT <- df2021 |> filter(home_team_id == "PIT")
PIT <- PIT |> group_by(game_id) |>summarize(n_games=(n()/2))
PIT <- PIT |> mutate(game = c(1:81))
PIT <- PIT |> select(-n_games)

SDN <- df2021 |> filter(home_team_id == "SDN")
SDN <- SDN |> group_by(game_id) |>summarize(n_games=(n()/2))
SDN <- SDN |> mutate(game = c(1:81))
SDN <- SDN |> select(-n_games)

SEA <- df2021 |> filter(home_team_id == "SEA")
SEA <- SEA |> group_by(game_id) |>summarize(n_games=(n()/2))
SEA <- SEA |> mutate(game = c(1:81))
SEA <- SEA |> select(-n_games)

SFN <- df2021 |> filter(home_team_id == "SFN")
SFN <- SFN |> group_by(game_id) |>summarize(n_games=(n()/2))
SFN <- SFN |> mutate(game = c(1:81))
SFN <- SFN |> select(-n_games)

SLN <- df2021 |> filter(home_team_id == "SLN")
SLN <- SLN |> group_by(game_id) |>summarize(n_games=(n()/2))
SLN <- SLN |> mutate(game = c(1:81))
SLN <- SLN |> select(-n_games)

TBA <- df2021 |> filter(home_team_id == "TBA")
TBA <- TBA |> group_by(game_id) |>summarize(n_games=(n()/2))
TBA <- TBA |> mutate(game = c(1:81))
TBA <- TBA |> select(-n_games)

TEX <- df2021 |> filter(home_team_id == "TEX")
TEX <- TEX |> group_by(game_id) |>summarize(n_games=(n()/2))
TEX <- TEX |> mutate(game = c(1:81))
TEX <- TEX |> select(-n_games)

TOR <- df2021 |> filter(home_team_id == "TOR")
TOR <- TOR |> group_by(game_id) |>summarize(n_games=(n()/2))
TOR <- TOR |> mutate(game = c(1:80))
TOR <- TOR |> select(-n_games)

WAS <- df2021 |> filter(home_team_id == "WAS")
WAS <- WAS |> group_by(game_id) |>summarize(n_games=(n()/2))
WAS <- WAS |> mutate(game = c(1:81))
WAS <- WAS |> select(-n_games)

team2021 <- bind_rows(ana, ARI, ATL, BAL, BOS, CHA, CHN, CIN, CLE, COL, DET, 
                      HOU, KCA, LAN, MIA, MIL, MIN, NYA, NYN, OAK, PHI, PIT,
                      SDN, SEA, SFN, SLN, TBA, TEX, TOR, WAS)

df2021 <- merge(df2021, team2021, by = "game_id")
df2021 <- df2021 |> unique()


#2022
ana <- df2022 |> filter(home_team_id == "ANA")
ana <- ana |> group_by(game_id) |>summarize(n_games=(n()/2))
ana <- ana |> mutate(game = c(1:81))
ana <- ana |> select(-n_games)

ARI <- df2022 |> filter(home_team_id == "ANA")
ARI <- ARI |> group_by(game_id) |>summarize(n_games=(n()/2))
ARI <- ARI |> mutate(game = c(1:81))
ARI <- ARI |> select(-n_games)

ATL <- df2022 |> filter(home_team_id == "ATL")
ATL <- ATL |> group_by(game_id) |>summarize(n_games=(n()/2))
ATL <- ATL |> mutate(game = c(1:81))
ATL <- ATL |> select(-n_games)

BAL <- df2022 |> filter(home_team_id == "BAL")
BAL <- BAL |> group_by(game_id) |>summarize(n_games=(n()/2))
BAL <- BAL |> mutate(game = c(1:81))
BAL <- BAL |> select(-n_games)

BOS <- df2022 |> filter(home_team_id == "BOS")
BOS <- BOS |> group_by(game_id) |>summarize(n_games=(n()/2))
BOS <- BOS |> mutate(game = c(1:81))
BOS <- BOS |> select(-n_games)

CHA <- df2022 |> filter(home_team_id == "CHA")
CHA <- CHA |> group_by(game_id) |>summarize(n_games=(n()/2))
CHA <- CHA |> mutate(game = c(1:81))
CHA <- CHA |> select(-n_games)

CHN <- df2022 |> filter(home_team_id == "CHN")
CHN <- CHN |> group_by(game_id) |>summarize(n_games=(n()/2))
CHN <- CHN |> mutate(game = c(1:81))
CHN <- CHN |> select(-n_games)

CIN <- df2022 |> filter(home_team_id == "CIN")
CIN <- CIN |> group_by(game_id) |>summarize(n_games=(n()/2))
CIN <- CIN |> mutate(game = c(1:81))
CIN <- CIN |> select(-n_games)

CLE <- df2022 |> filter(home_team_id == "CLE")
CLE <- CLE |> group_by(game_id) |>summarize(n_games=(n()/2))
CLE <- CLE |> mutate(game = c(1:81))
CLE <- CLE |> select(-n_games)

COL <- df2022 |> filter(home_team_id == "COL")
COL <- COL |> group_by(game_id) |>summarize(n_games=(n()/2))
COL <- COL |> mutate(game = c(1:81))
COL <- COL |> select(-n_games)

DET <- df2022 |> filter(home_team_id == "DET")
DET <- DET |> group_by(game_id) |>summarize(n_games=(n()/2))
DET <- DET |> mutate(game = c(1:82))
DET <- DET |> select(-n_games)

HOU <- df2022 |> filter(home_team_id == "HOU")
HOU <- HOU |> group_by(game_id) |>summarize(n_games=(n()/2))
HOU <- HOU |> mutate(game = c(1:81))
HOU <- HOU |> select(-n_games)

KCA <- df2022 |> filter(home_team_id == "KCA")
KCA <- KCA |> group_by(game_id) |>summarize(n_games=(n()/2))
KCA <- KCA |> mutate(game = c(1:81))
KCA <- KCA |> select(-n_games)

LAN <- df2022 |> filter(home_team_id == "LAN")
LAN <- LAN |> group_by(game_id) |>summarize(n_games=(n()/2))
LAN <- LAN |> mutate(game = c(1:81))
LAN <- LAN |> select(-n_games)

MIA <- df2022 |> filter(home_team_id == "MIA")
MIA <- MIA |> group_by(game_id) |>summarize(n_games=(n()/2))
MIA <- MIA |> mutate(game = c(1:81))
MIA <- MIA |> select(-n_games)

MIL <- df2022 |> filter(home_team_id == "MIL")
MIL <- MIL |> group_by(game_id) |>summarize(n_games=(n()/2))
MIL <- MIL |> mutate(game = c(1:81))
MIL <- MIL |> select(-n_games)

MIN <- df2022 |> filter(home_team_id == "MIN")
MIN <- MIN |> group_by(game_id) |>summarize(n_games=(n()/2))
MIN <- MIN |> mutate(game = c(1:81))
MIN <- MIN |> select(-n_games)

NYA <- df2022 |> filter(home_team_id == "NYA")
NYA <- NYA |> group_by(game_id) |>summarize(n_games=(n()/2))
NYA <- NYA |> mutate(game = c(1:81))
NYA <- NYA |> select(-n_games)

NYN <- df2022 |> filter(home_team_id == "NYN")
NYN <- NYN |> group_by(game_id) |>summarize(n_games=(n()/2))
NYN <- NYN |> mutate(game = c(1:81))
NYN <- NYN |> select(-n_games)

OAK <- df2022 |> filter(home_team_id == "OAK")
OAK <- OAK |> group_by(game_id) |>summarize(n_games=(n()/2))
OAK <- OAK |> mutate(game = c(1:80))
OAK <- OAK |> select(-n_games)

PHI <- df2022 |> filter(home_team_id == "PHI")
PHI <- PHI |> group_by(game_id) |>summarize(n_games=(n()/2))
PHI <- PHI |> mutate(game = c(1:81))
PHI <- PHI |> select(-n_games)

PIT <- df2022 |> filter(home_team_id == "PIT")
PIT <- PIT |> group_by(game_id) |>summarize(n_games=(n()/2))
PIT <- PIT |> mutate(game = c(1:81))
PIT <- PIT |> select(-n_games)

SDN <- df2022 |> filter(home_team_id == "SDN")
SDN <- SDN |> group_by(game_id) |>summarize(n_games=(n()/2))
SDN <- SDN |> mutate(game = c(1:81))
SDN <- SDN |> select(-n_games)

SEA <- df2022 |> filter(home_team_id == "SEA")
SEA <- SEA |> group_by(game_id) |>summarize(n_games=(n()/2))
SEA <- SEA |> mutate(game = c(1:81))
SEA <- SEA |> select(-n_games)

SFN <- df2022 |> filter(home_team_id == "SFN")
SFN <- SFN |> group_by(game_id) |>summarize(n_games=(n()/2))
SFN <- SFN |> mutate(game = c(1:81))
SFN <- SFN |> select(-n_games)

SLN <- df2022 |> filter(home_team_id == "SLN")
SLN <- SLN |> group_by(game_id) |>summarize(n_games=(n()/2))
SLN <- SLN |> mutate(game = c(1:81))
SLN <- SLN |> select(-n_games)

TBA <- df2022 |> filter(home_team_id == "TBA")
TBA <- TBA |> group_by(game_id) |>summarize(n_games=(n()/2))
TBA <- TBA |> mutate(game = c(1:81))
TBA <- TBA |> select(-n_games)

TEX <- df2022 |> filter(home_team_id == "TEX")
TEX <- TEX |> group_by(game_id) |>summarize(n_games=(n()/2))
TEX <- TEX |> mutate(game = c(1:81))
TEX <- TEX |> select(-n_games)

TOR <- df2022 |> filter(home_team_id == "TOR")
TOR <- TOR |> group_by(game_id) |>summarize(n_games=(n()/2))
TOR <- TOR |> mutate(game = c(1:81))
TOR <- TOR |> select(-n_games)

WAS <- df2022 |> filter(home_team_id == "WAS")
WAS <- WAS |> group_by(game_id) |>summarize(n_games=(n()/2))
WAS <- WAS |> mutate(game = c(1:81))
WAS <- WAS |> select(-n_games)

team2022 <- bind_rows(ana, ARI, ATL, BAL, BOS, CHA, CHN, CIN, CLE, COL, DET, 
                      HOU, KCA, LAN, MIA, MIL, MIN, NYA, NYN, OAK, PHI, PIT,
                      SDN, SEA, SFN, SLN, TBA, TEX, TOR, WAS)

df2022 <- merge(df2022, team2022, by = "game_id")
df2022 <- df2022 |>unique()


#2023
ana <- df2023 |> filter(home_team_id == "ANA")
ana <- ana |> group_by(game_id) |>summarize(n_games=(n()/2))
ana <- ana |> mutate(game = c(1:81))
ana <- ana |> select(-n_games)

ARI <- df2023 |> filter(home_team_id == "ANA")
ARI <- ARI |> group_by(game_id) |>summarize(n_games=(n()/2))
ARI <- ARI |> mutate(game = c(1:81))
ARI <- ARI |> select(-n_games)

ATL <- df2023 |> filter(home_team_id == "ATL")
ATL <- ATL |> group_by(game_id) |>summarize(n_games=(n()/2))
ATL <- ATL |> mutate(game = c(1:81))
ATL <- ATL |> select(-n_games)

BAL <- df2023 |> filter(home_team_id == "BAL")
BAL <- BAL |> group_by(game_id) |>summarize(n_games=(n()/2))
BAL <- BAL |> mutate(game = c(1:81))
BAL <- BAL |> select(-n_games)

BOS <- df2023 |> filter(home_team_id == "BOS")
BOS <- BOS |> group_by(game_id) |>summarize(n_games=(n()/2))
BOS <- BOS |> mutate(game = c(1:81))
BOS <- BOS |> select(-n_games)

CHA <- df2023 |> filter(home_team_id == "CHA")
CHA <- CHA |> group_by(game_id) |>summarize(n_games=(n()/2))
CHA <- CHA |> mutate(game = c(1:81))
CHA <- CHA |> select(-n_games)

CHN <- df2023 |> filter(home_team_id == "CHN")
CHN <- CHN |> group_by(game_id) |>summarize(n_games=(n()/2))
CHN <- CHN |> mutate(game = c(1:81))
CHN <- CHN |> select(-n_games)

CIN <- df2023 |> filter(home_team_id == "CIN")
CIN <- CIN |> group_by(game_id) |>summarize(n_games=(n()/2))
CIN <- CIN |> mutate(game = c(1:81))
CIN <- CIN |> select(-n_games)

CLE <- df2023 |> filter(home_team_id == "CLE")
CLE <- CLE |> group_by(game_id) |>summarize(n_games=(n()/2))
CLE <- CLE |> mutate(game = c(1:81))
CLE <- CLE |> select(-n_games)

COL <- df2023 |> filter(home_team_id == "COL")
COL <- COL |> group_by(game_id) |>summarize(n_games=(n()/2))
COL <- COL |> mutate(game = c(1:81))
COL <- COL |> select(-n_games)

DET <- df2023 |> filter(home_team_id == "DET")
DET <- DET |> group_by(game_id) |>summarize(n_games=(n()/2))
DET <- DET |> mutate(game = c(1:81))
DET <- DET |> select(-n_games)

HOU <- df2023 |> filter(home_team_id == "HOU")
HOU <- HOU |> group_by(game_id) |>summarize(n_games=(n()/2))
HOU <- HOU |> mutate(game = c(1:81))
HOU <- HOU |> select(-n_games)

KCA <- df2023 |> filter(home_team_id == "KCA")
KCA <- KCA |> group_by(game_id) |>summarize(n_games=(n()/2))
KCA <- KCA |> mutate(game = c(1:81))
KCA <- KCA |> select(-n_games)

LAN <- df2023 |> filter(home_team_id == "LAN")
LAN <- LAN |> group_by(game_id) |>summarize(n_games=(n()/2))
LAN <- LAN |> mutate(game = c(1:81))
LAN <- LAN |> select(-n_games)

MIA <- df2023 |> filter(home_team_id == "MIA")
MIA <- MIA |> group_by(game_id) |>summarize(n_games=(n()/2))
MIA <- MIA |> mutate(game = c(1:81))
MIA <- MIA |> select(-n_games)

MIL <- df2023 |> filter(home_team_id == "MIL")
MIL <- MIL |> group_by(game_id) |>summarize(n_games=(n()/2))
MIL <- MIL |> mutate(game = c(1:81))
MIL <- MIL |> select(-n_games)

MIN <- df2023 |> filter(home_team_id == "MIN")
MIN <- MIN |> group_by(game_id) |>summarize(n_games=(n()/2))
MIN <- MIN |> mutate(game = c(1:81))
MIN <- MIN |> select(-n_games)

NYA <- df2023 |> filter(home_team_id == "NYA")
NYA <- NYA |> group_by(game_id) |>summarize(n_games=(n()/2))
NYA <- NYA |> mutate(game = c(1:81))
NYA <- NYA |> select(-n_games)

NYN <- df2023 |> filter(home_team_id == "NYN")
NYN <- NYN |> group_by(game_id) |>summarize(n_games=(n()/2))
NYN <- NYN |> mutate(game = c(1:81))
NYN <- NYN |> select(-n_games)

OAK <- df2023 |> filter(home_team_id == "OAK")
OAK <- OAK |> group_by(game_id) |>summarize(n_games=(n()/2))
OAK <- OAK |> mutate(game = c(1:81))
OAK <- OAK |> select(-n_games)

PHI <- df2023 |> filter(home_team_id == "PHI")
PHI <- PHI |> group_by(game_id) |>summarize(n_games=(n()/2))
PHI <- PHI |> mutate(game = c(1:81))
PHI <- PHI |> select(-n_games)

PIT <- df2023 |> filter(home_team_id == "PIT")
PIT <- PIT |> group_by(game_id) |>summarize(n_games=(n()/2))
PIT <- PIT |> mutate(game = c(1:81))
PIT <- PIT |> select(-n_games)

SDN <- df2023 |> filter(home_team_id == "SDN")
SDN <- SDN |> group_by(game_id) |>summarize(n_games=(n()/2))
SDN <- SDN |> mutate(game = c(1:81))
SDN <- SDN |> select(-n_games)

SEA <- df2023 |> filter(home_team_id == "SEA")
SEA <- SEA |> group_by(game_id) |>summarize(n_games=(n()/2))
SEA <- SEA |> mutate(game = c(1:81))
SEA <- SEA |> select(-n_games)

SFN <- df2023 |> filter(home_team_id == "SFN")
SFN <- SFN |> group_by(game_id) |>summarize(n_games=(n()/2))
SFN <- SFN |> mutate(game = c(1:81))
SFN <- SFN |> select(-n_games)

SLN <- df2023 |> filter(home_team_id == "SLN")
SLN <- SLN |> group_by(game_id) |>summarize(n_games=(n()/2))
SLN <- SLN |> mutate(game = c(1:81))
SLN <- SLN |> select(-n_games)

TBA <- df2023 |> filter(home_team_id == "TBA")
TBA <- TBA |> group_by(game_id) |>summarize(n_games=(n()/2))
TBA <- TBA |> mutate(game = c(1:81))
TBA <- TBA |> select(-n_games)

TEX <- df2023 |> filter(home_team_id == "TEX")
TEX <- TEX |> group_by(game_id) |>summarize(n_games=(n()/2))
TEX <- TEX |> mutate(game = c(1:81))
TEX <- TEX |> select(-n_games)

TOR <- df2023 |> filter(home_team_id == "TOR")
TOR <- TOR |> group_by(game_id) |>summarize(n_games=(n()/2))
TOR <- TOR |> mutate(game = c(1:81))
TOR <- TOR |> select(-n_games)

WAS <- df2023 |> filter(home_team_id == "WAS")
WAS <- WAS |> group_by(game_id) |>summarize(n_games=(n()/2))
WAS <- WAS |> mutate(game = c(1:81))
WAS <- WAS |> select(-n_games)

team2023 <- bind_rows(ana, ARI, ATL, BAL, BOS, CHA, CHN, CIN, CLE, COL, DET, 
                      HOU, KCA, LAN, MIA, MIL, MIN, NYA, NYN, OAK, PHI, PIT,
                      SDN, SEA, SFN, SLN, TBA, TEX, TOR, WAS)

df2023 <- merge(df2023, team2023, by = "game_id")
df2023 <- df2023 |> unique()


#bring together all dataframes
total_k <- rbind(df2020, df2021, df2022, df2023)

#creating a dataframe that pulls out only plays in which an out was recorded
#summarize gathers all the outs recorded by a pitcher in a game
out_df <- total_k |>
  group_by(season, game_id, game, pitcher_id, home_pitcher_id, away_pitcher_id) |>
  summarize(outs = sum(outs_on_play))

#Filters out only PA that ended in a strikeout
#Then, sums the number of strikeouts recorded in a game
sumk_df <- total_k |>
  filter(event_seq == "K") |>
  group_by(season, game_id, pitcher_id, home_pitcher_id, away_pitcher_id) |>
  summarize(sum_k = n())

#Reduces the data frame to only include starting pitchers
sumk_df <- sumk_df |>
  filter(pitcher_id == away_pitcher_id | pitcher_id == home_pitcher_id)

#Reduces the data frame to only include starting pitchers
out_df <- out_df |>
  filter(pitcher_id == away_pitcher_id | pitcher_id == home_pitcher_id)


#create data frame that pulls out the game environment information and pitchers
df <- total_k |>
  select(season, game_id, game, stadium, home_team_id, away_team_id, 
           temp, day_of_week, event_seq, start_time, outs_on_play, day_night, hp_ump_id, wind_direction, 
           wind_speed, precipitation, sky, home_pitcher_id, away_pitcher_id, pitcher_id, pitcher_hand)

#Pulls out Strikeouts only, starting pitchers only
df <- df |>
  filter(event_seq == "K",
         pitcher_id == away_pitcher_id | pitcher_id == home_pitcher_id)

#adding total outs and strikeouts by pitcher to create master dataframe
k_df <- merge(df, out_df, by = c('season', 'game_id', 'pitcher_id', 'home_pitcher_id',
                                      'away_pitcher_id'))

k_df <- merge(k_df, sumk_df, by = c('season', 'game_id', 'pitcher_id', 'home_pitcher_id',
                                       'away_pitcher_id'))

#remove duplicates
k_df <- unique(k_df)

#make only one game column
k_df <- k_df |> select(-game.y)
k_df <- k_df |> rename(game = game.x)

#splitting game_id, then creating a game_id for away teams
k_df$team = str_sub(k_df$game_id, 1, 3)
k_df$id = str_sub(k_df$game_id, 4)

#changing team to away team id when necessary
k_df$team <- ifelse(k_df$pitcher_id == k_df$away_pitcher_id, 
       k_df$away_team_id,
       k_df$home_team_id)

#change game_id
k_df$game_id <- paste(k_df$team, k_df$id)
k_df$game_id <- str_replace_all(k_df$game_id, fixed(" "), "")
k_df <- k_df |> select(-team, -id)

#A Test.
#group by sum_k to get number of occurrences of strikeouts pitched
k_df |>
  group_by(sum_k) |>
  summarize(n=n())

#show mean strikeouts is 4.786
k_df |> summary()

###
###
#A Test. What do strikeout predictions look like in cold weather
#Cold Weather model (32-50 Degrees F)
cold_k <- k_df |>
  filter(temp <=50)

cold_k <- cold_k[order(cold_k$game_id), ]

cold_k |>
  summary()

#pull the mean of Ks per appearance
mean_k_cold <-
  cold_k |>
  pull(sum_k) |>
  mean()

#poisson value for the expected number of strikeouts for a starter
plot_pos_cold <-
  tibble(x=seq(0,14)) |>
  mutate(expected = dpois(
    x=x,
    lambda = mean_k_cold
  ))

#plotting the strikeouts histogram with a Poisson Distribution
#Distribution expects there to be fewer 1-3 strikeout performances and
#more 4,5,7 performances
ggplot() +
  geom_histogram(
    data= cold_k, aes(sum_k, after_stat(count/ sum(count))),
    binwidth = .5) +
  geom_line(data = plot_pos_cold, aes(x=x, y=expected),
            color = "red", linewidth =1) +
  theme_bw() +
  xlab("Streikeouts per player per cold game for 2020-2023") +
  ylab("Probability")

#create small cold weather DF to use in for loop
cold_k2 <- cold_k |> select(season, game_id, pitcher_id, sum_k, game)

#get k_rate per game
x_cold <- tibble()

#for loop updates the strikeout rate for every pitcher for every game they pitch
#from 2020-2023
#2020 no cold games were played
for(season_x in seq(2020, 2023)) {
  for (game_x in seq(1, 162)) {
    game_calc <- 
      cold_k |>
      filter((season == (season_x-1)) | 
               (season == season_x & game < game_x)) |>
      group_by(pitcher_id) |>
      summarize(
        k_rate = mean(sum_k),
        .groups = "keep"
      ) |>
      mutate(season = season_x, game=game_x)
    
    x_cold <- bind_rows(x_cold, game_calc)
  }
}


#tibble for only Charlie Morton as example
#he only pitched two games that fell under the cold weather classification
mort <- x_cold |>
  filter(pitcher_id == "mortc002")

#Join tibble to cold DF to create response variable
#creates a weird DF but it works moving forward
cold_k <-
  cold_k |>
  inner_join(x_cold, by = c('pitcher_id', 'season', 'game'))

#create game-by-game Ks plot by player
per_game_strikeout_plot_cold <-
  cold_k |>
  ggplot(aes(game, sum_k, group=pitcher_id)) +
  geom_line(alpha=.25) +
  facet_wrap(vars(season), nrow = 3) +
  theme_bw() +
  ylab("Total Strikeouts") +
  xlab("Game of Season")

per_game_strikeout_plot_cold

#adding poisson line to plot
per_game_strikeout_plot_cold + 
  geom_smooth(method = 'glm', method.args = list("family" = "poisson"),
              se=FALSE,
              linewidth = .5, color = 'blue',
              alpha=.25)

#Fitting poisson model to the 2020-2023 data
#strikeout rate influenced by K rate and temperature
cold_k_fit <-
  glm(sum_k ~ k_rate + temp,
      data = cold_k,
      family = "poisson")

#save outputs to a new column called Exp_K
cold_k <-
  cold_k |>
  ungroup() |>
  mutate(exp_k = predict(cold_k_fit, type = 'response'))

summary(cold_k_fit) |>
  print()

#generate the statistics of the model
tidy(cold_k_fit, exponentiate= TRUE, conf.int = TRUE)

#creating a column that shows the probability of getting less than 4 or
#more than 4 strikeouts
cold_k <-
  cold_k |>
  mutate(
    p_l4_k = ppois(q=4,
                   lambda = exp_k,
                   lower.tail=TRUE),
    p_g4_k = ppois(q=4,
                   lambda = exp_k,
                   lower.tail=FALSE)
  )

#testing to predict the probability of Jon Gray at 4.5 strikeouts
cold_gray <-
cold_k |>
  filter(pitcher_id == "grayj003") |>
  select(pitcher_id, k_rate, exp_k, p_l4_k, p_g4_k)

cold_gray <- unique(cold_gray)

#all model, everything below is same as above
all_k <- k_df

all_k <- all_k[order(all_k$game_id), ]
all_k |> summary()


#plotting the strikeouts histogram in a Poisson Dist
mean_k <-
  all_k |>
  pull(sum_k) |>
  mean()

plot_pos_all <-
  tibble(x=seq(0,16)) |>
  mutate(expected = dpois(
    x=x,
    lambda = mean_k
  ))

ggplot() +
  geom_histogram(
    data= all_k, aes(sum_k, after_stat(count/ sum(count))),
    binwidth = .5) +
  geom_line(data = plot_pos_cold, aes(x=x, y=expected),
            color = "red", linewidth =1) +
  theme_bw() +
  xlab("Streikeouts per player per game for 2020-2023") +
  ylab("Probability")

#create small cold DF for for loop
all_k2 <- all_k |> select(season, game_id, pitcher_id, sum_k, game)


#get k_rate per game
x_all <- tibble()

for(season_x in seq(2020, 2023)) {
  for (game_x in seq(1, 162)) {
    game_calc <- 
      all_k |>
      filter((season == (season_x-1)) | 
               (season == season_x & game < game_x)) |>
      group_by(pitcher_id) |>
      summarize(
        n=n(),
        k_rate = mean(sum_k),
        .groups = "keep"
      ) |>
      mutate(season = season_x, game=game_x)
    
    x_all <- bind_rows(x_all, game_calc)
  }
}


#tibble for only Charlie Morton as example
mort_all <- x_all |>
  filter(pitcher_id == "mortc002") |>
  tail()

#Join to create response variable
all_k <- 
  all_k |>
  inner_join(x_all,
             by = c("season", "pitcher_id", "game"))


#create game-by-game Ks plot by player
per_game_strikeout_plot_all <-
  all_k |>
  ggplot(aes(game, sum_k, group=pitcher_id)) +
  geom_line(alpha=.25) +
  facet_wrap(vars(season), nrow = 3) +
  theme_bw() +
  ylab("Total Strikeouts") +
  xlab("Game of Season")


#adding poisson line to plot
per_game_strikeout_plot_all + 
  geom_smooth(method = 'glm', method.args = list("family" = "poisson"),
              se=FALSE,
              linewidth = .5, color = 'blue',
              alpha=.25)

#Fitting the model to the 2020-2023 data
all_k_fit <-
  glm(sum_k ~ k_rate + temp + stadium + outs + wind_speed + wind_direction,
      data = all_k,
      family = "poisson")

all_k_fit2 <-  
  glm(sum_k ~ k_rate + temp + stadium + outs + wind_speed, 
                    data = all_k,
                    family = "poisson")
#save outputs to a new column called Exp_K
all_k <-
  all_k |>
  ungroup() |>
  mutate(exp_k = predict(all_k_fit, type = 'response'))

summary(all_k_fit) |>
  print()

tidy(all_k_fit, exponentiate= TRUE, conf.int = TRUE)

#Adding probability distributions to the dataframe
all_k <-
  all_k |>
  mutate(
    p_l2_k = ppois(q=2,
                   lambda = exp_k,
                   lower.tail=TRUE),
    p_g2_k = ppois(q=2,
                   lambda = exp_k,
                   lower.tail=FALSE)
  )

all_k <-
  all_k |>
  mutate(
    p_l3_k = ppois(q=3,
                   lambda = exp_k,
                   lower.tail=TRUE),
    p_g3_k = ppois(q=3,
                   lambda = exp_k,
                   lower.tail=FALSE)
  )

all_k <-
  all_k |>
  mutate(
    p_l4_k = ppois(q=4,
                   lambda = exp_k,
                   lower.tail=TRUE),
    p_g4_k = ppois(q=4,
                   lambda = exp_k,
                   lower.tail=FALSE)
  )

all_k <-
  all_k |>
  mutate(
    p_l5_k = ppois(q=5,
                   lambda = exp_k,
                   lower.tail=TRUE),
    p_g5_k = ppois(q=5,
                   lambda = exp_k,
                   lower.tail=FALSE)
  )

all_k <-
  all_k |>
  mutate(
    p_l6_k = ppois(q=6,
                   lambda = exp_k,
                   lower.tail=TRUE),
    p_g6_k = ppois(q=6,
                   lambda = exp_k,
                   lower.tail=FALSE)
  )

all_k <-
  all_k |>
  mutate(
    p_l7_k = ppois(q=7,
                   lambda = exp_k,
                   lower.tail=TRUE),
    p_g7_k = ppois(q=7,
                   lambda = exp_k,
                   lower.tail=FALSE)
  )

all_k <-
  all_k |>
  mutate(
    p_l8_k = ppois(q=8,
                   lambda = exp_k,
                   lower.tail=TRUE),
    p_g8_k = ppois(q=8,
                   lambda = exp_k,
                   lower.tail=FALSE)
  )

#different pitcher examples
all_gray <-
  all_k |>
  filter(pitcher_id == "grayj003") |>
  select(pitcher_id, sum_k, k_rate, exp_k, p_l4_k, p_g4_k) |>
  tail()

quintana <- all_k |>
  filter(pitcher_id == "quinj001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l4_k, p_g4_k) |>
  tail()

peralta <- all_k |>
  filter(pitcher_id == "peraf001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l6_k, p_g6_k) |>
  tail()

civale <- all_k |>
  filter(pitcher_id == "civaa001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l4_k, p_g4_k) |>
  tail()


bassitt <- all_k |>
  filter(pitcher_id == "bassc001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l5_k, p_g5_k) |>
  tail()

rodon <- all_k |>
  filter(pitcher_id == "rodoc001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l4_k, p_g4_k) |>
  tail()

javier <- all_k |>
  filter(pitcher_id == "javic001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l5_k, p_g5_k) |>
  tail()

musgrove <- all_k |>
  filter(pitcher_id == "musgj001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l5_k, p_g5_k) |>
  tail()

harrison <- all_k |>
  filter(pitcher_id == "harrk001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l4_k, p_g4_k) |>
  tail()

kelly <- all_k |>
  filter(pitcher_id == "kellm003") |>
  select(pitcher_id, game, k_rate, exp_k, p_l6_k, p_g6_k) |>
  tail()

quantrill <- all_k |>
  filter(pitcher_id == "quanc001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l2_k, p_g2_k) |>
  tail()

pivetta <- all_k |>
  filter(pitcher_id == "piven001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l6_k, p_g6_k) |>
  tail()

kirby <- all_k |>
  filter(pitcher_id == "kirbg001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l6_k, p_g6_k) |>
  tail()

allen <- all_k |>
  filter(pitcher_id == "allel003") |>
  select(pitcher_id, game, k_rate, exp_k, p_l5_k, p_g5_k) |>
  tail()

stripling <- all_k |>
  filter(pitcher_id == "strir001") |>
  select(pitcher_id, game, k_rate, exp_k, p_l3_k, p_g3_k) |>
  tail()

miller <- all_k |>
  filter(pitcher_id == "millb004") |>
  select(pitcher_id, game, k_rate, exp_k, p_l5_k, p_g5_k) |>
  tail()

thompson <- all_k |>
  filter(pitcher_id == "thomz002") |>
  select(pitcher_id, game, k_rate, exp_k, p_l4_k, p_g4_k) |>
  tail()


###Future versions can add more variables to look at

#I'm still working out how to solve the issue with pitcher outings having 
#the game numbers be a little weird. There used to be an issue because
#the game_id had repeat game_id AND game number. But, there should be very 
#little error because the for loop uses num < index, so repeat game numbers
#are rarely an issue because the game number is rarely larger than 
#the index