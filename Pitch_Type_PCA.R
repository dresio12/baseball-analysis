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


#loading in the pitch result statistics by player for all pitches
fastball <- read_excel("fastball.xlsx")
slider <- read_excel("slider.xlsx")
screwball <- read_excel("screw.xlsx")
sinker <- read_excel("sinker.xlsx")
curveball <- read_excel("curveball.xlsx")
sweeper <- read_excel("sweeper.xlsx")
slurve <- read_excel("slurve.xlsx")
cutter <- read_excel("cutter.xlsx")
changeup <- read_excel("changeup.xlsx")
split <- read_excel("split.xlsx")

#combines all the pitch results for all pitches
pitch_arsenal_stats <- rbind(fastball, changeup, slider, screwball, sinker,
                             curveball, sweeper, slurve, cutter, split)

#loads player spin rates and movement profiles for each pitcher's pitches
active_spin <- read_excel("active-spin.xlsx")
pitch_movement <- read_excel("pitch_movement.xlsx")

#make a common column name to join on
colnames(pitch_arsenal_stats)[1] <- c("name")
colnames(active_spin)[1] <- c("name")
colnames(pitch_movement)[2] <- c("name")

#df that combines movement profile with pitch results
pitchers <- left_join(pitch_arsenal_stats, pitch_movement)
  
#ignoring for now, adds spin profile
#pitchers <- left_join(pitchers, active_spin, by = 'name')

#drop unnecesary extra columns
colnames(pitchers)[3] <- c("team")
pitchers <- pitchers |> select(-pitcher_id, -team_name_abbrev, -pitch_type_name,
                               -pitches_thrown, -pitch_per, -pitches_per_game, 
                               -team_name)

#drop NAs
pitchers <- na.omit(pitchers)

#alter column order
pitchers <- pitchers |> relocate(year, .before = name)
pitchers <- pitchers |> relocate(pitch_hand, .after = name)

#starting Principal Component Analysis (PCA)

#selecting all columns in this range
scaled_pitchers_knn <-
  scale(pitchers |> select(run_value_per_100:percent_rank_diff_x))

#remove NAs
scaled_pitchers_knn <- na.omit(scaled_pitchers_knn)

#creates the PCA fit
pca_fit <-
  prcomp(scaled_pitchers_knn)

print(pca_fit$rotation)

print(pca_fit$sdev^2)

#getting the variance
pca_var <- pca_fit$sdev^2

#getting the percent of variance that each variable is "responsible" for
pca_pct <- round(pca_var/ sum(pca_var) * 100, 2)
print(pca_pct)

#adds the principal component values to each pitcher's pitches
pitchers <- pitchers |>
  bind_cols(pca_fit$x)

#creating a plot to visualize PC1 against PC2
#doesn't show much
ggplot(pitchers, aes(x = PC1, y = PC2)) +
  geom_point() +
  theme_bw() +
  xlab(paste0("PC1 = ", pca_pct[1], "%")) +
  ylab(paste0("PC2 = ", pca_pct[2], "%"))

#adds a color scale based on PC3
#still doesn't show much
ggplot(pitchers, aes(x = PC1, y = PC2, color = PC3)) +
  geom_point() +
  theme_bw() +
  xlab(paste0("PC1 = ", pca_pct[1], "%")) +
  ylab(paste0("PC2 = ", pca_pct[2], "%")) +
  scale_color_continuous(
    paste0("PC3 = ", pca_pct[3], "%"),
    low = "red", high = "black")


#Using pitch name as the data point color, we clearly see grouping by pitch type
color_count <- length(unique(pitchers$pitch_name))
get_palette <- colorRampPalette(brewer.pal(9, "Set1"))

ggplot(pitchers, aes(x = PC1, y = PC2, color = pitch_name)) +
  geom_point() +
  theme_bw() +
  xlab(paste0("PC1 = ", pca_pct[1], "%")) +
  ylab(paste0("PC2 = ", pca_pct[2], "%")) +
  scale_color_manual("Pitch type", values = get_palette(color_count))

#####
#attempting to cluster pitch types together based on the principal components

k_mean_fit <- kmeans(pitchers |> select(PC1, PC2), 
                     centers = 20, iter.max = 10)

k_mean_fit2 <- kmeans(pitchers |> select(PC1, PC2, PC3, PC4, PC5), 
                      centers = 7, iter.max = 10)

#creating a seaparate df for cluster analysis #2
pitchers2 <- pitchers


#######
#CLUSTER ANALYSIS ATTEMPT ONE USING FIRST TWO PCs
#adding the cluster number to each pitch
pitchers <-
  pitchers |> 
  mutate(cluster = k_mean_fit$cluster)

#testing to make sure the clusters were added properly
pitchers |>
  select(name, pitch_name, avg_speed, cluster) |>
  head()

#testing
#pulling out all pitches in cluster 1, calculating mean speed
pitchers_cluster <- pitchers |>
  filter(cluster == 1) |>
  group_by(pitch_name) |>
  summarize(n=n(),
            avg_speed = mean(avg_speed)) |>
  arrange(-n) |>
  print(n=Inf)

#finding the average speed of each pitch in each cluster
pitchers_cluster <-
  pitchers |>
  group_by(cluster, pitch_name) |>
  summarize(n=n(),
            avg_speed = mean(avg_speed),
            .groups = "drop") 


#shows all the clusters organized by pitch type, shows number 
#of pitch types within each cluster
pitchers_cluster |>
  ggplot(aes(x=n, y=pitch_name)) +
  geom_col(position = 'dodge') + 
  theme_bw() +
  facet_wrap(vars(cluster)) +
  theme(strip.background = element_blank()) +
  xlab("Pitch Type") + 
  ylab("Count")

#a test, average pitch speed per cluster
pitchers_cluster |>
  group_by(cluster) |>
  summarize(avg_speed = mean(avg_speed))

#moves the cluster column to after the pitch name for better viewing
pitchers <- pitchers |> relocate(cluster, .after = pitch_name)


####
####
#Cluster analysis attempt two using first FIVE PCs
#all steps same as for cluster attempt 1
pitchers2 <-
  pitchers2 |> 
  mutate(cluster = k_mean_fit2$cluster)

pitchers2 |>
  select(name, pitch_name, avg_speed, cluster) |>
  head()

pitchers_cluster2 <- pitchers2 |>
  filter(cluster == 1) |>
  group_by(pitch_name) |>
  summarize(n=n(),
            avg_speed = mean(avg_speed)) |>
  arrange(-n) |>
  print(n=Inf)

pitchers_cluster2 <-
  pitchers2 |>
  group_by(cluster, pitch_name) |>
  summarize(n=n(),
            avg_speed = mean(avg_speed),
            .groups = "drop") 

pitchers_cluster2 |>
  ggplot(aes(x=n, y=pitch_name)) +
  geom_col(position = 'dodge') + 
  theme_bw() +
  facet_wrap(vars(cluster)) +
  theme(strip.background = element_blank()) +
  xlab("Pitch Type") + 
  ylab("Count")

pitchers_cluster2 |>
  group_by(cluster) |>
  summarize(avg_speed = mean(avg_speed))

pitchers2 <- pitchers2 |> relocate(cluster, .after = pitch_name)


###attempting analysis
###analysis is using ANALYSIS 1 data
#groups creates df with the pitcher name, pitch name, and cluster only
groups1 <- pitchers |> select(name, pitch_name, cluster)

#creating different groups of pitchers based on pitch type cluster
g1 <- groups1 |> filter(pitch_name == '4-Seam Fastball' &
                         cluster == '9')
g2 <- groups1 |> filter(pitch_name == 'Slider' &
                         cluster == '4')
g1 <- left_join(g1 , g2, join_by(name))

#keeps only the pitchers that match the above conditions
g1 <- na.omit(g1)

#preliminary conclusions
#I am unsure what to make of the clusters, as I have yet to find a combination
#that matches anything similar to what Statcast does with the 
#pitcher similarities, either by speed/movement nor results.

#20 clusters is probably too small of a number since there are so many
#unique profiles and combinations.

#I tried to combine speed/movement and results, perhaps better results would
#come from doing one or the other, separately


###attempting analysis
###analysis is using ANALYSIS 2 data
#groups creates df with the pitcher name, pitch name, and cluster only
groups2 <- pitchers2 |> select(name, pitch_name, cluster)

#creating different groups of pitchers based on pitch type cluster
g1 <- groups2 |> filter(pitch_name == '4-Seam Fastball' &
                       cluster == '3')
g2 <- groups2 |> filter(pitch_name == 'Changeup' &
                         cluster == '4')
g1 <- left_join(g1 , g2, join_by(name))

#keeps only the pitchers that match the above conditions
g1 <- na.omit(g1)


#preliminary conclusions
#same as above, I'll put this on the back burner for now