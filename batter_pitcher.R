library(tidyverse)


#load dataframes
library(readxl)
all2019 <- read_excel("all2019.xlsx")
all2020 <- read_excel("all2020.xlsx")
all2021 <- read_excel("all2021.xlsx")
all2022 <- read_excel("all2022.xlsx")
all2023 <- read_excel("all2023.xlsx")
info2019 <- read_excel("info2019.xlsx")
info2020 <- read_excel("info2020.xlsx")
info2021 <- read_excel("info2021.xlsx")
info2022 <- read_excel("info2022.xlsx")
info2023 <- read_excel("info2023.xlsx")

#add game descriptions to the play by play data
df2019 <- left_join(all2019, info2019, by = "game_id")

df2020 <- left_join(all2020, info2020, by = "game_id")

df2021 <- left_join(all2021, info2021, by = "game_id")

df2022 <- left_join(all2022, info2022, by = "game_id")

df2023 <- left_join(all2023, info2023, by = "game_id")

#### Creating Game number for each team, each season
#2019

#get all games for Angels
ana <- df2019 |> filter(home_team_id == "ANA")
#total rows shows how many games played
ana <- ana |> group_by(game_id) |>summarize(n_games=(n()/2))
#adds game number of season column
ana <- ana |> mutate(game = c(1:81))
#removes unnecessary column
ana <- ana |> select(-n_games)

ARI <- df2019 |> filter(home_team_id == "ANA")
ARI <- ARI |> group_by(game_id) |>summarize(n_games=(n()/2))
ARI <- ARI |> mutate(game = c(1:81))
ARI <- ARI |> select(-n_games)

ATL <- df2019 |> filter(home_team_id == "ATL")
ATL <- ATL |> group_by(game_id) |>summarize(n_games=(n()/2))
ATL <- ATL |> mutate(game = c(1:81))
ATL <- ATL |> select(-n_games)

BAL <- df2019 |> filter(home_team_id == "BAL")
BAL <- BAL |> group_by(game_id) |>summarize(n_games=(n()/2))
BAL <- BAL |> mutate(game = c(1:81))
BAL <- BAL |> select(-n_games)

BOS <- df2019 |> filter(home_team_id == "BOS")
BOS <- BOS |> group_by(game_id) |>summarize(n_games=(n()/2))
BOS <- BOS |> mutate(game = c(1:81))
BOS <- BOS |> select(-n_games)

CHA <- df2019 |> filter(home_team_id == "CHA")
CHA <- CHA |> group_by(game_id) |>summarize(n_games=(n()/2))
CHA <- CHA |> mutate(game = c(1:80))
CHA <- CHA |> select(-n_games)

CHN <- df2019 |> filter(home_team_id == "CHN")
CHN <- CHN |> group_by(game_id) |>summarize(n_games=(n()/2))
CHN <- CHN |> mutate(game = c(1:81))
CHN <- CHN |> select(-n_games)

CIN <- df2019 |> filter(home_team_id == "CIN")
CIN <- CIN |> group_by(game_id) |>summarize(n_games=(n()/2))
CIN <- CIN |> mutate(game = c(1:81))
CIN <- CIN |> select(-n_games)

CLE <- df2019 |> filter(home_team_id == "CLE")
CLE <- CLE |> group_by(game_id) |>summarize(n_games=(n()/2))
CLE <- CLE |> mutate(game = c(1:81))
CLE <- CLE |> select(-n_games)

COL <- df2019 |> filter(home_team_id == "COL")
COL <- COL |> group_by(game_id) |>summarize(n_games=(n()/2))
COL <- COL |> mutate(game = c(1:81))
COL <- COL |> select(-n_games)

DET <- df2019 |> filter(home_team_id == "DET")
DET <- DET |> group_by(game_id) |>summarize(n_games=(n()/2))
DET <- DET |> mutate(game = c(1:81))
DET <- DET |> select(-n_games)

HOU <- df2019 |> filter(home_team_id == "HOU")
HOU <- HOU |> group_by(game_id) |>summarize(n_games=(n()/2))
HOU <- HOU |> mutate(game = c(1:81))
HOU <- HOU |> select(-n_games)

KCA <- df2019 |> filter(home_team_id == "KCA")
KCA <- KCA |> group_by(game_id) |>summarize(n_games=(n()/2))
KCA <- KCA |> mutate(game = c(1:81))
KCA <- KCA |> select(-n_games)

LAN <- df2019 |> filter(home_team_id == "LAN")
LAN <- LAN |> group_by(game_id) |>summarize(n_games=(n()/2))
LAN <- LAN |> mutate(game = c(1:81))
LAN <- LAN |> select(-n_games)

MIA <- df2019 |> filter(home_team_id == "MIA")
MIA <- MIA |> group_by(game_id) |>summarize(n_games=(n()/2))
MIA <- MIA |> mutate(game = c(1:81))
MIA <- MIA |> select(-n_games)

MIL <- df2019 |> filter(home_team_id == "MIL")
MIL <- MIL |> group_by(game_id) |>summarize(n_games=(n()/2))
MIL <- MIL |> mutate(game = c(1:81))
MIL <- MIL |> select(-n_games)

MIN <- df2019 |> filter(home_team_id == "MIN")
MIN <- MIN |> group_by(game_id) |>summarize(n_games=(n()/2))
MIN <- MIN |> mutate(game = c(1:81))
MIN <- MIN |> select(-n_games)

NYA <- df2019 |> filter(home_team_id == "NYA")
NYA <- NYA |> group_by(game_id) |>summarize(n_games=(n()/2))
NYA <- NYA |> mutate(game = c(1:81))
NYA <- NYA |> select(-n_games)

NYN <- df2019 |> filter(home_team_id == "NYN")
NYN <- NYN |> group_by(game_id) |>summarize(n_games=(n()/2))
NYN <- NYN |> mutate(game = c(1:81))
NYN <- NYN |> select(-n_games)

OAK <- df2019 |> filter(home_team_id == "OAK")
OAK <- OAK |> group_by(game_id) |>summarize(n_games=(n()/2))
OAK <- OAK |> mutate(game = c(1:81))
OAK <- OAK |> select(-n_games)

PHI <- df2019 |> filter(home_team_id == "PHI")
PHI <- PHI |> group_by(game_id) |>summarize(n_games=(n()/2))
PHI <- PHI |> mutate(game = c(1:81))
PHI <- PHI |> select(-n_games)

PIT <- df2019 |> filter(home_team_id == "PIT")
PIT <- PIT |> group_by(game_id) |>summarize(n_games=(n()/2))
PIT <- PIT |> mutate(game = c(1:81))
PIT <- PIT |> select(-n_games)

SDN <- df2019 |> filter(home_team_id == "SDN")
SDN <- SDN |> group_by(game_id) |>summarize(n_games=(n()/2))
SDN <- SDN |> mutate(game = c(1:81))
SDN <- SDN |> select(-n_games)

SEA <- df2019 |> filter(home_team_id == "SEA")
SEA <- SEA |> group_by(game_id) |>summarize(n_games=(n()/2))
SEA <- SEA |> mutate(game = c(1:81))
SEA <- SEA |> select(-n_games)

SFN <- df2019 |> filter(home_team_id == "SFN")
SFN <- SFN |> group_by(game_id) |>summarize(n_games=(n()/2))
SFN <- SFN |> mutate(game = c(1:81))
SFN <- SFN |> select(-n_games)

SLN <- df2019 |> filter(home_team_id == "SLN")
SLN <- SLN |> group_by(game_id) |>summarize(n_games=(n()/2))
SLN <- SLN |> mutate(game = c(1:81))
SLN <- SLN |> select(-n_games)

TBA <- df2019 |> filter(home_team_id == "TBA")
TBA <- TBA |> group_by(game_id) |>summarize(n_games=(n()/2))
TBA <- TBA |> mutate(game = c(1:81))
TBA <- TBA |> select(-n_games)

TEX <- df2019 |> filter(home_team_id == "TEX")
TEX <- TEX |> group_by(game_id) |>summarize(n_games=(n()/2))
TEX <- TEX |> mutate(game = c(1:81))
TEX <- TEX |> select(-n_games)

TOR <- df2019 |> filter(home_team_id == "TOR")
TOR <- TOR |> group_by(game_id) |>summarize(n_games=(n()/2))
TOR <- TOR |> mutate(game = c(1:81))
TOR <- TOR |> select(-n_games)

WAS <- df2019 |> filter(home_team_id == "WAS")
WAS <- WAS |> group_by(game_id) |>summarize(n_games=(n()/2))
WAS <- WAS |> mutate(game = c(1:81))
WAS <- WAS |> select(-n_games)

#create df that adds all game numbers and IDs
team2019 <- bind_rows(ana, ARI, ATL, BAL, BOS, CHA, CHN, CIN, CLE, COL, DET, 
                      HOU, KCA, LAN, MIA, MIL, MIN, NYA, NYN, OAK, PHI, PIT,
                      SDN, SEA, SFN, SLN, TBA, TEX, TOR, WAS)

#adds game number columns to primary df, then removes all duplicates
df2019 <- merge(df2019, team2019, by = "game_id")
df2019 <- df2019 |> unique()


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

team2020 <- bind_rows(ana, ARI, ATL, BAL, BOS, CHA, CHN, CIN, CLE, COL, DET, 
                      HOU, KCA, LAN, MIA, MIL, MIN, NYA, NYN, OAK, PHI, PIT,
                      SDN, SEA, SFN, SLN, TBA, TEX, TOR, WAS)

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

#changing visitor to away for df2023 for consistency
change <- c("Visitor")
change_to <- c("Away")

df2023$bat_team_id <- replace(df2023$bat_team_id, df2023$bat_team_id %in% change, change_to)

#changing 0s and 1s in bat_team_id to away and home
change <- c("0", "1")
change_to <- c("Away", "Home")

df2022$bat_team_id <- replace(df2022$bat_team_id, df2022$bat_team_id %in% change, change_to)
df2021$bat_team_id <- replace(df2021$bat_team_id, df2021$bat_team_id %in% change, change_to)
df2020$bat_team_id <- replace(df2020$bat_team_id, df2020$bat_team_id %in% change, change_to)

#replace wasn't working for some reason so used an ifelse
df2019$bat_team_id <- ifelse(df2019$bat_team_id == '0', 'Away', 'Home')

#Changing event_id numbers to characters like 2023 so they can be merged
df2019$event_id <- as.character(df2019$event_id)
df2020$event_id <- as.character(df2020$event_id)
df2021$event_id <- as.character(df2021$event_id)
df2022$event_id <- as.character(df2022$event_id)
df2023$event_id <- as.character(df2023$event_id)

#Bring all DFs together through 2022 because they were not 
#previously cleaned in excel
alldata <- bind_rows(df2019, df2020, df2021, df2022)

#replacing event_id numbers with event name
alldata$event_id[alldata$event_id == '2'] <- 'out'
alldata$event_id[alldata$event_id == '3'] <- 'strikeout'
alldata$event_id[alldata$event_id == '4'] <- 'stolen base'
alldata$event_id[alldata$event_id == '5'] <- 'defensive indifference'
alldata$event_id[alldata$event_id == '6'] <- 'caught stealing'
alldata$event_id[alldata$event_id == '8'] <- 'pick off'
alldata$event_id[alldata$event_id == '9'] <- 'wild pitch'
alldata$event_id[alldata$event_id == '10'] <- 'passed ball'
alldata$event_id[alldata$event_id == '11'] <- 'balk'
alldata$event_id[alldata$event_id == '12'] <- 'other advance/out'
alldata$event_id[alldata$event_id == '13'] <- 'foul error'
alldata$event_id[alldata$event_id == '14'] <- 'walk'
alldata$event_id[alldata$event_id == '15'] <- 'intentional walk'
alldata$event_id[alldata$event_id == '16'] <- 'hbp'
alldata$event_id[alldata$event_id == '17'] <- 'interference'
alldata$event_id[alldata$event_id == '18'] <- 'error'
alldata$event_id[alldata$event_id == '19'] <- 'fc'
alldata$event_id[alldata$event_id == '20'] <- 'single'
alldata$event_id[alldata$event_id == '21'] <- 'double'
alldata$event_id[alldata$event_id == '22'] <- 'triple'
alldata$event_id[alldata$event_id == '23'] <- 'home run'

#adding df2023 to the rest of the data
alldata <- bind_rows(alldata, df2023)

#creating dataframe for plate appearances only
batter_plays <- alldata |>
  filter(event_id != 'error' | batted_ball_type != 'NA') |>
  filter(event_id != 'balk' & event_id != 'foul error' & event_id != 'pick off'
         & event_id != 'defensive indifference' & event_id != 'stolen base' & 
           event_id != 'passed ball' & event_id != 'wild pitch' & 
           event_id != 'caught stealing' & event_id != 'interference' & 
           event_id != 'other advance/out') 

#generating column for the number of outcomes between 
#specific batter/pitchers combinations
combos <- batter_plays |>
  group_by(season, batter_id, pitcher_id, event_id) |>
  summarise(n=n())

#overrides PA data to only have the events
batter_plays <- batter_plays |>
  select(season, batter_id, pitcher_id, event_id)

#generates the "before" data for years 2019:2022  
before <- combos |>
  filter(season == '2019' | season == '2020' | season == '2021' | season == '2022')
  
#viewing event frequency  
before |>
  group_by(event_id) |>
  summarize(n=n())
 
#generate "after" data for year 2023
after <- combos |>
  filter(season == '2023')

#total number of events between batters and pitchers for each dataframe
b_combos <- before |>
  group_by(batter_id, pitcher_id) |>
  summarise(btotal = sum(n)) 

a_combos <- after |>
  group_by(batter_id, pitcher_id) |>
  summarise(atotal = sum(n)) 

#filter for 15 or more before combos and 9 or more after combos
ownb <- b_combos |>
  filter(btotal>=15)

owna <- a_combos |>
  filter(atotal>=9)

#creates df with only common combinations
#used to ensure the table at end of code is correct
bna <- left_join(ownb, owna)
bna <- na.omit(bna)


#adds a repetitive column with the total number of events for a combo
before <- left_join(before, b_combos)

#create vector for PA and other data
events <- c('batter_id', 'pitcher_id', 'PA', 'single', 'double', 'triple', 
            'home_run', 'walk', 'hbp', 
            'error', 'strikeout')

#creates empty df using above names for columns
allbefore <- data.frame(matrix(nrow = 0, ncol = length(events)))

colnames(allbefore) = events

#filling in empty df with batter and pitcher values from before df
allbefore[rownames(before), "batter_id"] <- before$batter_id
allbefore[rownames(before), "pitcher_id"] <- before$pitcher_id

#filling in event data with values from before dataframe
allbefore$single <- ifelse(before$event_id == 'single', before$n, '0')
allbefore$double <- ifelse(before$event_id == 'double', before$n, '0')
allbefore$triple <- ifelse(before$event_id == 'triple', before$n, '0')
allbefore$home_run<- ifelse(before$event_id == 'home run', before$n, '0')
allbefore$walk <- ifelse(before$event_id == 'walk', before$n, '0')
allbefore$hbp <- ifelse(before$event_id == 'hbp', before$n, '0')
allbefore$error <- ifelse(before$event_id == 'error', before$n, '0')
allbefore$strikeout <- ifelse(before$event_id == 'strikeout', before$n, '0')

#changing event data to integers 
allbefore$single <- as.integer(allbefore$single)
allbefore$double <- as.integer(allbefore$double)
allbefore$triple <- as.integer(allbefore$triple)
allbefore$home_run <- as.integer(allbefore$home_run)
allbefore$walk <- as.integer(allbefore$walk)
allbefore$hbp <- as.integer(allbefore$hbp)
allbefore$error <- as.integer(allbefore$error)
allbefore$strikeout <- as.integer(allbefore$strikeout)

#"squishes all the rows together so all the data appears in one row
allbefore <- allbefore |> 
  group_by(batter_id, pitcher_id) |>
  summarise(across(c(single, double, triple, home_run, walk, hbp, error, 
                     strikeout), sum))

#Because plate appearances (PA) were not unique, could not sum 
#them with previous data

#creates a filtered before df so that plate appearances is a single row
filter_before <- before |> ungroup() |> select(batter_id, pitcher_id, btotal) 
filter_before <- unique(filter_before)

#creates empty PA column in main df, sorts by batter and 
#pitcher id to ensure the values fill in properly
allbefore[, 'PA'] = NA
allbefore <- arrange(allbefore, batter_id, pitcher_id)
filter_before <- arrange(filter_before, batter_id, pitcher_id)
allbefore$PA <- filter_before$btotal

#rearrange columns so PA is column 3
allbefore <- allbefore[, c(1, 2, 11, 3, 4, 5, 6, 7, 8, 9, 10)]

#creating WOBA column
allbefore <- allbefore |>
  mutate(wOBA = (((.69*walk) + (.72*hbp) + (.89*single) + (1.27*double) + 
                   (1.62*triple) + (2.1*home_run))/PA)) |>
  mutate(across(c('wOBA'), round, 3)) |>
  mutate(WRC = ((((wOBA-.318)/1.2)+.12)*PA)) |>
  mutate(across(c('WRC'), round, 2))

#adds season column, puts it as column 1
allbefore <- allbefore |>
  mutate(season = 's19-22')

allbefore <- allbefore[, c(14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)]

#filter for 15 or more PAs
before15 <- allbefore |> filter(PA >=15)

###
#creating after dataframe, all steps are the same as for the 15before
after <- left_join(after, a_combos)

events <- c('batter_id', 'pitcher_id', 'PA', 'single', 'double', 'triple', 
            'home_run', 'walk', 'hbp', 
            'error', 'strikeout')

allafter <- data.frame(matrix(nrow = 0, ncol = length(events)))

colnames(allafter) = events

allafter[rownames(after), "batter_id"] <- after$batter_id
allafter[rownames(after), "pitcher_id"] <- after$pitcher_id

allafter$single <- ifelse(after$event_id == 'single', after$n, '0')
allafter$double <- ifelse(after$event_id == 'double', after$n, '0')
allafter$triple <- ifelse(after$event_id == 'triple', after$n, '0')
allafter$home_run<- ifelse(after$event_id == 'home run', after$n, '0')
allafter$walk <- ifelse(after$event_id == 'walk', after$n, '0')
allafter$hbp <- ifelse(after$event_id == 'hbp', after$n, '0')
allafter$error <- ifelse(after$event_id == 'error', after$n, '0')
allafter$strikeout <- ifelse(after$event_id == 'strikeout', after$n, '0')

allafter$single <- as.integer(allafter$single)
allafter$double <- as.integer(allafter$double)
allafter$triple <- as.integer(allafter$triple)
allafter$home_run <- as.integer(allafter$home_run)
allafter$walk <- as.integer(allafter$walk)
allafter$hbp <- as.integer(allafter$hbp)
allafter$error <- as.integer(allafter$error)
allafter$strikeout <- as.integer(allafter$strikeout)


allafter <- allafter |> 
  group_by(batter_id, pitcher_id) |>
  summarise(across(c(single, double, triple, home_run, walk, hbp, error, 
                     strikeout), sum))

filter_after <- after |> ungroup() |> select(batter_id, pitcher_id, atotal) 
filter_after <- unique(filter_after)

allafter[, 'PA'] = NA
allafter <- arrange(allafter, batter_id, pitcher_id)
filter_after <- arrange(filter_after, batter_id, pitcher_id)
allafter[rownames(filter_after), "PA"] <- filter_after$atotal

allafter <- allafter[, c(1, 2, 11, 3, 4, 5, 6, 7, 8, 9, 10)]

allafter <- allafter |>
  mutate(wOBA = (((.69*walk) + (.72*hbp) + (.89*single) + (1.27*double) + 
                    (1.62*triple) + (2.1*home_run))/PA)) |>
  mutate(across(c('wOBA'), round, 3)) |>
  mutate(WRC = ((((wOBA-.318)/1.2)+.12)*PA)) |>
  mutate(across(c('WRC'), round, 2))

allafter <- allafter |>
  mutate(season = 's23')

allafter <- allafter[, c(14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)]

after9 <- allafter |> filter(PA >=9)

#create new df that merges on batter and pitcher id where before 
#left half of the data and after is the right side of the data
qualified <- merge(before15, after9, by = c('batter_id', 'pitcher_id'))

#create csv for visualization in tableau
write.csv(qualified, '~/Baseball Stuff\\qualified.csv', row.names = FALSE)
