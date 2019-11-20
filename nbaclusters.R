library(dplyr)
library(nbastatR)

teams = nba_teams()
teamnames = teams[c("slugTeam","idTeam")]
#View(x[[2]][[1]])
x = days_scores(game_dates =as.Date(as.Date("2019-10-22"):as.Date("2019-11-18"), origin="1970-01-01")
                , include_standings = F, return_message = T)
games = x[[2]][[1]][c("dateGame","idGame","idTeamHome","idTeamAway")]
withid = merge(games,teamnames,by.x = "idTeamAway",by.y = "idTeam",sort = FALSE)
colnames(withid)[5] = "AwayTeam"

mu = merge(withid,teamnames,by.x = "idTeamHome",by.y = "idTeam",sort = FALSE)
colnames(mu)[6] = "HomeTeam"

mu = mu[c("idGame","dateGame","AwayTeam","HomeTeam")]

b1 = box_scores(game_ids = mu$idGame,box_score_types = "traditional",result_types = "player")

b2 = box_scores(game_ids = mu$idGame,box_score_types = "advanced",result_types = "player")

b3 = box_scores(game_ids = mu$idGame,box_score_types = "misc",result_types = "player")

b4 = box_scores(game_ids = mu$idGame,box_score_types = "tracking",result_types = "player")
#View(b4[2][[1]][[1]])

merged1 = merge(b1[2][[1]][[1]],b2[2][[1]][[1]])
merged1 = merge(merged1,b3[2][[1]][[1]])
merged1 = merge(merged1,b4[2][[1]][[1]])
merged1 = merge(merged1,mu)

merged1 = merged1 %>%
  mutate(location = ifelse(slugTeam == HomeTeam,"Home","Away"))
merged1 = merged1 %>%
  mutate(opponent = ifelse(slugTeam == HomeTeam,AwayTeam,HomeTeam))
merged1 = merged1 %>%
  mutate(passes = round(passes/minExact,2))
merged1 = merged1 %>%
  mutate(rebSucces = round(treb/trebChances,2))
merged1 = merged1 %>%
  mutate(passLead = round(ast/minExact/passes,2))
merged1 = merged1 %>%
  mutate(tp36 = round(touches / minExact * 36,1))
merged1 = merged1 %>%
  mutate(chuckPct = round( fgaContested / (fgaUncontested + fgaContested),2))
merged1 = merged1 %>%
  mutate(rebch36 = round(trebChances / minExact * 36,1))
merged1 = merged1 %>%
  mutate(fbp36 = round(ptsFastBreak / minExact * 36,1))
merged1 = merged1 %>%
  mutate(paintp36 = round(ptsPaint / minExact * 36,1))
merged1 = merged1 %>%
  mutate(dd = ifelse(treb>=10,1,0)*ifelse(ast>=10,1,0)*1.5+ifelse(treb>=10,1,0)*ifelse(pts>=10,1,0)*1.5+ifelse(pts>=10,1,0)*ifelse(ast>=10,1,0)*1.5)
merged1 = merged1 %>%
  mutate(dk = pts+(tov*-0.5)+((stl+blk)*2)+(ast*1.5)+(treb*1.25)+(fg3m*0.5)+dd)
merged1 = merged1 %>%
  mutate(dkpm = round(dk / minExact,2))
head(merged1)
merged1 = merge(clustdf,merged1)
clustdf
##### cluster
colnames(merged1)
cluster = merged1[c("namePlayer","minExact","ptsPaint","passes","ast","pfd","treb","trebChances","blk","stl","touches","fg3a","fgaRimDefended","fgaUncontested","fta","pf","distMiles","pts")]
summed = aggregate(. ~ namePlayer, cluster, sum)
inc_cols = c("ptsPaint","passes","ast","pfd","treb","trebChances","blk","stl","touches","fg3a","fgaRimDefended","fgaUncontested","fta","pf","distMiles","pts")
library(data.table)
setDT(summed)
summed[, (inc_cols) := lapply(.SD, function(x) 
  x / summed[['minExact']] ), .SDcols = inc_cols]
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
data2 <- summed
library(tidyverse)
data2 = data2 %>% remove_rownames %>% column_to_rownames(var="namePlayer")
data2 = data2[,-1]
data2 <- na.omit(data2)

data2 = scale(data2)
head(data2)

k2 <- kmeans(data2, centers = 3, nstart = 25)
str(k2)
fviz_cluster(k2, data = data2)
data2[k2$cluster==1,]
clustdf <- data.frame(data2,k2$cluster)
clustdf = rownames_to_column(as.data.frame(clustdf), var = "namePlayer")
clustdf[,"grp"] = clustdf$k2.cluster
clustdf = clustdf[c("namePlayer","grp")]
merged1 = merge(clustdf,merged1)
####

todesktop = merged1[c("idGame","dateGame","location","slugTeam","opponent","namePlayer","minExact","fg3m","fg3a","fgm","fga","ftm","fta","oreb","treb","ast","tov","pf","pfd","stl","blk","blka","pts","dk","dkpm","pctAST","pctTREB","pctUSG","pace","touches","tp36","rebSucces","trebChances","rebch36","passes","passLead","chuckPct","fbp36","paintp36","ptsFastBreak","ptsPaint","ptsOffTOV","ptsSecondChance","fgaContested","fgmContested","fgaUncontested","fgmUncontested","fgmRimDefended","fgaRimDefended","grp")]
todesktop = todesktop %>%
  group_by(idGame) %>%
  arrange(slugTeam)
todesktop = todesktop %>%
  group_by(slugTeam) %>%
  arrange(idGame)
todesktop$minExact = round(todesktop$minExact,2)
todesktop$pace = round(todesktop$pace,0)
todesktop$pctAST = round(todesktop$pctAST,2)
todesktop$pctTREB = round(todesktop$pctTREB,2)
todesktop$pctUSG = round(todesktop$pctUSG,2)

write.csv(todesktop, file = "/Users/Macbook/Desktop/20boxscores.csv")
tb1 = box_scores(game_ids = mu$idGame,box_score_types = "traditional",result_types = "team")
tb2 = box_scores(game_ids = mu$idGame,box_score_types = "advanced",result_types = "team")
tb3 = box_scores(game_ids = mu$idGame,box_score_types = "misc",result_types = "team")
tb4 = box_scores(game_ids = mu$idGame,box_score_types = "tracking",result_types = "team")

merged = merge(tb1[2][[1]][[1]],tb2[2][[1]][[1]])
merged = merge(merged,tb3[2][[1]][[1]])
merged = merge(merged,tb4[2][[1]][[1]])
merged = merge(merged,mu)
merged = merged %>%
  mutate(chuckPct = round( fgaContested / (fgaContested + fgaUncontested),2))
merged = merged %>%
  mutate(location = ifelse(slugTeam == HomeTeam,"Home","Away"))
merged = merged %>%
  mutate(opponent = ifelse(slugTeam == HomeTeam,AwayTeam,HomeTeam))

tbtodesktop = merged[c("idGame","dateGame","location","opponent","slugTeam","minExact","fg3m","fg3a","fgm","fga","ftm","fta","oreb","treb","ast","tov","pf","pfd","stl","blk","blka","pts","pace","chuckPct", "ptsFastBreak","ptsPaint","ptsOffTOV","ptsSecondChance","ptsFastBreakOpponent","ptsPaintOpponent","ptsOffTOVOpponent","ptsSecondChanceOpponent","fgmContested","fgaContested","fgmUncontested","fgaUncontested","fgmRimDefended","fgaRimDefended")]
tbtodesktop = tbtodesktop %>%
  group_by(slugTeam) %>%
  arrange(idGame)
tbtodesktop$minExact = round(tbtodesktop$minExact,1)
tbtodesktop$pace = round(tbtodesktop$pace,0)
write.csv(tbtodesktop, file = "/Users/Macbook/Desktop/20teamboxscores.csv")

View(tb4[2][[1]][[1]])

#k2$cluster
#table(k2$cluster)
#set.seed(123)
#View(clustdf)
#fviz_nbclust(data2, kmeans, method = "wss")
#fviz_nbclust(data2, kmeans, method = "silhouette")
