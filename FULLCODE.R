team5man = read.csv("5manteam.csv", header = TRUE, sep = ",", dec = ".")
opp5man = read.csv("5manopponent.csv", header = TRUE, sep = ",", dec = ".")

a = left_join(team5man, opp5man, by = "Name")
a$netpoints5 = a$Points.x - a$Points.y
##################################
team4man = read.csv("4manteam.csv", header = TRUE, sep = ",", dec = ".")
opp4man = read.csv("4manopponent.csv", header = TRUE, sep = ",", dec = ".")

b = left_join(team4man, opp4man, by = "Name")
b$netpoints4 = b$Points.x - b$Points.y
##################################
team3man = read.csv("5manteam.csv", header = TRUE, sep = ",", dec = ".")
opp3man = read.csv("5manopponent.csv", header = TRUE, sep = ",", dec = ".")

a = left_join(team5man, opp5man, by = "Name")
a$netpoints5 = a$Points.x - a$Points.y
##################################
team3man = read.csv("3manteam.csv", header = TRUE, sep = ",", dec = ".")
opp3man = read.csv("3manopponent.csv", header = TRUE, sep = ",", dec = ".")

c = left_join(team3man, opp3man, by = "Name")
c$netpoints3 = c$Points.x - c$Points.y
##################################
team2man = read.csv("2manteam.csv", header = TRUE, sep = ",", dec = ".")
opp2man = read.csv("2manopponent.csv", header = TRUE, sep = ",", dec = ".")

d = left_join(team2man, opp2man, by = "Name")
d$netpoints2 = d$Points.x - d$Points.y
##################################
team1man = read.csv("1manteam.csv", header = TRUE, sep = ",", dec = ".")
opp1man = read.csv("1manopponent.csv", header = TRUE, sep = ",", dec = ".")

e = left_join(team1man, opp1man, by = "Name")
e$netpoints1 = e$Points.x - e$Points.y
##################################
team0man = read.csv("0manteam.csv", header = TRUE, sep = ",", dec = ".")
opp0man = read.csv("0manopponent.csv", header = TRUE, sep = ",", dec = ".")

f = left_join(team0man, opp0man, by = "Name")
f$netpoints0 = f$Points.x - f$Points.y
##################################
a = arrange(a, Name)
b = arrange(b, Name)
c = arrange(c, Name)
d = arrange(d, Name)
e = arrange(e, Name)
f = arrange(f, Name)

stbn = cbind(a[,c(1,56)], b$netpoints4, c$netpoints3, d$netpoints2, e$netpoints1, f$netpoints0)

colnames(stbn) = c("Team", 
                   "Net Rating - 5 Starters", "Net Rating - 4 Starters",
                   "Net Rating - 3 Starters", "Net Rating - 2 Starters",
                   "Net Rating - 1 Starters", "Net Rating - 0 Starters")

write.xlsx(stbn,
           "netrtg.xlsx",
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE,
           showNA = TRUE,
           password = NULL
)
                   

postbn = subset(stbn, stbn$Name != "ATL" & stbn$Name != "CHA" &
                  stbn$Name != "CHI" & stbn$Name != "CLE" &
                  stbn$Name != "DET" & stbn$Name != "GSW" &
                  stbn$Name != "MEM" & stbn$Name != "MIN" &
                  stbn$Name != "NOP" & stbn$Name != "NYK" &
                  stbn$Name != "PHX" & stbn$Name != "SAC" &
                  stbn$Name != "SAS" & stbn$Name != "WAS")

nbateams = read.csv("nbateams.csv", header = T, sep =";")
nbateams = nbateams[,1:2]
aaa = merge(stbn, nbateams, by = "Name")

library(dplyr)
library(tidyverse)
library(reshape2)

deneme = melt(stbn, id.vars = "Name")

ggplot(deneme, aes(variable, value)) + 
  geom_line(aes(group = Name), color = "gray", alpha = 0.6) +
  geom_line(data = subset(deneme, Name == "MIL"), aes(group = Name), color = "dark green") +
  geom_line(data = subset(deneme, Name == "UTA"), aes(group = Name), color = "purple") +
  geom_line(data = subset(deneme, Name == "LAC"), aes(group = Name), color = "dark blue") +
  geom_line(data = subset(deneme, Name == "TOR"), aes(group = Name), color = "dark red") +
  geom_line(data = subset(deneme, Name == "BOS"), aes(group = Name), color = "green") +
  ##geom_line(data = subset(deneme, Name == "LAL"), aes(group = Name), color = "gold") +
  ##geom_label(data = subset(deneme, Name == "MIL" | Name == "UTA" | Name == "LAC" | 
                          ## Name == "TOR" | Name == "BOS" | Name == "LAL"), aes(label = Name)) +
  labs(title = "How Effective Is Each Team When Playing A Given Number Of Starters?",
     x = 'Total Number of Starters On Court',
     y = 'Net Points (Team - Opponent) Per 100 Poss',
     caption = 'graph: @burakcankoc \nSource: pbpstats.com') +
  scale_x_discrete(labels=c("5","4","3","2","1","0")) +
  theme(text = element_text(size=20),
        legend.position = "right",
        legend.title = element_text(face = "bold")) +
  geom_label(aes(x = "netpoints5", y = 21.3, label = "MIL")) +
  geom_label(aes(x = "netpoints5", y = 11.6, label = "UTA")) +
  geom_label(aes(x = "netpoints5", y = 9.75, label = "LAC")) +
  geom_label(aes(x = "netpoints5", y = 8.7, label = "TOR")) +
  geom_label(aes(x = "netpoints5", y = 8.0, label = "BOS"))
  ##geom_label(aes(x = "netpoints5", y = 5.1, label = "LAL")) +
  ##facet_wrap(~Name)
  
minutes = cbind(a[,c(1,2)], b$Minutes.x, c$Minutes.x, d$Minutes.x, e$Minutes.x, f$Minutes.x)
library(janitor)
minutes = minutes %>% adorn_totals("row")

library(xlsx)

write.xlsx(minutes,
  "minutesnba.xls",
  sheetName = "Sheet1",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE,
  showNA = TRUE,
  password = NULL
)

xsds = read.xlsx("minutesnba.xls", sheetIndex = 1, as.data.frame = T)
colnames(xsds) = c("Team", 
                   "Minutes(5)", "% of Total Minutes(5)", 
                   "Minutes(4)", "% of Total Minutes(4)", 
                   "Minutes(3)", "% of Total Minutes(3)",
                   "Minutes(2)", "% of Total Minutes(2)", 
                   "Minutes(1)", "% of Total Minutes(1)",
                   "Minutes(0)", "% of Total Minutes(0)")

xsds


library(gt)

gt_tbl = gt(tibble(xsds))

gt_tbl <- 
  gt_tbl %>%
  tab_header(
    title = md("**Number of Starters on Court and Proportion of Total Minutes Played**")
  ) %>%
  tab_source_note(
    source_note = md("Table: @burakcankoc")
  ) %>%
  tab_source_note(
    source_note = md("Source: pbpstats.com")
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "left", weight = "bold")
    ),
    locations = cells_body(
      columns = vars(Team))
  ) %>%
  tab_spanner(
    label = "5 Starters",
    columns = 2:3
  ) %>%
  tab_spanner(
    label = "4 Starters",
    columns = 4:5
  ) %>%
  tab_spanner(
    label = "3 Starters",
    columns = 6:7
  ) %>%
  tab_spanner(
    label = "2 Starters",
    columns = 8:9
  ) %>%
  tab_spanner(
    label = "1 Starters",
    columns = 10:11
  ) %>%
  tab_spanner(
    label = "0 Starters",
    columns = 12:13, gather = T
  ) %>%
  tab_style(
    style = cell_text(align = "center" ,weight = "bold"),
    locations = cells_column_spanners(spanners = c("5 Starters", "4 Starters", 
                                                   "3 Starters", "2 Starters", 
                                                   "1 Starters", "0 Starters"))
  ) %>%
  fmt_percent(
    columns = c(3,5,7,9,11,13),
    decimals = 2
  ) %>% 
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels(columns = 1:13) 
  ) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_body()
  ) %>%
  tab_style(
    style = cell_fill(color = "bisque1"),
    locations = cells_body(
      rows = 31)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = 31)
  ) %>% 
  tab_options(
    heading.title.font.size = px(20)
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = c("left"),
        color = "solid",
        weight = px(10)
      )
    ),
    locations = list(
      cells_body(
        columns = 2,
        rows = everything()
      ),
      cells_body(
        columns = 4,
        rows = everything()
      ),
      cells_body(
        columns = 6,
        rows = everything()
      ),
      cells_body(
        columns = 8,
        rows = everything()
      ),
      cells_body(
        columns = 10,
        rows = everything()
      ),
      cells_body(
        columns = 12,
        rows = everything()
      )
    )
  )

gt_tbl

########################################
teammajority = read.csv("majorityvsmajorityteam.csv", header = TRUE, sep = ",", dec = ".")
oppmajority = read.csv("majorityvsmajorityopponent.csv", header = TRUE, sep = ",", dec = ".")
amajority = left_join(teammajority, oppmajority, by = "Name")
amajority$netpoints5 = amajority$Points.x - amajority$Points.y
amajority = arrange(amajority, Name)
majority = cbind(amajority[,c(1,56)])
colnames(majority) = c("Team", "Net Points")

write.xlsx(majority,
           "majorityvsmajority.xlsx",
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE,
           showNA = TRUE,
           password = NULL
)


#######################################
teamstarter = read.csv("starterteam.csv", header = TRUE, sep = ",", dec = ".")
oppstarter = read.csv("starteropponent.csv", header = TRUE, sep = ",", dec = ".")
teambench = read.csv("benchteam.csv", header = TRUE, sep = ",", dec = ".")
oppbench = read.csv("benchopponent.csv", header = TRUE, sep = ",", dec = ".")

starter = left_join(teamstarter, oppstarter, by = "Name")
starter$netpointstarter = starter$Points.x - starter$Points.y
bench = left_join(teambench, oppbench, by = "Name")
bench$netpointsbench = bench$Points.x - bench$Points.y
starben = cbind(starter[,c(1,56)], bench$netpointsbench)
colnames(starben) = c("Team", "Starters", "Bench")

deneme = melt(starben, id.vars = "Team")

ggplot(data = deneme, aes(x=variable, y=value, group = variable)) +
  geom_boxplot(notch=T) +
  geom_point() +
  scale_y_continuous(name = "Net Points per 100 Poss",
                     breaks = seq(-15, 15, 2.5),
                     limits=c(-15, 15)) +
  ggtitle("Net Points by Starters and Benches on the Court") +
  labs(title = "Starter Majority Lineups and Bench Majority Lineups",
       x = 'Lineup Majority On Court',
       y = 'Net Points (Team - Opponent) Per 100 Poss',
       caption = 'graph: @burakcankoc \nSource: pbpstats.com') +
  theme(text = element_text(size=20))

mean(bench$netpointsbench)
mean(starter$netpointstarter)

starben = arrange(starben, Team)
