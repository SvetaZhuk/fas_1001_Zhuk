dat_2021 <- read_csv("~/Dropbox/fas_1001_Zhuk/_tp/_tp2/Data/cora-cdem-2021_F1.csv")

head(dat_2021)
dim(dat_2021)

class(dat_2021$dc21_imm_level)

dat_2019 <- read_csv("~/Dropbox/fas_1001_Zhuk/_tp/_tp2/Data/cora-cdem-2019_F1.csv")
dim(dat_2019)

summary(dat_2019$immigration_level)
table(dat_2019$immigration_level, useNA = "always")

table(dat_2021$dc21_imm_level, useNA = "always")

metadata <- read_csv("~/Dropbox/fas_1001_Zhuk/_tp/_tp2/Data/Metadata_Country_API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_6508308.csv")

dim(metadata)

metadata_can <- read_csv("~/Dropbox/fas_1001_Zhuk/_tp/_tp2/Data/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_6508308.csv", skip = 4)

dim(metadata_can)

head(metadata_can)
table(metadata_can$Canada)

library(tidyverse)

metadata_can_unemployment <- metadata_can |> select('Country Name', '2019', '2021') |> 
  filter('Country Name' == "Canada")

class(metadata_can$"Country Name")

class(metadata_can)
library(dplyr)

unique(metadata_can$"Country Name")

metadata_can_unemployment <- metadata_can |> 
  select(`Country Name`, `2019`, `2021`) |> 
  filter(`Country Name` == "Canada")

#смотрим сколько у нас есть данных в основном dataset по immigration_level_2019, чтобы потом сравнить
dat_2019 |> select(immigration_level) |> 
  table()

#отфильтровала 2019 по immigration_level, убрала obs 4 - don't know, переименовала observations

dat_2019_imm <- dat_2019 |> mutate(immigration_level = case_when(
  immigration_level == 1 ~ "More", 
  immigration_level == 2 ~ "Fewer", 
  immigration_level == 3 ~ "About the same as now")) |> select(immigration_level) |> 
  filter(immigration_level != 4) 

#проверяю, чтобы числа совпадали

dat_2019_imm |> table()

class(dat_2019_imm$immigration_level)

#нужно создать новую колонну year, чтобы на каждый obs было 2019 

dat_2019_imm <- dat_2019_imm |> mutate(year = "2019")

# Отфильтровала для 2019, теперь тоже самое нужно сделать для 2021 
# cначала проверю какие у меня есть данные в переменной про immigrants и переименую ее 

table(dat_2021$dc21_imm_level)

# Создаю новый data frame
# переменную переименовать 
# переименовать observations
# NA убрать
# сравнить результаты
# добавить строчку год 

dat_2021_imm <- dat_2021 |> mutate(immigration_level = case_when(
  dc21_imm_level == 1 ~ "More", 
  dc21_imm_level == 2 ~ "Fewer", 
  dc21_imm_level == 3 ~ "About the same as now")) |> 
  select(immigration_level) |> 
  filter(immigration_level != "-99")

# проверяю те же ли данные у меня по числам 

dat_2021_imm |> table(useNA ="always")

# супер, теперь нужно добавить новую колонку год 

dat_2021_imm <- dat_2021_imm |> mutate(year = "2021")

dat_2021_imm |> head()

# теперь мне нужно как-то совместить эти два data sets 

# 4 - Fusion des données ----

Data_clean <- left_join(dat_2019_imm, dat_2021_imm) |> # Jonction des données
  na.omit()


Data_clean <- full_join(dat_2019_imm, dat_2021_imm, by = "immigration_level") 

## Совместила два data sets ЮХУУУУУУУУУ

Data_clean <- bind_rows(dat_2019_imm, dat_2021_imm)

Data_clean |> select(year) |> 
  table()

Data_clean |> select(immigration_level) |> 
  table()

# Cупер даже все совпадает. Теперь мне нужно как-то сюда вставить data.set с unemployment
# кажется нужно поменять на pivot.longer 

metadata_can_unemployment_longer <- metadata_can_unemployment |> 
  pivot_longer(cols = starts_with(c("2019", "2021")),
               names_to = "year", 
               values_to = "unemployment")

metadata_can_unemployment_longer |> head()

# Ура! I'm so fucking special 
# теперь мне нужно это тоже как-то добавить в мои данные merge c data set 2019, 2021, который я превратила в data_clean 


Data_combined <- left_join(Data_clean, metadata_can_unemployment_longer, by = c("year"))

Data_combined |> select(immigration_level) |> 
  table()

#Тут какие-то пляски с бубном, который предложил Chat GPT, чтобы нормально merge columns


Data_combined <- Data_combined %>%
  mutate(`Country Name` = coalesce(`Country Name.x`, `Country Name.y`), 
         `Unemployment` = coalesce(`unemployment.x`, `unemployment.y`)) %>%
  select(-contains(".x"), -contains(".y")) # Remove the old "X" and "Y" columns


#похоже, что сработало
Data_combined |> select(Unemployment) |> 
  table()

# теперь попробую сделать график в ggplot и подумать, что там с ним делать 
# я сделала график, но похоже, что мне нужно переделать мои данные, потому что я решила, что очевиднее будет сделать по среднему

library(ggplot2)

ggplot(data = Data_combined, aes(x = immigration_level, fill = as.factor(year))) +
  geom_bar(position = "dodge") +
  scale_x_discrete(limits = c("More", "Fewer", "About the same as now")) + # изменяет порядок столбцов на графике, и переменная categorielle 
  scale_fill_manual(values = c("2019" = "#f0f0f0", "2021" = "#bdbdbd"), name = "Year") +
  labs(title = "Comparisons of Immigration level perceptions by year", 
       subtitle = "Unemployment rates : 2019 - 5.66%, 2021 - 7.46%",
       x = "Do you think Canada shoud admit ... [immigrants]", 
       y = "Don't know") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "black", size = .2), 
        panel.grid.minor.y = element_line(color = "grey70", size = .1),
        axis.line = element_line(color = "grey"), 
        plot.title = element_text(size = 14, family = "Times New Roman"),
        text = element_text(face = "plain", family = "Times New Roman"))

## Cчитаем пропорции по 2019 и 2022 

data_clean_proportions <- Data_combined |>
  filter(!is.na(immigration_level)) |> 
  group_by(year, immigration_level) |> 
  summarise(count = n(), Unemployment = first(Unemployment), .groups = "keep") |> 
  group_by(year) |> 
  mutate(total = sum(count), proportion = count / total * 100)


## теперь как это все прекрасность использовать в графике 

ggplot(data = data_clean_proportions, 
       aes(x = immigration_level, 
           y = proportion, 
           fill = as.factor(year))) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("More", "About the same as now", "Fewer")) + # изменяет порядок столбцов на графике, и переменная categorielle
  scale_fill_manual(values = c("2019" = "#f0f0f0", "2021" = "#bdbdbd"), name = "Year") +
  labs(title = "Comparisons of Immigration level perceptions by year", 
       subtitle = "Unemployment rates : 2019 - 5.66%, 2021 - 7.46%",
       x = "Do you think Canada shoud admit ... [immigrants]", 
       y = "Proportions") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "black", size = .2), 
        panel.grid.minor.y = element_line(color = "black", size = .1),
        axis.line = element_line(color = "grey"), 
        plot.title = element_text(size = 14, family = "Times New Roman"),
        text = element_text(face = "plain", family = "Times New Roman"))


## Попробую построить другой график, но нужно понять каааааак 


can_imm_att <- ggplot(data = data_clean_proportions, 
       aes(x = as.factor(Unemployment), 
           y = proportion,
           group = immigration_level)) +
  geom_line(aes(linetype = immigration_level)) +
  geom_point(size = 2) +
  scale_x_discrete(labels = c("5.66%", "7.46%")) +
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0, 50, by = 10)) +
  labs(title = "Canada's Immigration Attitudes and Unemployment rate: A 2019-2021 Perspective", 
       x = "Unemployment rate", 
       y = "Proportion of attitudes towards immigration (%)", 
       linetype = "Canada should admit ... [immigrants]") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "black", size = .2), 
        panel.grid.minor.y = element_line(color = "black", size = .1),
        axis.line = element_line(color = "grey"), 
        plot.title = element_text(size = 14, family = "Times New Roman"),
        text = element_text(face = "plain", family = "Times New Roman"))

can_imm_att

## Теперь пробую сохранить этот график 

save(can_imm_att, file = "~/Dropbox/fas_1001_Zhuk/_tp/_tp2/figures/can_imm_att.Rda")

## png

ggsave(can_imm_att, 
       filename = "~/Dropbox/fas_1001_Zhuk/_tp/_tp2/figures/final/can_imm_att.png",
       dpi = 320, 
       bg = "white",
       units = "cm",
       height = 10, 
       width = 20)

## теперь нужно попотеть и сохранить 2ой график 

can_imm_att_bars <- ggplot(data = data_clean_proportions, 
       aes(x = immigration_level, 
           y = proportion, 
           fill = as.factor(year))) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("More", "About the same as now", "Fewer")) + 
  scale_fill_manual(values = c("2019" = "#f0f0f0", "2021" = "#bdbdbd"), name = "Year") +
  labs(title = "Comparisons of Immigration level perceptions by year in Canada", 
       subtitle = "Unemployment rates : 2019 - 5.66%, 2021 - 7.46%",
       x = "Do you think Canada shoud admit ... [immigrants]", 
       y = "Proportions (%)") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "black", size = .2), 
        panel.grid.minor.y = element_line(color = "black", size = .1),
        axis.line = element_line(color = "grey"), 
        plot.title = element_text(size = 14, family = "Times New Roman"),
        text = element_text(face = "plain", family = "Times New Roman"))

can_imm_att_bars

## сохраняю это потрясающее действо 

save(can_imm_att_bars, file = "~/Dropbox/fas_1001_Zhuk/_tp/_tp2/figures/can_imm_att_bars.Rda")

ggsave(can_imm_att_bars, 
       filename = "~/Dropbox/fas_1001_Zhuk/_tp/_tp2/figures/final/can_imm_att_bars.png",
       dpi = 320, 
       bg = "white",
       units = "cm",
       height = 10, 
       width = 20)

