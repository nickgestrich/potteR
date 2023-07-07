## code to prepare `guinea` dataset goes here

library(dplyr, warn.conflicts = FALSE)
library(readxl)
library(janitor)
library(tidyr)
library(stringr)

rim <- read_excel(here::here("U:/github projects/guinea/excavation/ceramics/data/Projet Niger Milo 2023 Ceramique tessons.xlsx"),sheet = "tessons de bord")
body <- read_excel(here::here("U:/github projects/guinea/excavation/ceramics/data/Projet Niger Milo 2023 Ceramique tessons.xlsx"),sheet = "tessons de planse")

#take only site NDK1
rim <- rim |>
  janitor::clean_names() |>
  filter(site == "NDK1")

body <- body |>
  janitor::clean_names() |>
  filter(site == "NDK1")

#make data uniform
rim <- rim |>
  rename(rim_angle = x, rim_type = rim) |>
  separate(context, into = c("sondage", "context"), sep = "-")

body <- body |>
  mutate(col = as.character(col),
         inc = as.character(inc)) |> # in future, check all data types programmatically
  separate(cont, into = c("sondage", "context")) # here the data is not uniform, so more work is needed to extract these columns properly

body1 <- body |> filter(sondage %in% c("T1", "B"))
body2 <- body |>
  filter(!sondage %in% c("T1", "B")) |>
  mutate(context = sondage) |>
  mutate(sondage = str_sub(sondage, 1, 1),
         context = str_sub(context, 2, 3))

body <- bind_rows(body1, body2)

rm(body1, body2)

#into both datasets, add a combined unit-context vector
rim <- rim |> mutate(sond_con = paste(sondage, context, sep = "-"))
body <- body |> mutate(sond_con = paste(sondage, context, sep = "-"))

# unify data types
#the numeric variables
numerics <- c("n_sh", "diam", "mx_th", "min_th")

#convert all others to character vectors
rim <- rim |>
  mutate_if(!(names(rim) %in% numerics), as.character)

body <- body |>
  mutate_if(!(names(body) %in% numerics), as.character)

# Next, we have certain attributes that are exclusive to rim sherds,
# whereas others are common to both rim and body sherds.
# We will thus build a further dataset with all sherds in order to analyse the latter category.

r <- rim |> select(all_of(names(body)))

all_sherds <- bind_rows(body, r)

rm(r)

guinea <- list(rim, body, all_sherds)

usethis::use_data(guinea, overwrite = TRUE)
