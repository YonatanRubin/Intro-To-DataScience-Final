### installations
# install.packages(reticulate)
# library(reticulate)
# reticulate::install_miniconda()
# then in terminal:
# conda install pandas

library(reticulate)
library(tidyverse)
use_miniconda("~/.local/share/r-miniconda")
pd<-import("pandas")

#credit to camille from here https://stackoverflow.com/questions/68581643/how-to-convert-a-string-representation-of-a-list-to-a-list-with-the-tidyverse
parse_list <- function(str){
  unlist(strsplit(gsub("[\\[\\]']", "", str, perl = TRUE), ", "))
}
bool<- function(str){
  str=="1"
}


ingridiants_2<- tibble(pd$read_pickle("data/ingr_map.pkl"))

recipes <- read_csv("data/RAW_recipes.csv")
pp_rec<-read_csv("data/PP_recipes.csv")

techniques_order<-c(
  'bake','barbecue','blanch','blend','boil','braise','brine','broil','caramelize','combine',
  'crock pot','crush','deglaze','devein','dice','distill','drain','emulsify','ferment','freeze',
  'fry','grate','griddle','grill','knead','leaven','marinate','mash','melt','microwave','parboil',
  'pickle','poach','pour','pressure cook','puree','refrigerate','roast','saute','scald','scramble',
  'shred','simmer','skillet','slow cook','smoke','smooth','soak','sous-vide','steam','stew','strain',
  'tenderize','thicken','toast','toss','whip','whisk')

get_techniques <- function(x) {
  split = strsplit(x,"")[[1]]
  techniques_order[split=="1"]
}

recipes<-recipes|>
  mutate(nutrition = map(str_extract_all(nutrition, "[0-9\\.]+"), as.numeric),
         calories = sapply(nutrition, `[`, 1),
         total_fat = sapply(nutrition, `[`, 2),
         sugar = sapply(nutrition, `[`, 3),
         sodium = sapply(nutrition, `[`, 4),
         protein = sapply(nutrition, `[`, 5)) |>
  select(-nutrition)

# we first create a column for each technique, the for each row fill it with the
pp_rec <- pp_rec |>
  mutate(techniques = gsub("\\D", "", techniques)) |> # change to a single string of boolean flags
  group_by(id) |>
    reframe(id = id, t = get_techniques(techniques)) |> # get all the techniques applied for each recipe
  ungroup() |>
  mutate(applied = TRUE) |> pivot_wider(
    id_cols = id,
    names_from = t,
    values_from = applied,
    values_fill = FALSE
  ) %>% # pivot wider
  full_join(pp_rec, ., by = "id") |> # rejoin with the original to keep the other columns
  select(-c(techniques)) #filter the original techniques

recipe_ingrediants<-pp_rec |>
  group_by(id) |>
    reframe(recipe_id = id, ingrediant_id = as.integer(parse_list(ingredient_ids))) |> 
  left_join(ingridiants_2,join_by("ingrediant_id"=="id"),multiple="first")

# find recipes that are mainly popular in a specific season
INTERACTION_COUNT <- 25
CENTRELIZED_SEASON_EVENNESS<-0.75

seasons<-c("winter","spring","summer","fall")
interactions<-read_csv("data/RAW_interactions.csv") |>
  mutate(season=month(date)%%12%/%3) |>
  mutate(season=seasons[season+1])

recipe_by_season<-interactions |>
  group_by(recipe_id,season) |>
    summarise(count=n())
  
# we now want to find if a recipe is mostly made in one season or multiple

#using pielou evenness to check the evenness since it was the first result when I googled how to check evenness
pielou_evenness <- function(distribution,groups=NA) {
  if(is.na(groups)){
    groups<-length(distribution)
  }
  n <- sum(distribution)
  p <- distribution[distribution>0] / n
  shannon_entropy <- -sum(p * log(p))
  max_entropy <- log(groups)
  evenness_index <- shannon_entropy / max_entropy
  return(evenness_index)
}

recipes_eveness<-recipe_by_season |>
  group_by(recipe_id) |>
    filter(sum(count)>INTERACTION_COUNT) |>
  pivot_wider(id_cols = c("recipe_id"), names_from = season, values_from = count, values_fill = 0) |> # it is faster to filter and only then pivot for some rason
  rowwise()|>
  mutate(evenness=pielou_evenness(across(all_of(seasons)))) |>
  mutate(total=sum(across(all_of(seasons))),most_common=seasons[max.col(across(all_of(seasons)))])

### QUESTION 1 ###

#assuming complexity is a function of time and steps
minute_quantile<-quantile(recipes$minutes,c(0.1,0.9))
steps_quantile<-quantile(recipes$n_steps,c(0.1,0.9))

filtered<-recipes |>
  filter(between(minutes,minute_quantile[1],minute_quantile[2]),between(n_steps,steps_quantile[1],steps_quantile[2])) |>
  right_join(recipes_eveness,join_by("id"=="recipe_id"))|>
  mutate(most_common=ifelse(evenness>CENTRELIZED_SEASON_EVENNESS,"none",most_common))

minutes_median<-median(filtered$minutes,na.rm = TRUE)
steps_median<-median(filtered$n_steps,na.rm = TRUE)

filtered |> ggplot()+
  aes(x=n_steps,y=minutes,color=most_common)+
  geom_density2d()+
  geom_hline(yintercept = minutes_median)+
  geom_vline(xintercept = steps_median)+
  facet_wrap(vars(most_common))