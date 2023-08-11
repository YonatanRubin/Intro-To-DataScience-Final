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

ingridiants_2<- tibble(pd$read_pickle("data/ingr_map.pkl"))

recipes <- read_csv("data/RAW_recipes.csv") |>
  select(-c(steps,description,ingredients))|>
  mutate(nutrition = map(str_extract_all(nutrition, "[0-9\\.]+"), as.numeric),
         calories = sapply(nutrition, `[`, 1),
         total_fat = sapply(nutrition, `[`, 2),
         sugar = sapply(nutrition, `[`, 3),
         sodium = sapply(nutrition, `[`, 4),
         protein = sapply(nutrition, `[`, 5)) |>
  select(-nutrition)


pp_rec<-read_csv("data/PP_recipes.csv") |> select(-ends_with("tokens"))

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


# # we first create a column for each technique, the for each row fill it with the

# recipe_ingrediants<-pp_rec |>
  # group_by(id) |>
  #   reframe(recipe_id = id, ingrediant_id = as.integer(parse_list(ingredient_ids))) |> 
  # left_join(ingridiants_2,join_by("ingrediant_id"=="id"),multiple="first")

# find recipes that are mainly popular in a specific season
INTERACTION_COUNT <- 25
CENTRELIZED_SEASON_EVENNESS<-0.75
seasons<-c("winter","spring","summer","fall")

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

interactions<-read_csv("data/RAW_interactions.csv") |>
  select(-review)|>
  mutate(season=month(date)%%12%/%3) |>
  mutate(season=seasons[season+1])

recipe_by_season<-interactions |>
  group_by(recipe_id,season) |>
    summarise(count=n())|>
  ungroup()

recipes_eveness<-recipe_by_season |>
  group_by(recipe_id) |>
    filter(sum(count)>INTERACTION_COUNT) |>
  pivot_wider(id_cols = c("recipe_id"), names_from = season, values_from = count, values_fill = 0) |> # it is faster to filter and only then pivot for some rason
  rowwise()|>
  mutate(evenness=pielou_evenness(across(all_of(seasons)))) |>
  mutate(total=sum(across(all_of(seasons))),most_common=seasons[max.col(across(all_of(seasons)))])|>
  rename(id="recipe_id")|>
  ungroup()

### QUESTION 1 ###

#assuming complexity is a function of time and steps
minute_quantile<-quantile(recipes$minutes,c(0.1,0.9))
steps_quantile<-quantile(recipes$n_steps,c(0.1,0.9))

filtered<-recipes |>
  filter(between(minutes,minute_quantile[1],minute_quantile[2]),between(n_steps,steps_quantile[1],steps_quantile[2])) |>
  right_join(recipes_eveness)|>
  mutate(most_common=ifelse(evenness>CENTRELIZED_SEASON_EVENNESS,"none",most_common))|>
  mutate(most_common=factor(most_common,levels=c(seasons,"none")))
  
minutes_median<-median(filtered$minutes,na.rm = TRUE)
steps_median<-median(filtered$n_steps,na.rm = TRUE)

filtered |> ggplot()+
  aes(x=n_steps,y=minutes,color=most_common)+
  geom_density2d()+
  geom_hline(yintercept = minutes_median)+
  geom_vline(xintercept = steps_median)+
  facet_wrap(vars(most_common))+
  scale_color_manual(values=c("purple","green","gold2","salmon","olivedrab"))

### QUESTION 2 ###
### Is the rating of recipes that are made year round affected by the time?
all_year<-recipes_eveness |> filter(evenness>CENTRELIZED_SEASON_EVENNESS)
seasonal<-recipes_eveness |> filter(evenness<=CENTRELIZED_SEASON_EVENNESS)
plot_score_evenness<-function(r){
r|>
  left_join(interactions,join_by("id"=="recipe_id"), multiple = "all") |>
  select(id,season,rating) |>
  group_by(id,season) |>
  summarise(rating=mean(rating)) |>
  summarise(ev=pielou_evenness(rating)) |>
  ggplot()+
    aes(x=ev)+
    geom_rect(xmax=CENTRELIZED_SEASON_EVENNESS,xmin=-Inf,ymin=-Inf, ymax=Inf,fill="salmon", alpha = 0.1) +
    geom_rect(xmin=CENTRELIZED_SEASON_EVENNESS,xmax=Inf,ymin=-Inf, ymax=Inf,fill="lightgreen", alpha = 0.1) +
    geom_vline(xintercept = CENTRELIZED_SEASON_EVENNESS)+
    xlab("evenness of the per season mean")+
    geom_histogram()
}
plot_score_evenness(all_year)
plot_score_evenness(seasonal)

N<-100
R<-100
mean_diff <- function(recipe_id, comments) {
  season <- first(recipes_eveness[recipes_eveness$id == recipe_id,]$most_common)
  season_ratings <- sample(comments[comments$season==season,]$rating,R,replace=TRUE)
  non_season_ratings <- sample(comments$rating,R,replace=TRUE)
  mean(season_ratings) - mean(non_season_ratings)
}


ids<-recipes_eveness|>
  group_by(evenness>CENTRELIZED_SEASON_EVENNESS)|>
  select(id)|>
  sample_n(N %/% 2,replace = FALSE)|>
  ungroup()|>
  pull()

# for better performance create a subset of the potential ids
interactions_subset<-interactions|>filter(recipe_id %in% ids)

# bootstrap for each interaction
diffs<-enframe(map(ids,
  ~ replicate(R, {mean_diff(.x, interactions_subset[interactions_subset$recipe_id == .x, ])})
))|>unnest(value)|>
  mutate(id=ids[name])|>
  select(-name)

diffs |>
  group_by(id) |>
  summarise(sd=sd(value),mean=mean(value))|>
  ungroup()|>
  left_join(recipes_eveness) |>
  mutate(even_flag=evenness>CENTRELIZED_SEASON_EVENNESS)|>
  select(id,sd,mean,even_flag) |>
  group_by(even_flag)


## QUESTION 3 ###
# install ggradar for the plot
# devtools::install_github("xl0418/ggradar2",dependencies=TRUE)

library(ggradar2)
technique_per_season<-recipes_eveness |>
  mutate(most_common=ifelse(evenness>CENTRELIZED_SEASON_EVENNESS,"none",most_common))|>
  pivot_longer(all_of(seasons))|>
  right_join(pp_rec) |>
  select(name,value,any_of(techniques_order),most_common) |>
  mutate(across(any_of(techniques_order),~ . * value))|>
  group_by(most_common) |>
  drop_na()|>
  summarise(total=sum(value),across(any_of(techniques_order),sum))|>
  mutate(across(any_of(techniques_order),~ . / total))

technique_per_season |> 
  select(most_common,any_of(techniques_order))|>
  select(most_common,where(~ max(.)>=0.2)) |>
  ggradar2(
   group.point.size = 1,
   group.line.width = 1
  )

hot_techniques<-c('bake','barbecue','boil','braise','broil','caramelize',
                  'crock pot','emulsify','ferment',
                  'fry','griddle','grill','melt','microwave','parboil',
                  'poach','pressure cook','roast','saute','scald',
                  'simmer','skillet','slow cook','smoke','sous-vide','steam','stew',
                  'toast')

pp_rec |>
  mutate(
    hot_techniques = rowSums(across(any_of(hot_techniques))),
    cold_techniques = rowSums(across(any_of(techniques_order))) - hot_techniques
  ) |>
  mutate(hotness_percentage = hot_techniques / (hot_techniques + cold_techniques)) |>
  inner_join(recipes_eveness) |>
  pivot_longer(any_of(techniques_order), names_to = "technique") |>
  filter(value) |>
  select(id,
         technique,
         hotness_percentage,
         hot_techniques,
         cold_techniques) |>
  group_by(id) |>
  summarise(
    t = paste(technique, collapse = ","),
    total_techniques = min(hot_techniques) + min(cold_techniques),
    hotness_percentage = min(hotness_percentage)
  ) |>
  arrange(desc(hotness_percentage), desc(total_techniques)) |> ungroup() |>
  filter(hotness_percentage != 1 | total_techniques != 1) |>
  ggplot() + aes(x = hotness_percentage, fill = hotness_percentage >= 0.5) +
  geom_histogram() +
  scale_fill_manual(values = c("dodgerblue1", "salmon"),
                    labels = c("cold", "hot")) +
  xlab("percentage of hot techniques from total techniques")

interactions |> ggplot() +
  aes(x = interaction(day(date), month(date)),
      fill = factor(season, levels = seasons)) +
  geom_bar(stat = "count", alpha = 0.5) + coord_polar() +
  scale_fill_manual(values = c("purple", "green", "yellow2", "orange1")) +
  scale_x_discrete(breaks = seq(0, 365, by = 73)) +
  scale_y_continuous(breaks = seq(0, 4000, by = 500)) +
  xlab("date") +
  theme(axis.text.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25))
