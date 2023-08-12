### installations
# install.packages(reticulate)
# library(reticulate)
# reticulate::install_miniconda()
# then in terminal:
# conda install pandas

library(reticulate)
library(tidyverse)
library(glue)
#setup environment
set.seed(3)
use_miniconda("~/.local/share/r-miniconda")
pd<-import("pandas")

#credit to camille from here https://stackoverflow.com/questions/68581643/how-to-convert-a-string-representation-of-a-list-to-a-list-with-the-tidyverse
parse_list <- function(str){
  unlist(strsplit(gsub("[\\[\\]']", "", str, perl = TRUE), ", "))
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

#load data
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

recipe_ingredients<-pp_rec |>
group_by(id) |>
  reframe(recipe_id = id, ingredient_id = as.integer(parse_list(ingredient_ids)))

# find recipes that are mainly popular in a specific season
INTERACTION_COUNT <- 24
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

rm(recipe_by_season)
### QUESTION 1 ####

error<-interactions |>
  mutate(day=day(date),month=month(date))|>
  group_by(season,month,day)|>
  summarise(n=n())|>
  ungroup()|>
  group_by(season)|>
  summarise(sd=sd(n),mean=mean(n))|>
  ungroup()

  
print(error)

interactions|>left_join(error,by="season")|>
  ggplot() +
  aes(x = interaction(day(date),month(date)),
      fill = factor(season, levels = seasons)) +
  geom_bar(stat = "count", alpha = 0.5) +
  coord_polar()+
  scale_fill_manual(values = c("purple", "green", "yellow2", "orange1")) +
  scale_x_discrete(breaks = seq(0, 365, by = 73)) +
  scale_y_continuous(breaks = seq(0, 4000, by = 500)) +
  xlab("date") +
  ylab("comments count")+
  labs(title = "Activity By Date",fill="season")+
  theme(axis.text.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25))


### QUESTION 2 ###
### Is the rating of recipes that are made year round affected by the time?

rating_evenness <- recipes_eveness |>
  mutate(seasonal = ifelse(evenness <= CENTRELIZED_SEASON_EVENNESS,"seasonal","all-year")) |>
  left_join(interactions, join_by("id" == "recipe_id"), multiple = "all") |>
  select(id, season, rating, seasonal) |>
  group_by(id, season) |>
  summarise(rating = mean(rating), seasonal = first(seasonal)) |>
  summarise(ev = pielou_evenness(rating), seasonal = first(seasonal)) |>
  filter(!is.na(ev))


confidence <- rating_evenness |>
  group_by(seasonal)|>
  arrange(ev)|>
  summarise(quantile80=ev[n()*0.2])

print(confidence)

rating_evenness |> ggplot() +
  aes(x = ev) +
  geom_rect(
    xmax = CENTRELIZED_SEASON_EVENNESS,
    xmin = -Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = "salmon",
    alpha = 0.1
  ) +
  geom_rect(
    xmin = CENTRELIZED_SEASON_EVENNESS,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = "lightgreen",
    alpha = 0.1
  ) +
  facet_wrap(vars(seasonal), ) +
  xlab("evenness of the per season mean") +
  ylab("count")+
  labs(title="Distribution Of The Score Evenness Across Seasonal and Yearly Recipes")+
  annotate("text",label="uneven",x=0.25,y=4000)+
  annotate("text",label="even",x=0.85,y=4000)+
  geom_histogram(fill="white")+
  geom_vline(data=confidence,aes(xintercept = quantile80),linetype="dashed")

# rm(confidence,rating_evenness)

# 2.2
N<-90
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

recipe_diffs<-diffs |>
  group_by(id) |>
  summarise(sd=sd(value),mean=mean(value))|>
  ungroup()|>
  left_join(recipes_eveness) |>
  mutate(seasonal=ifelse(evenness>CENTRELIZED_SEASON_EVENNESS,"all-year","seasonal"))|>
  select(id,sd,mean,seasonal)

diffs_stats<-recipe_diffs|>
  group_by(seasonal)|>summarise(E=round(mean(mean),4),SD=round(sd(mean),4))|>
  mutate(label=glue("E({seasonal})={E} SD({seasonal})={SD})"))

confidence<-recipe_diffs|>
  group_by(seasonal)|>
  reframe(enframe(quantile(mean,probs=c(0.025,0.975)))) |>
  ungroup()|>
  pivot_wider()
print(confidence)

recipe_diffs|> ggplot()+
  aes(x=mean,color=seasonal)+
  geom_density()+
  labs(title="Density Of E(most common season)-E(yearly)",
       caption=glue_collapse(diffs_stats$label,sep="\n"))+
  xlab("E(most common season)-E(yearly)")
print(diffs_stats$label)

### QUESTION 3 ###

#assuming complexity is a function of time and steps
library(infotheo)
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
  aes(x=n_steps,y=minutes)+
  facet_wrap(vars(most_common))+
  stat_density2d_filled(geom="polygon",contour_var = "ndensity",aes(fill=..level..))+
  geom_hline(yintercept = minutes_median)+
  geom_vline(xintercept = steps_median)+
  labs(title="Complexity Heatmap By Season",fill="Density")

ratings <- interactions|>
  group_by(recipe_id)|>
  summarise(rating=mean(rating),popularity=n())

filtered<-filtered|>
  mutate(complexity=minutes/max(minutes,na.rm = TRUE)+n_steps/max(n_steps,na.rm=TRUE))|>
  select(id,complexity,most_common)|>
  inner_join(ratings,join_by("id"=="recipe_id"))|>
  group_by(most_common) 

correlation<-filtered |>
  filter(!is.na(complexity),!is.na(rating))|>
  summarise(R2=cor(complexity,rating)^2,MI=mutinformation(discretize(complexity),discretize(rating)))|>
  ungroup()

print(correlation)
correlation<-correlation|>
  mutate(complexity=max(filtered$complexity,na.rm = TRUE)*0.8,# position in bottom right
         popularity=max(filtered$popularity,na.rm = TRUE)*0.8,
         rating= 4,
         label=glue("MI: {round(MI,4)}"))

filtered |>
  ggplot()+
  aes(x=complexity, y=rating,color=most_common)+
  geom_density2d()+
  facet_wrap(vars(most_common))+
  geom_text(data=correlation,aes(label=label))+
  scale_color_manual(values=c("purple","green","gold2","salmon","olivedrab"))+
  labs(title="Density of Rating By Complexity")

# rm(filtered,correlation,ratings,minute_quantile,steps_quantile,minutes_median,steps_median)

## QUESTION 4 ###
# install ggradar for the plot
# devtools::install_github("ricardo-bion/ggradar"",dependencies=TRUE)
library(ggradar)

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
  rename(chill="refrigerate",mix="combine")|>
  select(most_common,where(~ max(.)>=0.2)) |> # The number 0.2 may look like a random magic number. This is because it is, and has no actual meaning
  ggradar(
   group.point.size = 1,
   group.line.width = 1,
   font.radar = "mono",
   base.size = 12,
   plot.legend = TRUE,
   legend.position = "bottom"
   )+  labs(title="Technique Usage")

hot_techniques<-c('bake','barbecue','boil','braise','broil','caramelize',
                  'crock pot','emulsify','ferment',
                  'fry','griddle','grill','melt','microwave','parboil',
                  'poach','pressure cook','roast','saute','scald',
                  'simmer','skillet','slow cook','smoke','sous-vide','steam','stew',
                  'toast')

minimal<-technique_per_season|>
  select(most_common,any_of(hot_techniques))|>
  pivot_longer(any_of(hot_techniques))|>
  ungroup()|>
  group_by(name)|>
  filter(value==min(value),max(value)>0.05)|>
  ungroup()

technique_per_season |> 
  select(most_common,any_of(hot_techniques))|>
  pivot_longer(any_of(hot_techniques))|>
  group_by(name)|>filter(max(value)>0.05)|> ungroup() |>
  ggplot(aes(x = most_common, y = name, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_point(data=minimal,aes(color="black"))+
  scale_color_identity()+
  xlab("season")+
  ylab("technique")+
  labs(title = "Hot Techniques By Season",fill="% of recipes",caption = "each dot marks the lowest tile for the technique") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Question 5 ###
# find the 20 most common ingredients 
common_ingredient <- recipe_ingredients |>
  select(ingredient_id) |>
  group_by(ingredient_id) |>
  summarise(count = n()) |>
  arrange(desc(count))|> 
  ungroup()|> slice(1:20) 
#find the recipes which has these ingredients
recipe_ingredients|>
  filter(ingredient_id %in%  common_ingredient$ingredient_id) |>
  # amalgamate these recipe's season
  inner_join(recipes_eveness,join_by('recipe_id' == 'id')) |>
  group_by(recipe_id,most_common)|>
  summarise(season_count = n())|>
  arrange(desc(season_count))|>
  group_by(most_common)|> 
  summarise(season_2 = n())|>
  arrange(desc(season_2))|>
  rename(season = "most_common")|>
  ggplot()+
  aes(x = season, y = season_2, fill = season)+
  geom_bar(stat = 'identity', alpha = 0.8)+
  scale_fill_manual(values = c('salmon','yellowgreen', 'gold', 'darkturquoise' ))+
  xlab("seasons") +
  ylab("recipe wich includes common ingredients")+
  labs(title = "Sum Recipes Contain 20 Common ingredients")
