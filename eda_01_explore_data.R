library(tidyverse)
library(brms)
library(tidybayes)
library(scales)
library(patchwork)
library(broom.mixed)
library(rstanarm)
library(ggtext)
options(mc.cores = parallel::detectCores())

source('data_03_major_cities_proximity.R')

# Plot stuff
clrs <- rev(MetBrewer::met.brewer("Ingres", length(unique(combined_data$degree_of_endangerment))))
theme_set(theme_bw())


theme_nice <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(family = "Jost", face = "bold"),
          axis.title = element_text(family = "Jost Medium"),
          strip.text = element_text(family = "Jost", face = "bold",
                                    size = rel(1), hjust = 0),
          strip.background = element_rect(fill = NA, color = NA))
}


p1 <- combined_data |> 
  count(degree_of_endangerment_factor) |> 
  mutate(prop = n / sum(n),
         prop_nice = label_percent(accuracy = 0.1)(prop)) |> 
  ggplot(aes(x = degree_of_endangerment_factor, y = n,
             fill = degree_of_endangerment_factor)) +
  geom_col(fill = clrs) +
  geom_text(aes(y = 50, label = prop_nice), color = "white", size = 2.5, 
            angle = 90, hjust = 0) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Degree of Endangerment", y = "Count") +
  theme(plot.title = element_text(size = rel(1), hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
p1


p2 <- combined_data |> 
  count(degree_of_endangerment_factor) |> 
  mutate(pr_k = n / sum(n),
         cum_pr_k = cumsum(pr_k)) |> 
  ggplot(aes(x = degree_of_endangerment_factor, y = cum_pr_k)) +
  geom_line(aes(group = 0), color = clrs, size = 1) +
  geom_point(shape = 21, fill = clrs, color = "white", size = 5, stroke = 1) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "Degree of Endangerment", y = "Cumulative proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2

p1 | p2

p3 <- combined_data |> 
  count(degree_of_endangerment_factor) |> 
  mutate(pr_k = n / sum(n),
         cum_pr_k = cumsum(pr_k)) |> 
  mutate(alpha_k = qlogis(cum_pr_k)) |> 
  ggplot(aes(x = degree_of_endangerment_factor, y = alpha_k)) +
  geom_line(aes(group = 0), size = 1, color = clrs) +
  geom_point(shape = 21, fill = clrs, color = "white", size = 5, stroke = 1) +
  labs(x = "Degree of Endangerment", y = "Log cumulative odds")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3


p1 | p2 | p3



combined_data |> 
  dplyr::select(degree_of_endangerment_factor, literacy) |>
  drop_na() |>
  mutate(
    group = case_when(
      literacy >= quantile(literacy, 0.9) ~ "Top 10%",
      literacy <= quantile(literacy, 0.1) ~ "Bottom 10%",
      TRUE ~ "Middle 80%"
    ),
    
    group = factor(group,
                   levels = c("Top 10%", "Middle 80%", "Bottom 10%"),
                   ordered = T)
  ) |>
  group_by(degree_of_endangerment_factor, group) |>
  summarise(count = n()) |>
  group_by(group) |>
  mutate(prop = count / sum(count),
         prop_nice = label_percent(accuracy = 0.1)(prop)) |>
  ggplot(aes(x = degree_of_endangerment_factor, 
             fill = group, 
             y = count)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
  geom_text(aes(label = prop_nice),
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("#FFDE00", "#ABBBC4","#009DDC")) +
  labs(x = "Degree of Endangerment", fill = "Countries\n(By Literacy Rate)",
       title = "Degree of Endangerment:", 
       y = "Count",
       subtitle = "Most Literate vs. Least Literate Countries") +
  theme_nice() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 




l2 <- combined_data |> 
  filter(minority_ed_policy == 1) |>
  dplyr::select(degree_of_endangerment_factor, literacy) |>
  drop_na() |>
  mutate(
    group = case_when(
      literacy >= quantile(literacy, 0.9) ~ "Top 10%",
      literacy <= quantile(literacy, 0.1) ~ "Bottom 10%",
      TRUE ~ "Middle 80%"
    ),
    
    group = factor(group,
                   levels = c("Top 10%", "Middle 80%", "Bottom 10%"),
                   ordered = T)
  ) |>
  group_by(degree_of_endangerment_factor, group) |>
  summarise(count = n()) |>
  group_by(group) |>
  mutate(prop = count / sum(count),
         prop_nice = label_percent(accuracy = 0.1)(prop)) |>
  ggplot(aes(x = degree_of_endangerment_factor, 
             fill = group, 
             y = count)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
  geom_text(aes(label = prop_nice),
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("#FFDE00", "#ABBBC4","#009DDC")) +
  labs(x = "Degree of Endangerment", fill = "Countries\n(By Literacy Rate)",
       subtitle = "<b>Countries <span style = 'color:blue;'>Without</span> Minority Ed Policy</b>", 
       y = "Count") +
  theme_nice() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_markdown(size = 12)) 



l1 <- combined_data |> 
  filter(minority_ed_policy == 0) |>
  dplyr::select(degree_of_endangerment_factor, literacy) |>
  drop_na() |>
  mutate(
    group = case_when(
      literacy >= quantile(literacy, 0.9) ~ "Top 10%",
      literacy <= quantile(literacy, 0.1) ~ "Bottom 10%",
      TRUE ~ "Middle 80%"
    ),
    
    group = factor(group,
                   levels = c("Top 10%", "Middle 80%", "Bottom 10%"),
                   ordered = T)
  ) |>
  group_by(degree_of_endangerment_factor, group) |>
  summarise(count = n()) |>
  group_by(group) |>
  mutate(prop = count / sum(count),
         prop_nice = label_percent(accuracy = 0.1)(prop)) |>
  ggplot(aes(x = degree_of_endangerment_factor, 
             fill = group, 
             y = count)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
  geom_text(aes(label = prop_nice),
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("#FFDE00", "#ABBBC4","#009DDC")) +
  labs(x = "Degree of Endangerment", fill = "Countries\n(By Literacy Rate)",
       subtitle = "<b>Countries <span style = 'color:blue;'>With</span> Minority Ed Policy</b>",
       y = "Count") +
  ylim(ggplot_build(l2)$layout$panel_scales_y[[1]]$range$range) +
  theme_nice() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_markdown(size=12)) 


l1 + l2 + 
  plot_annotation(title = "Language Endangerment: Most Literate vs. Least Literate Countries") +
  plot_layout(guides = "collect")


combined_data |> 
  dplyr::select(degree_of_endangerment_factor, infant_mortality) |>
  drop_na() |>
  mutate(
    group = case_when(
      infant_mortality >= quantile(infant_mortality, 0.9) ~ "Top 10%",
      infant_mortality <= quantile(infant_mortality, 0.1) ~ "Bottom 10%",
      TRUE ~ "Middle 80%"
    ),
    
    group = factor(group,
                   levels = c("Top 10%", "Middle 80%", "Bottom 10%"),
                   ordered = T)
  ) |>
  group_by(degree_of_endangerment_factor, group) |>
  summarise(count = n()) |>
  group_by(group) |>
  mutate(prop = count / sum(count),
         prop_nice = label_percent(accuracy = 0.1)(prop)) |>
  ggplot(aes(x = degree_of_endangerment_factor, 
             fill = group, 
             y = count)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
  geom_text(aes(label = prop_nice),
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("#FFDE00", "#ABBBC4","#009DDC")) +
  labs(x = "Degree of Endangerment", 
       fill = "Countries\n(By Infant Mortality Rate)",
       title = "Degree of Endangerment:", 
       subtitle = "Most Fatal vs. Least Fatal for Infants") +
  theme_nice() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



i2 <- combined_data |> 
  filter(minority_ed_policy == 1) |>
  dplyr::select(degree_of_endangerment_factor, infant_mortality) |>
  drop_na() |>
  mutate(
    group = case_when(
      infant_mortality >= quantile(infant_mortality, 0.9) ~ "Top 10%",
      infant_mortality <= quantile(infant_mortality, 0.1) ~ "Bottom 10%",
      TRUE ~ "Middle 80%"
    ),
    
    group = factor(group,
                   levels = c("Top 10%", "Middle 80%", "Bottom 10%"),
                   ordered = T)
  ) |>
  group_by(degree_of_endangerment_factor, group) |>
  summarise(count = n()) |>
  group_by(group) |>
  mutate(prop = count / sum(count),
         prop_nice = label_percent(accuracy = 0.1)(prop)) |>
  ggplot(aes(x = degree_of_endangerment_factor, 
             fill = group, 
             y = count)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
  geom_text(aes(label = prop_nice),
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("#FFDE00", "#ABBBC4","#009DDC")) +
  labs(x = "Degree of Endangerment", fill = "Countries\n(By Infant Mortality Rate)",
       subtitle = "<b>Countries <span style = 'color:blue;'>Without</span> Minority Ed Policy</b>", 
       y = "Count") +
  theme_nice() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_markdown(size = 12)) 



i1 <- combined_data |> 
  filter(minority_ed_policy == 0) |>
  dplyr::select(degree_of_endangerment_factor, infant_mortality) |>
  drop_na() |>
  mutate(
    group = case_when(
      infant_mortality >= quantile(infant_mortality, 0.9) ~ "Top 10%",
      infant_mortality <= quantile(infant_mortality, 0.1) ~ "Bottom 10%",
      TRUE ~ "Middle 80%"
    ),
    
    group = factor(group,
                   levels = c("Top 10%", "Middle 80%", "Bottom 10%"),
                   ordered = T)
  ) |>
  group_by(degree_of_endangerment_factor, group) |>
  summarise(count = n()) |>
  group_by(group) |>
  mutate(prop = count / sum(count),
         prop_nice = label_percent(accuracy = 0.1)(prop)) |>
  ggplot(aes(x = degree_of_endangerment_factor, 
             fill = group, 
             y = count)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
  geom_text(aes(label = prop_nice),
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("#FFDE00", "#ABBBC4","#009DDC")) +
  labs(x = "Degree of Endangerment", fill = "Countries\n(By Infant Mortality Rate)",
       subtitle = "<b>Countries <span style = 'color:blue;'>With</span> Minority Ed Policy</b>",
       y = "Count") +
  ylim(ggplot_build(i2)$layout$panel_scales_y[[1]]$range$range) +
  theme_nice() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_markdown(size=12)) 


i1 + i2 + 
  plot_annotation(title = "Language Endangerment: Most Fatal vs. Least Fatal Countries for Infants") +
  plot_layout(guides = "collect") 



getMapGepProp<-function(){
  list(
    showframe = T,
    showocean = T,
    #oceancolor = 'rgb(28,107,160)',
    oceancolor = 'rgb(222,243,246)',
    projection = list(
      type = 'orthographic',
      rotation = list(
        lon = 60,
        lat = 10)
    ),
    lonaxis =  list(
      showgrid = F,
      gridcolor = 'rgb(102, 102, 102)'
    ),
    lataxis = list(
      showgrid = F,
      gridcolor = 'rgb(102, 102, 102)'
    )
  )
}

