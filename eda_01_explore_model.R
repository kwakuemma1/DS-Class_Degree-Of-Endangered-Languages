library(tidyverse)
library(brms)
library(tidybayes)
library(scales)
library(patchwork)
library(broom.mixed)
library(rstanarm)



library(DiagrammeR)
options(mc.cores = parallel::detectCores())

# source('data_03_major_cities_proximity.R')

# Plot stuff
clrs <- rev(MetBrewer::met.brewer("Ingres", length(unique(combined_data$degree_of_endangerment))))
theme_set(theme_bw())



p1 <- combined_data |> 
  count(degree_of_endangerment_factor) |> 
  mutate(prop = n / sum(n),
         prop_nice = label_percent(accuracy = 0.1)(prop)) |> 
  ggplot(aes(x = degree_of_endangerment_factor, y = n, fill = degree_of_endangerment_factor)) +
  geom_col(fill = clrs) +
  geom_text(aes(y = 50, label = prop_nice), color = "white", size = 2.5, 
            angle = 90, hjust = 0) +
  scale_y_continuous(labels = label_comma()) +
  # scale_fill_viridis_d(option = "rocket", end = 0.85, guide = "none") +
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

p1|p2



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

library(echarts4r)
  
  
combined_data |>
  dplyr::select(degree_of_endangerment_factor, literacy) |>
  mutate(id = 1:dplyr::n()) |>
  pivot_wider(id_cols=id, names_from = degree_of_endangerment_factor, values_from = literacy) |>
  janitor::clean_names() |>
  e_charts() |>
  e_boxplot(vulnerable, outliers = FALSE) |>
  e_boxplot(definitely_endangered, outliers = FALSE) |>
  e_boxplot(severely_endangered, outliers = FALSE) |>
  e_boxplot(critically_endangered, outliers = FALSE) |>
  e_boxplot(extinct, outliers = FALSE) |>
  # e_color(color = clrs) |>
  e_x_axis(label = list(rotate = 45)) |>
  e_tooltip(
      formatter = e_tooltip_pointer_formatter("currency"),
      axisPointer = list(
        type = "cross"
      )
    )


ggplot(combined_data) +
  aes(x = degree_of_endangerment_factor, y = literacy) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()


the_levels_of_endangerment = c("vulnerable", 
                               "definitely endangered",
                               "severely endangered",
                               "critically endangered",
                               "extinct")

model_formula_factor <- as.formula(
  paste0(
    'degree_of_endangerment_factor ~ ', 
    paste0(covariates, collapse = '+')
  )
)

model_formula_factor

data_mod <- combined_data |>
  dplyr::select(any_of(c(covariates, 'degree_of_endangerment_factor'))) |>
  drop_na() |> as.data.frame()




# Predicting the PROBABILITY of endangerment of each of the degrees of endangerment
model_factor <- stan_polr(data = data_mod, 
                          formula = model_formula_factor, 
                          method = "logistic",
                          prior = R2(0.25),
                          refresh = 0,
                          seed = 9)

model_factor |> 
  add_predicted_draws(newdata = data_mod |> sample_n(1)) |> ungroup() |>
  mutate(
    .prediction = factor(.prediction,
                         levels = the_levels_of_endangerment,
                         ordered = TRUE)
  ) |> 
  count(.prediction) |> 
  mutate(prop = n / sum(n),
         prop_nice = label_percent(accuracy = 0.1)(prop))


simulated_conditions <- tribble(
  ~newdata,
  data_mod |> sample_n(1),
  data_mod |> sample_n(1),
  data_mod |> sample_n(1),
  data_mod |> sample_n(1),
  data_mod |> sample_n(1),
  data_mod |> sample_n(1)
) |> 
  mutate(
    title = map_chr(
      newdata,
      ~{paste0('Actual: ', .x$degree_of_endangerment_factor)}
    ),
    newdata = map(
      newdata,
      ~{.x  |> dplyr::select(-degree_of_endangerment_factor)}
    )
  ) |>
  mutate(pred_plot = map2(newdata, title, ~{
    model_factor |> 
      add_predicted_draws(newdata = .x) |> 
      ungroup() |> 
      mutate(
        .prediction = factor(.prediction,
                             levels = the_levels_of_endangerment,
                             ordered = TRUE)
      ) |> 
      count(.prediction) |> 
      mutate(prop = n / sum(n),
             prop_nice = label_percent(accuracy = 0.1)(prop)) |> 
      ggplot(aes(x = .prediction, y = n)) +
      geom_col(aes(fill = .prediction)) +
      geom_text(aes(y = 50, label = prop_nice), color = "white", size = 2.5, 
                angle = 90, hjust = 0) +
      scale_y_continuous(labels = label_comma()) +
      scale_fill_viridis_d(option = "rocket", end = 0.85, guide = "none") +
      labs(x = NULL, y = "Count", 
           title = .y) +
      theme(plot.title = element_text(size = rel(1), hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1))
  }))

wrap_plots(simulated_conditions$pred_plot, nrow = 2, byrow = FALSE)


model_factor |> 
  gather_draws(literacy, infant_mortality, 
               agriculture, proximity_to_capital_city, 
               minority_ed_policy, urban_pop_pc_change_1960_2017) |> 
  ggplot(aes(x = .value, fill = .variable)) +
  stat_halfeye(normalize = "xy") +
  scale_fill_viridis_d(option = "rocket", end = 0.85, guide = "none") +
  facet_wrap(vars(.variable), scales = "free_x")



model_factor_intercept <- stan_polr(data = data_mod |> mutate(intercept = 1), 
                          formula = degree_of_endangerment_factor ~ intercept, 
                          method = "logistic",
                          prior = R2(0.25),
                          refresh = 0,
                          seed = 9)


model_int_only <- brm(
  bf(degree_of_endangerment_factor ~ 1),
  data = data_mod,
  family = cumulative(link = "logit"),
  # prior = priors,
  # init = rep(list(inits), 4),
  save_warmup = TRUE,
  chains = 4, iter = 2000, seed = 9, cores = 4,
  backend = "cmdstanr", refresh = 0
)


model_int_only |> 
  gather_draws(`^b_Intercept.*`, regex = TRUE) |> 
  ggplot(aes(x = .value, fill = .variable)) +
  stat_halfeye(normalize = "xy") +
  scale_fill_viridis_d(option = "rocket", end = 0.85, guide = "none") +
  facet_wrap(vars(.variable), scales = "free_x")


model_int_only |> 
  gather_draws(`^b_Intercept.*`, regex = TRUE) |> 
  mutate(.value = plogis(.value)) |> 
  ggplot(aes(x = .value, fill = .variable)) +
  stat_halfeye(normalize = "xy") +
  scale_fill_viridis_d(option = "rocket", end = 0.85, guide = "none") +
  scale_x_continuous(labels = label_percent()) +
  facet_wrap(vars(.variable), scales = "free_x")


prob_k <- model_factor |> 
  tidy(effects = "fixed") |> 
  pull(estimate) |> 
  rethinking::dordlogit(7:10, 0, a = _)
prob_k

