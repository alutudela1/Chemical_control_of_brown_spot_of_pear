# per fungicide / strain
dat %>%  
  nest(data = c(dose, colony_diameter)) %>% 
  mutate(mod = map(data, ~broom::tidy(drm(colony_diameter~dose, fct = LL.3(), data = .))))%>%
  unnest(c(mod))%>% 
  tibble()

# per fungicide
dat %>%  
  nest(data = c(-fungicide)) %>% 
  mutate(mod = map(data, ~broom::tidy(drm(colony_diameter~dose, fct = LL.3(), data = .))))%>%
  unnest(c(mod)) %>% 
  tibble()
