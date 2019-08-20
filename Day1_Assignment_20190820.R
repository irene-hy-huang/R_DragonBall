Bath <- train0 %>% select(Id, FullBath, HalfBath) %>% 
  left_join(Bsmt, by = "Id") %>% 
  mutate(BuildingBath = FullBath + HalfBath) %>% 
  mutate(Totalbath = BsmtBath + BuildingBath)
