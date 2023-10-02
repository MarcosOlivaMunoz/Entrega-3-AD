load(url("https://www.causeweb.org/tshs/datasets/ultrarunning.RData"))

ultrarunning <- ultrarunning %>% as_tibble() %>% mutate(sex = factor(sex,
  levels=c(1, 2), labels=c("Male", "Female"))) %>% mutate(pb_surface =
  factor(pb_surface,  levels=c(1, 2, 3, 4), labels=c("Trail", "Track", "Road",
  "Mixed")))
