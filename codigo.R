library(tidyverse, warn.conflicts = FALSE)
library(ggcorrplot)

load(url("https://www.causeweb.org/tshs/datasets/ultrarunning.RData"))

ultrarunning %>% as_tibble()
ultrarunning <- ultrarunning %>% as_tibble() %>% mutate(sex = factor(sex,
  levels=c(1, 2), labels=c("Male", "Female"))) %>% mutate(pb_surface =
  factor(pb_surface,  levels=c(1, 2, 3, 4), labels=c("Trail", "Track", "Road",
  "Mixed"))); ultrarunning

# %>% ggcorrplot(., hc.order = TRUE, type = "lower", colors = c("#FF0000", "#FFFFFF", "#00FF00"))

correlacion <- function(dataframe) {
  dataframe %>% select(c(1,4,6:10)) %>% na.omit(.) %>% cor(.) %>% 
    ggcorrplot(., hc.order = TRUE, type = "lower", 
               colors = c("#FF0000", "#FFFFFF", "#00FF00"))
}

corregir <- function(dataframe, mode) {
  if (mode == "up") {
    dataframe <- dataframe %>% fill(age, .direction = "up") %>%
      fill(pb_elev, .direction = "up") %>% fill(pb100k_dec, .direction = "up") %>%
      fill(avg_km, .direction = "up") %>% fill(teique_sf, .direction = "up") %>%
      fill(steu_b, .direction = "up") %>% fill(stem_b, .direction = "up")
  }
  else{
    if (mode == "down") {
      dataframe <- dataframe %>% fill(age, .direction = "down") %>%
        fill(pb_elev, .direction = "down") %>% fill(pb100k_dec, .direction = "down") %>%
        fill(avg_km, .direction = "down") %>% fill(teique_sf, .direction = "down") %>%
        fill(steu_b, .direction = "down") %>% fill(stem_b, .direction = "down")
    }
    else{
      if (mode == "mean") {
        dataframe <- dataframe %>% 
          replace_na(replace = list(age = round(mean(.$age, na.rm = T),0),
                                    pb_elev = round(mean(.$pb_elev, na.rm = T),0),
                                    pb100k_dec = round(mean(.$pb100k_dec, na.rm = T),2),
                                    avg_km = round(mean(.$avg_km, na.rm = T),0),
                                    teique_sf = round(mean(.$teique_sf, na.rm = T),6),
                                    steu_b = round(mean(.$steu_b, na.rm = T),0),
                                    stem_b = round(mean(.$stem_b, na.rm = T),6)))
      }
    }
  }
}
  
correlacion(ultrarunning)  # Control
corregir(ultrarunning, "mean") %>% select(c(1,4,6:10)) %>% na.omit(.) %>% cor(.)
norm(correlacion(ultrarunning) - correlacion(corregir("mean"))) 
