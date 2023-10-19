library(tidyverse, warn.conflicts = FALSE)
library(ggcorrplot)
library(GGally)

load(url("https://www.causeweb.org/tshs/datasets/ultrarunning.RData"))

ultrarunning %>% as_tibble()
ultrarunning <- ultrarunning %>% as_tibble() %>% mutate(sex = factor(sex,
  levels=c(1, 2), labels=c("Male", "Female"))) %>% mutate(pb_surface =
  factor(pb_surface,  levels=c(1, 2, 3, 4), labels=c("Trail", "Track", "Road",
  "Mixed"))); ultrarunning

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
ult_corregido<-corregir(ultrarunning, "mean") 
#norm(correlacion(ultrarunning) - correlacion(corregir(ultrarunning,"mean"))) 

#================================================



ult_corregido1 <- ult_corregido %>% mutate(teique_sf = ordered(cut(.$teique_sf, 3),labels=c("Bajo","Medio","Alto")))
ult_corregido2 <- ult_corregido %>% mutate(steu_b = ordered(cut(.$steu_b, 3),labels=c("Bajo","Medio","Alto")))
ult_corregido3 <- ult_corregido %>% mutate(stem_b = ordered(cut(.$stem_b, 3),labels=c("Bajo","Medio","Alto")))
ult_corregido_tot <- ult_corregido  %>% mutate(teique_sf = ordered(cut(.$teique_sf, 3),labels=c("Bajo","Medio","Alto"))) %>% mutate(steu_b = ordered(cut(.$steu_b, 3),labels=c("Bajo","Medio","Alto"))) %>% mutate(stem_b = ordered(cut(.$stem_b, 3),labels=c("Bajo","Medio","Alto")))


df1 <- ult_corregido1 %>% filter(teique_sf == "Bajo") %>% select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>% add_row(ult_corregido1 %>% filter(teique_sf == "Medio") %>%
  select(where(is.numeric)) %>% summarise(across(everything(), mean))) %>%add_row(ult_corregido1 %>%
  filter(teique_sf == "Alto") %>% select(where(is.numeric)) %>% summarise(across(everything(), mean)))
df2 <- ult_corregido2 %>% filter(steu_b == "Bajo") %>% select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>% add_row(ult_corregido2 %>% filter(steu_b == "Medio") %>%
  select(where(is.numeric)) %>% summarise(across(everything(), mean))) %>%add_row(ult_corregido2 %>%
  filter(steu_b == "Alto") %>% select(where(is.numeric)) %>% summarise(across(everything(), mean)))
df3 <- ult_corregido3 %>% filter(stem_b == "Bajo") %>% select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>% add_row(ult_corregido3 %>% filter(stem_b == "Medio") %>%
  select(where(is.numeric)) %>% summarise(across(everything(), mean))) %>%add_row(ult_corregido3 %>%
  filter(stem_b == "Alto") %>% select(where(is.numeric)) %>% summarise(across(everything(), mean)))
