# Necessary Package ---------

# install.packages("tidyverse")
library(tidyverse)


# Pipe Example -------------

c(1,2,3) %>% 
  mean %>% 
  "+"(3) %>% 
  "/"(2) -> w

w

# Data Pipeline Example 1 -------

"https://github.com/higgi13425/medicaldata/raw/master/data-raw/messy_data/messy_cirrhosis.xlsx" %>% {
  download.file(
    url = .,
    destfile = paste0("./",basename(.)),
    mode = "wb")
}


messy_cirrhosis <- readxl::read_xlsx(path = "messy_cirrhosis.xlsx", col_names = F)

messy_cirrhosis

messy_cirrhosis %>% 
  slice(-(1:3)) %>%
  "["(,-3) %>% 
  set_names(c("cirrho_type", "sex", paste0("a",rep(1:2,each = 2),".s",c("n","y")))) %>% 
  mutate(cirrho_type = if_else(is.na(cirrho_type), lag(cirrho_type), cirrho_type)) %>% 
  mutate(across(a1.sn:a2.sy, ~as.integer(.x))) %>% 
  pivot_longer(names_to = "AS", values_to = "count", cols = a1.sn:a2.sy) -> xx


messy_cirrhosis %>% 
  slice(-(1:3)) %>%
  "["(,-3) %>% 
  set_names(c("cirrho_type", "sex", paste0("a",rep(1:2,each = 2),".s",c("n","y")))) %>% 
  mutate(cirrho_type = if_else(is.na(cirrho_type), lag(cirrho_type), cirrho_type)) %>% 
  mutate(across(a1.sn:a2.sy, ~as.integer(.x))) %>% 
  pivot_longer(names_to = "AS", values_to = "count", cols = a1.sn:a2.sy) %>% 
  separate(AS, c("age","surv_id")) %>% 
  mutate(
    age = factor(age, levels = c("a1","a2"), labels = c("Under 18","Adult")),
    surv_id = factor(surv_id, levels = c("sn","sy"), labels = c("No","Yes"))
  )  -> xx

messy_cirrhosis %>% 
  slice(-(1:3)) %>%
  "["(,-3) %>% 
  set_names(c("cirrho_type", "sex", paste0("a",rep(1:2,each = 2),".s",c("n","y")))) %>% 
  mutate(cirrho_type = if_else(is.na(cirrho_type), lag(cirrho_type), cirrho_type)) %>% 
  mutate(across(a1.sn:a2.sy, ~as.integer(.x))) %>% 
  pivot_longer(names_to = "AS", values_to = "count", cols = a1.sn:a2.sy) %>% 
  separate(AS, c("age","surv_id")) %>% 
  mutate(
    age = factor(age, levels = c("a1","a2"), labels = c("Under 18","Adult")),
    surv_id = factor(surv_id, levels = c("sn","sy"), labels = c("No","Yes"))
    ) %>% 
  write_csv("./tidy_cirrhosis.csv", col_names = T)
  
  
  

  # select_if(.predicate =  ~is.na(.x) %>% all)


cirr_dt <- read.csv("tidy_cirrhosis.csv", header = T)
str(cirr_dt)

cirr_dt %>% 
  mutate(sex = factor(sex, levels = c("Male","Female")),
         age = factor(age, 
                      levels = c("Under 18","Adult"), 
                      ordered = T),
         surv_id = factor(surv_id, levels = c("Yes","No"))) %>% 
  group_by(cirrho_type,surv_id) %>% 
  summarise(ave_count = sum(count)) %>% 
  write.csv("cir_surid_counts.csv")











