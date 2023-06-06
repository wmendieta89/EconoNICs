

rem2gdp <- readxl::read_xls("data_world_bank.xls", sheet = "remittances") %>% 
  pivot_longer(-c(1:4), values_to = "rem2gdp") %>% 
  janitor::clean_names() %>% 
  mutate(name = as.numeric(name))

gdp <- readxl::read_xls("data_world_bank.xls", sheet = "gdp") %>% 
  pivot_longer(-c(1:4), values_to = "gdp") %>% 
  janitor::clean_names() %>% 
  mutate(name = as.numeric(name))

roll_sd <- timetk::slidify(.f = sd, 
                           .period = 5, 
                           .align = "right")



consumption <- readxl::read_xls("data_world_bank.xls", sheet = "consumption") %>% 
  pivot_longer(-c(1:4), values_to = "consumption") %>% 
  janitor::clean_names() %>% 
  mutate(name = as.numeric(name)) %>% 
  tsibble::as_tsibble(index = name, key = country_name) %>%
  mutate(consumption = consumption / lag(consumption) - 1,
         consumption_sd = roll_sd(consumption)) %>% 
  drop_na()


data <- rem2gdp %>% 
  left_join(gdp, by = c("country_name", "country_code", "income_group", "region", "name")) %>% 
  drop_na(rem2gdp, gdp) %>%
  group_by(country_name) %>% 
  summarise(country_name = country_name, 
            region = region, 
            income_group = income_group, 
            rem2gdp = mean(rem2gdp), 
            gdp = mean(gdp)) %>% 
  distinct()


gr7 <- data %>% 
  filter(income_group != 0,
         str_detect(region, "Latin America")) %>% 
  ggplot(aes(rem2gdp, gdp, label = country_name))+
  geom_point(color = "steelblue", size = 3, alpha = 0.75)+
  geom_smooth(method = "lm", se = F)+
  theme_bw()+
  labs(title = "Latinoamérica: Correlación entre remesas/PIB y crecimiento económico")
#facet_wrap(~ income_group, scales = "free")

ggplotly(gr7)

data2 <- rem2gdp %>% 
  left_join(consumption, by = c("country_name", "country_code", "income_group", "region", "name")) %>% 
  drop_na(rem2gdp, consumption, consumption_sd) %>% 
  group_by(country_name) %>% 
  summarise(country_name = country_name, 
            region = region, 
            income_group = income_group, 
            rem2gdp = mean(rem2gdp), 
            consumption_sd = mean(consumption_sd)) %>% 
  distinct()


gr8 <- data2 %>% 
  filter(#name == 2021, 
         income_group != 0,
         str_detect(region, "Latin America")) %>% 
  ggplot(aes(rem2gdp, consumption_sd, label = country_name))+
  geom_point(color = "steelblue", size = 3, alpha = 0.75)+
  geom_smooth(method = "lm", se = F)+
  theme_bw()
  #facet_wrap(~ income_group, scales = "free")

ggplotly(gr8)


