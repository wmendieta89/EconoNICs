# Análisis del sector Real

pacman::p_load(tidyverse, readxl, plotly, janitor, lubridate)

# Data preprocessing ----

pib_nominal_produccion_tbl <- read_excel("pib_trimestral.xlsx", sheet = "Produccion") %>% 
  slice(1:31)

pib_real_produccion_tbl <- read_excel("pib_trimestral.xlsx", sheet = "Produccion") %>% 
  slice(32:65)

# cleaning function 
clean_production <- function(x){
  x %>%
    row_to_names(row_number = 4) %>% 
    clean_names() %>%
    pivot_longer(-actividad) %>%
    mutate(value = as.numeric(value)) %>% 
    drop_na(value) %>% 
    group_by(actividad) %>% 
    mutate(fecha = seq.Date(from = as.Date("2006-03-31"), 
                            by = "quarter", 
                            length.out = n())) %>% 
    select(fecha, actividad, value) %>% 
    mutate(yy = year(fecha)) %>% 
    group_by(yy, actividad) %>% 
    summarise(fecha = max(fecha),
              actividad = actividad, 
              value = sum(value, na.rm = T)) %>% 
    distinct() %>% 
    ungroup()
    
}

real_produccion <- pib_real_produccion_tbl %>%
  clean_production() %>% 
  rename(real = value) %>% 
  select(-yy)

nominal_produccion <- pib_nominal_produccion_tbl %>% 
  clean_production() %>% 
  rename(nominal = value) %>% 
  select(-yy)

estructura <- nominal_produccion %>% 
  pivot_wider(names_from = actividad, values_from = nominal) %>% 
  mutate(across(-c(fecha), ~ ./`Producto interno bruto`)) %>% 
  pivot_longer(-c(fecha), names_to = "actividad", values_to = "estructura")

crecimiento <- real_produccion %>% 
  tsibble::as_tsibble(index = fecha, key = actividad) %>% 
  group_by(actividad) %>% 
  mutate(crecimiento = real / lag(real, 1)-1) %>% 
  ungroup() %>% 
  select(-real) %>% 
  as_tibble()


datos <- real_produccion %>% 
  left_join(nominal_produccion, by = c("actividad", "fecha")) %>% 
  left_join(estructura        , by = c("actividad", "fecha")) %>% 
  left_join(crecimiento       , by = c("actividad", "fecha"))


pib_produccion_tbl <- datos %>% 
  tsibble::as_tsibble(index = fecha, key = actividad) %>% 
  mutate(factor = lag(estructura,1),
         contribucion = factor * crecimiento,
         deflactor = nominal / real * 100,
         inflacion_deflactor = deflactor / lag(deflactor, 1)-1, 
         yy = year(fecha)) %>% 
  as_tibble() 

# Analisis: PIB real ----
graficos_pib <- pib_produccion_tbl %>% 
  filter(actividad == "Producto interno bruto") %>% 
  select(fecha, real, crecimiento)

p1 <- graficos_pib %>%
  mutate(PIB = log(real)) %>% 
  ggplot(aes(x = fecha, y = PIB))+
  geom_line(colour = "steelblue")+
  geom_smooth(se = F, colour = "red", linetype = "dotted", linewidth = 0.5, show.legend = T)+
  theme_bw()+
  labs(title = "PIB Real de Nicaragua", x="", y="Logaritmo del PIB")

ggplotly(p1)


p2 <- ggplotly(graficos_pib %>%
  mutate(zero = 0,
         crecimiento = round(crecimiento * 100, digits = 1)) %>% 
  ggplot(aes(x = fecha, y = crecimiento))+
  geom_line(colour = "#205375")+
  geom_line(aes(x = fecha, y = zero), colour = "black", linewidth = 0.5)+
  theme_bw()+
  labs(title = "Crecimiento de Nicaragua", x="", y="porcentajes (%)")
   )


# Actividades más dinámicas en 2021 y 2022
actividades_raw <- pib_produccion_tbl %>% 
  filter(year(fecha)>2016) %>% 
  select(yy, actividad, crecimiento) %>% 
  pivot_wider(names_from = yy, values_from = crecimiento) %>% 
  arrange(desc(`2022`)) %>% 
  mutate(across(-actividad, ~ scales::percent(. %>% round(digits = 3))))

actividades_mas_dinamicas_tbl <- actividades_raw %>% 
  filter(!str_detect(actividad, "Producto interno")) %>% 
  bind_rows(actividades_raw %>% 
              filter(str_detect(actividad, "Producto interno")))


p3 <- ggplotly(actividades_mas_dinamicas_tbl %>% 
  mutate(crecimiento = as.numeric(str_replace_all(`2022`, "%", "")),
         actividad = case_when(
           str_detect(actividad, "Impuestos") ~ "Impuestos",
           str_detect(actividad, "Producto interno") ~ "PIB",
           TRUE ~ actividad),
         color = case_when(
           crecimiento < 0 ~ "darkgrey",
           crecimiento >= 0 ~ "#205375"),
         color = if_else(str_detect(actividad, "PIB"), 
                         "#F66B0E", color)) %>%
  arrange(desc(crecimiento)) %>% 
  mutate(actividad = as_factor(actividad) %>% fct_rev()) %>% 
  ggplot(aes(y = actividad, x = crecimiento, fill = color, text = str_glue("{actividad}: {crecimiento}%")))+
    geom_bar(stat = "identity")+
  scale_fill_identity()+
  theme_bw()+
  labs(x = "", y="", title = "Crecimiento por actividades")+
  guides(fill = "none"),
  tooltip = c("text")
  ) 

p3

contribucion_actividades <- pib_produccion_tbl %>% 
  filter(year(fecha)>2020) %>% 
  select(yy, actividad, contribucion) %>% 
  pivot_wider(names_from = yy, values_from = contribucion) %>% 
  arrange(desc(`2022`)) %>% 
  mutate(across(-actividad, ~ scales::percent(. %>% round(digits = 4))))


contribucion_actividades













  

















