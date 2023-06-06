
#remittances

pacman::p_load(tidyverse, plotly, timetk, lubridate)

load("C:/Users/William Mendieta/Documents/GitHub/Macroeconomic_Data_Nicaragua/datos_macro.RData")


remesas <- datos_macro %>%  
  filter(str_detect(name, "remesas")) %>% 
  select(fecha, value)


# Evolución de las remesas ----
gr1 <- remesas %>% 
  ggplot(aes(fecha, value))+
  geom_line(color = "#112B3C")+
  theme_bw()+
  labs(title = "Remesas familiares (millones de dólares)", 
       y="", x="")

ggplotly(gr1)

# Flujos de remesas anuales ----
remesas_anuales_tbl <- remesas %>% 
  mutate(yy = year(fecha)) %>% 
  group_by(yy) %>% 
  summarize(yy = yy, 
            remesas = sum(value, na.rm = T)) %>% 
  distinct() %>% 
  mutate(`Flujo enero-marzo anualizado` = if_else(yy == 2023, remesas / 3 * 12, remesas)) %>% 
  ungroup()

gr2 <- remesas_anuales_tbl %>% 
  ggplot()+
  geom_col(aes(yy, `Flujo enero-marzo anualizado`), fill = "lightgrey", color = "darkgrey")+
  geom_col(aes(yy, remesas), fill = "#112B3C")+
  theme_bw()+
  labs(title = "Remesas recibidas anualmente (millones de dolares)",
      x = "", y= "")

ggplotly(gr2)  

# Crecimiento anual de las remesas
crecimiento_remesas_tbl <- remesas_anuales_tbl %>% 
  select(yy,remesas) %>% 
  mutate(crecimiento = remesas / lag(remesas, 1) *100 - 100)

gr3 <- crecimiento_remesas_tbl %>%
  filter(yy<2023) %>% 
  ggplot(aes(yy, crecimiento))+
  geom_line(color = "steelblue")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  labs(title = "Crecimiento de las remesas familiares (porcentajes)",
       x = "", y="")

ggplotly(gr3)


# Descomposición de las remesas

decomp_remesas <- remesas %>%
  mutate(lrem = log(value)) %>% 
  select(-value) %>% 
  bind_cols(., tk_stl_diagnostics(., fecha, lrem) %>% select(-fecha)) %>% 
  select(-lrem)


gr4 <- decomp_remesas %>% 
  ggplot()+
#  geom_line(aes(fecha, lrem), color = "steelblue")+
  geom_line(aes(fecha, trend), color = "darkgrey", linewidth = 1)+
  geom_line(aes(fecha, seasadj), color = "steelblue")+
  theme_bw()

ggplotly(gr4)  


gr5 <- decomp_remesas %>% 
  pivot_longer(-fecha) %>%
  mutate(name = fct_relevel(as_factor(name), 
                           c("observed", "seasadj", "trend", "season", "remainder"))) %>% 
  ggplot(aes(fecha, value))+
  geom_line(color = "steelblue")+
  facet_wrap(~ name, scales = "free", ncol = 1)+
  theme_bw()+
  theme(strip.background = element_rect("#112B3C"), 
        strip.text = element_text(colour = "white"))+
  labs(title = "Descomposición temporal de la serie de remesas (en logaritmos)", 
       x = "", y="")

ggplotly(gr5)


# Flujos acumulados de remesas por yy
flujos_acumulados_tbl <- remesas %>% 
  filter(fecha>as.Date("2019-12-31")) %>% 
  mutate(yy = year(fecha), 
         mes = month(fecha, label = T)) %>% 
  select(-fecha) %>% 
  pivot_wider(names_from = yy, values_from = value) %>% 
  mutate(across(-mes, cumsum))


gr6 <- flujos_acumulados_tbl %>%
  ggplot()+
  geom_point(aes(mes, `2023`), shape = 1,color = "steelblue", size = 3)+
  geom_point(aes(mes, `2022`), shape = 2,color = "darkgrey", size = 3)+
  geom_point(aes(mes, `2021`), shape = 3,color = "orange", size = 3, alpha = 0.5)+
  geom_point(aes(mes, `2020`), shape = 4,color = "red", size = 3, alpha = 0.5)+
  theme_bw()+
  labs(title = "Flujos acumulados de remesas (millones de dólares)", x="", y="")


ggplotly(gr6)


# Remesas y actividad económica ----


rem_plus_imae <- datos_macro %>% 
  filter(str_detect(name, "imae|remesas"),
         !str_detect(name, "sin_impuestos")) %>% 
  select(fecha, name, value) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  arrange(fecha) %>% 
  mutate(across(-fecha, ~ .x / lag(.x, 1) * 100- 100)) %>% 
  pivot_longer(-fecha) %>% 
  filter(year(fecha)> 2006)


rem_plus_imae %>% 
  plot_time_series(fecha, value, .color_var = name, .smooth = F)

rem_plus_imae %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot() + 
  geom_point(aes(remesas, imae))+
  geom_smooth(aes(remesas, imae), method = "lm")


roll_cor <- slidify(
  .f = cor,
  .period = 12*5, 
  .align = "right"
)

gr7 <- ggplotly(rem_plus_imae %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(rollcorr = roll_cor(imae, remesas)) %>% 
  drop_na(rollcorr) %>% 
  ggplot()+
  geom_line(aes(fecha, rollcorr), color = "steelblue")+
  theme_bw()+
  labs(title = "Remesas - Crecimiento: Una relación que se hace cada vez más fuerte",
       x="", y="")
)

gr7

cor_data <- rem_plus_imae %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  drop_na()

cor(cor_data$imae, cor_data$remesas)









