##  ¿Cómo empezar? 


# Si no tenés remotes
#install.packages("remotes")

# Instalar ARcenso desde GitHub
#remotes::install_github("SoyAndrea/arcenso")

# Instalar paquetes necesarios desde CRAN
#install.packages(c("dplyr", "tidyr", "ggplot2", "gt"))



library(arcenso)  # obtención de datos censales
library(dplyr)    # procesamiento de datos
library(tidyr)    # orden y transformación de datos
library(ggplot2)  # diseño de gráficos
library(gt)       # diseño de tablas


arcenso_app()

check_repository(topic = "estructura", geo_code = "00")



## Preparación de los datos


poblacion_1970 <- get_census(id = "1970_00_estructura_01")

pob_1970 <- poblacion_1970 |>
  filter(sexo != "Total") |>
  mutate(
    censo = 1970,
    grupo_de_edad = case_when(
      grupo_de_edad == "0-4" ~ "00-04",
      grupo_de_edad == "5-9" ~ "05-09",
      TRUE ~ grupo_de_edad
    )
  ) |>
  rename(grupo_edad = grupo_de_edad) |>
  select(censo, sexo, grupo_edad, poblacion)


### Censo 1980


poblacion_1980 <- get_census(id = "1980_00_estructura_03")

pob_1980 <- poblacion_1980 |>
  filter(
    urbano_rural == "Total",
    sexo != "Total",
    edad != "Total"
  ) |>
  mutate(
    censo = 1980,
    edad_num = ifelse(edad == "85 y más", 85, as.numeric(edad)),
    grupo_edad = case_when(
      edad_num %in% c(0:4)   ~ "00-04",
      edad_num %in% c(5:9)   ~ "05-09",
      edad_num %in% c(10:14) ~ "10-14",
      edad_num %in% c(15:19) ~ "15-19",
      edad_num %in% c(20:24) ~ "20-24",
      edad_num %in% c(25:29) ~ "25-29",
      edad_num %in% c(30:34) ~ "30-34",
      edad_num %in% c(35:39) ~ "35-39",
      edad_num %in% c(40:44) ~ "40-44",
      edad_num %in% c(45:49) ~ "45-49",
      edad_num %in% c(50:54) ~ "50-54",
      edad_num %in% c(55:59) ~ "55-59",
      edad_num %in% c(60:64) ~ "60-64",
      edad_num %in% c(65:69) ~ "65-69",
      edad_num %in% c(70:74) ~ "70-74",
      edad_num %in% c(75:79) ~ "75-79",
      edad_num %in% c(80:84) ~ "80-84",
      TRUE ~ "85 y más"
    )
  ) |>
  select(-edad, -urbano_rural, -edad_num) |>
  select(censo, sexo, grupo_edad, poblacion)



poblacion <- bind_rows(pob_1970, pob_1980) |>
  mutate(
    poblacion = as.numeric(poblacion),
    sexo = factor(sexo, levels = c("Varones", "Mujeres")),
    grupo_edad = factor(
      grupo_edad,
      levels = c(
        "00-04", "05-09", "10-14", "15-19", "20-24",
        "25-29", "30-34", "35-39", "40-44", "45-49",
        "50-54", "55-59", "60-64", "65-69", "70-74",
        "75-79", "80-84", "85 y más"))
  )


## Estructura de la población



head(poblacion)



### Pirámide poblacional

piramide <- poblacion |>
  group_by(censo, sexo) |>
  mutate(
    poblacion_rel = if_else(
      sexo == "Varones",
      -poblacion / sum(poblacion),
       poblacion / sum(poblacion))) |>
  ungroup()

# Pirámide comparativa
piramide |>
  ggplot(aes(x = poblacion_rel, y = grupo_edad, fill = sexo)) +
  geom_col() +
  facet_wrap(~ censo, ncol = 2) +
  scale_fill_manual(values = c("#00f59b", "#7014f2")) +
  scale_x_continuous(
    labels = function(x) paste0(abs(round(x * 100, 1)), "%"),
    limits = c(-0.15, 0.15),
    breaks = seq(-0.15, 0.15, by = 0.05)) +
  labs(
    title    = "Estructura de la población por sexo y grupo quinquenal de edad",
    subtitle = "Argentina. Años 1970 y 1980",
    x        = "Porcentaje",
    y        = "Grupo quinquenal de edad",
    caption  = "Fuente: INDEC, Censo Nacional de Población 1970 y 1980. Procesado con ARcenso.",
    fill     = "Sexo") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12) )



### Índice de envejecimiento


envejecimiento <- poblacion |>
  group_by(censo) |>
  summarise(
    poblacion_0a14   = sum(poblacion[grupo_edad %in% c("00-04", "05-09", "10-14")]),
    poblacion_65ymas = sum(poblacion[grupo_edad %in% c("65-69", "70-74", "75-79", "80-84", "85 y más")]),
    indice = round(poblacion_65ymas / poblacion_0a14 * 100, 0))

gt(envejecimiento) |>
  tab_header(
    title    = "Comparación del índice de envejecimiento",
    subtitle = "Argentina. Años 1970 y 1980"
  ) |>
  tab_spanner(
    label   = "Población",
    columns = c(poblacion_0a14, poblacion_65ymas)
  ) |>
  fmt_number(
    columns  = c(poblacion_0a14, poblacion_65ymas),
    decimals = 0,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  cols_label(
    poblacion_0a14   = "0 a 14 años",
    poblacion_65ymas = "65 años y más",
    indice           = "Índice"
  ) |>
  tab_source_note(
    source_note = 
      md("**Fuente:** elaboración propia en base a datos de INDEC (Censos Nacionales de Población 1970 y 1980).")) |>
  tab_options(
    table.background.color = "white"
  )

### Índice de feminidad

feminidad <- poblacion |>
  filter(
    grupo_edad %in% c("60-64", "65-69", "70-74", "75-79", "80-84", "85 y más")
  ) |>
  group_by(censo, grupo_edad, sexo) |>
  summarise(poblacion = sum(poblacion), .groups = "drop") |>
  pivot_wider(names_from = sexo, values_from = poblacion) |>
  mutate(
    indice_feminidad = round(Mujeres / Varones * 100, 0)
  ) |>
  select(-Varones, -Mujeres)


feminidad |>
  pivot_wider(
    names_from   = censo,
    values_from  = indice_feminidad,
    names_prefix = "censo_"
  ) |>
  ggplot(aes(y = grupo_edad)) +
  geom_segment(
    aes(x = censo_1970, xend = censo_1980, yend = grupo_edad),
    color = "grey85",
    linewidth = 1) +
  geom_point(aes(x = censo_1970), color = "#ff0f7b", size = 3) +
  geom_point(aes(x = censo_1980), color = "#f89b29", size = 3) +
  geom_text(aes(x = censo_1970, label = censo_1970), hjust = 1.4, size = 3) +
  geom_text(aes(x = censo_1980, label = censo_1980), hjust = -0.4, size = 3) +
  labs(
    x        = "Mujeres por cada 100 varones",
    y        = "Grupo de edad",
    title    = "Cambio en el índice de feminidad de la población de 60 años y más",
    subtitle = "Argentina. Años 1970 y 1980",
    caption  = "Fuente: INDEC, Censo Nacional de Población 1970 y 1980. Procesado con ARcenso."
  ) +
  theme_minimal()


### Índice de Whipple

edad_simple_1980 <- get_census(id = "1980_00_estructura_03") |>
  filter(
    urbano_rural == "Total",
    sexo == "Total",
    edad != "Total",
    edad != "85 y más") |>
  mutate(
    edad = as.numeric(edad),
    poblacion = as.numeric(poblacion))

whipple <- edad_simple_1980 |>
  filter(edad >= 23, edad <= 62) |>
  summarise(
    pob_multiplos_5 = sum(poblacion[edad %in% seq(25, 60, by = 5)]),
    pob_total       = sum(poblacion),
    indice_whipple  = round(pob_multiplos_5 / (pob_total / 5) * 100, 1) )


gt(whipple) |>
  tab_header(
    title    = "Índice de Whipple",
    subtitle = "Argentina. Censo 1980 (población de 23 a 62 años)") |>
  fmt_number(
    columns  = c(pob_multiplos_5, pob_total),
    decimals = 0,
    sep_mark = ".",
    dec_mark = ",") |>
  cols_label(
    pob_multiplos_5 = "Población en edades múltiplo de 5",
    pob_total       = "Población total (23 a 62 años)",
    indice_whipple  = "Índice") |>
  tab_source_note(
    source_note = 
      md("**Fuente:** elaboración propia en base a datos de INDEC (Censo Nacional de Población 1980). Escala de referencia: <105 muy preciso, 105–124 aceptable, ≥125 deficiente.")) |>
  tab_options(
    table.background.color = "white"
  )

