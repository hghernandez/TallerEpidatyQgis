library(tidyverse)
library(readr)


data_raw_23 <- read_delim("http://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/19075374-b180-48a0-aaaf-e0e44cd6816f/download/informacion-publica-dengue-zika-nacional-se-1-a-52-de-2023-2024-06-10.csv", 
                                                                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)


data_raw_24 <- read_delim("http://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/e57edd84-096d-4348-9137-2f5dae9c8f55/download/informacion-publica-dengue-zika-nacional-se-1-a-52-de-2024-2025-04-14.csv",
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Veo la cantidad de datos

View(data_raw_23 %>%
  filter(id_prov_indec_residencia == 30) %>%
  group_by(across(c(9,1))) %>%
  summarise(n= sum(cantidad)))

View(data_raw_24 %>%
       filter(id_prov_indec_residencia == 30) %>%
       group_by(sepi_min,id_depto_indec_residencia) %>%
       summarise(n= sum(cantidad)))

# Selecciono solo Entre Rios y proceso

data_raw_23 <- data_raw_23 %>% filter(id_prov_indec_residencia == 30) 


data_raw_24 <- data_raw_24 %>% filter(id_prov_indec_residencia == 30)


# Armo una combinacion de todos los departamentos y todas las semanas epi

# Suponé que estos son tus 12 grupos de edad


deptos <- c("008","015","021","028","035","042","049","056",
            "063","070","077","084","088","091","098","105","113")

gredad <- 
# Crear todas las combinaciones posibles

combinaciones <- expand.grid(
  semana = 1:52,
  edad = 1:12,
  deptos = deptos,
  stringsAsFactors = FALSE
)

str(combinaciones)
str(data_raw_23)

# Armo el dataset de 2023

dengue_er_2023 <- combinaciones %>%
  left_join(data_raw_23 %>%
              select(sepi_min,id_grupo_etario,id_depto_indec_residencia,cantidad), by= c("semana"= "sepi_min","edad"="id_grupo_etario","deptos"="id_depto_indec_residencia"))%>%
    mutate(cantidad= if_else(is.na(cantidad),0,as.numeric(cantidad)),
           anio= 2023,
           evento="Dengue")

# Armo el dataset de 2024

dengue_er_2024 <- combinaciones %>%
  left_join(data_raw_24 %>%
              select(sepi_min,id_grupo_etario,id_depto_indec_residencia,cantidad), by= c("semana"= "sepi_min","edad"="id_grupo_etario","deptos"="id_depto_indec_residencia"))%>%
  mutate(cantidad= if_else(is.na(cantidad),0,as.numeric(cantidad)),
         anio= 2024,
         evento="Dengue")




