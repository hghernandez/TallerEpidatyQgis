library(tidyverse)

def <- readr::read_csv("http://datos.salud.gob.ar/dataset/27c588e8-43d0-411a-a40c-7ecc563c2c9f/resource/fab9e990-865c-43c4-a643-3dbc3b70a934/download/defunciones-ocurridas-y-registradas-en-la-republica-argentina-anos-2005-2021.csv",
                         locale = readr::locale(encoding = "latin1"))

def <- readr::read_delim("http://datos.salud.gob.ar/dataset/27c588e8-43d0-411a-a40c-7ecc563c2c9f/resource/e71b36a0-a68b-4bb5-a969-0a25929785bb/download/defuncion2023.csv",
                         locale = readr::locale(encoding = "UTF-8"),delim = ";")

##%######################################################%##
#                                                          #
####      Genero el dataset de defunciones por la       ####
####      causas de analisis básicos para el 2021       ####
#                                                          #
##%######################################################%##


unique(def$grupo_edad)

defu_2022 <- def %>%
       filter(anio == 2022) %>%
       mutate(
         CAUSA = case_when(
           cie10_causa_id >= 'I00' &
             cie10_causa_id <= 'I99' & cie10_causa_id != 'I46' ~ 'CV',
           cie10_causa_id >= 'C00' &
             cie10_causa_id <= 'D48' ~ 'TUM',
           (cie10_causa_id >= 'A00' &
              cie10_causa_id <= 'B99') |
             (cie10_causa_id >= 'J00' & cie10_causa_id <= 'J22')
           |
             (cie10_causa_id >= 'G00' & cie10_causa_id <= 'G03') ~ 'INF',
           (cie10_causa_id >= 'V01' &
              cie10_causa_id <= 'V99') |
             (cie10_causa_id >= 'W00' & cie10_causa_id <= 'Y98') ~ 'CE',
         TRUE ~ 'TLD'
       ),
       jurisdiccion_de_residencia_id= case_when(as.numeric(jurisdiccion_de_residencia_id) < 10 ~ 
                                                  paste0(0,jurisdiccion_de_residencia_id),
                                                TRUE ~ as.character(jurisdiccion_de_residencia_id)),
       Sexo= case_when(Sexo == 'masculino' ~ 'Varones',
                       Sexo == 'femenino' ~ 'Mujeres',
                       Sexo== "desconocido" ~ "desconocido",
                       Sexo=="indeterminado" ~ "indeterminado")
       ) %>%
       group_by("IDPROV"=jurisdiccion_de_residencia_id,
                "NOMPROV"= jurisdicion_residencia_nombre,
                "EDAD"=grupo_edad,
                "SEXO"= Sexo,
                CAUSA)%>%
       summarise(DEFUNCIONES= sum(cantidad))%>%
       bind_rows(group_by(.,IDPROV,NOMPROV,EDAD,CAUSA)%>%
                   summarise(DEFUNCIONES= sum(DEFUNCIONES))%>%
                   mutate(SEXO="Ambos sexos")) %>%
       bind_rows(group_by(.,IDPROV,NOMPROV,SEXO,CAUSA)%>%
                   summarise(DEFUNCIONES= sum(DEFUNCIONES))%>%
                   mutate(EDAD="07.TODAS LAS EDADES"))%>%
  filter(EDAD != "06.Sin especificar" & 
           !SEXO %in% c("desconocido","indeterminado") &
           !IDPROV %in% c(98,99)) %>%
  arrange(as.numeric(IDPROV,EDAD,SEXO))

#Completo todas las combinaciones

provincias <- defu_2022 %>%
  ungroup()%>%
  distinct(IDPROV,NOMPROV)



all <- tidyr::crossing("IDPROV"=defu_2022$IDPROV,
                       "EDAD"=defu_2022$EDAD,
                       "SEXO"=defu_2022$SEXO,
                       "CAUSA"=defu_2022$CAUSA) %>%
  left_join(defu_2022 %>% ungroup() %>% select(-c("NOMPROV")),by= c("IDPROV","EDAD","SEXO","CAUSA")) %>%
  left_join(provincias, by= "IDPROV")


#Cargo las estimaciones de poblacion

poblacion <- read.table("src/poblacion1990-2021.txt")

#Proceso los demás años

library(readxl)


#Listo el nombre de las sheet para usar el nombre y codigo de prov
sheets <- excel_sheets("src/c2_proyecciones_prov_2010_2040.xls")

poblaciones <- list()

for (i in 2:length(sheets)){

  poblaciones[[i]] <- read_excel("src/c2_proyecciones_prov_2010_2040.xls", 
                       sheet = i,range = "A61:X84")
  
  poblaciones[[i]] <- poblaciones[[i]] %>% 
    reshape2::melt() %>%
    select(c(1,8)) %>%
    filter(!is.na(value))%>%
    mutate(sexo =  rep(c(rep('Ambos sexos',22),rep('Varones',22),rep('Mujeres',22)),6),
           anio= rep(2022:2027, each = 66),
           id_edad = rep(c(22, 1:21), length.out = 396),
           gredad= paste0(id_edad,".",`...1`))
  
  
  
  poblaciones[[i]]['codigo'] <- as.numeric(substring(sheets[i],1,2))
  poblaciones[[i]]['localidad'] <- paste0(substring(sheets[i],1,2),"-",substring(sheets[i],4,stringr::str_length(sheets[i])))
  print(i)
}

#Unifico las poblaciones

df_pobl_22_27 <- do.call("rbind",poblaciones)

# Armo estimaciones para usar en Vitales y epidemio

pob_vitales <- df_pobl_22_27 %>%
       mutate(EDAD_VITALES= case_when(id_edad <= 3 ~ "01.De a 0  a 14 anios",
                              id_edad %in% c(4,5,6,7) ~ "02.De 15 a 34 anios",
                              id_edad %in% c(8,9,10,11) ~ "03.De 35 a 54 anios",
                              id_edad %in% c(12,13,14,15) ~ "04.De 55 a 74 anios",
                              id_edad %in% c(16,17,18,19,20,21) ~ "05.De 75 anios y mas",
                              id_edad == 22 ~ "06.TODAS LAS EDADES"),
              EDAD_EPIDEMIO= case_when(id_edad %in% c(14,15,16,17,18,19,20,21) ~ "14.De 65 anios y mas",
                                      id_edad == 22 ~ "15.TODAS LAS EDADES",
                                      TRUE ~ gredad))%>%
       group_by(anio,codigo,localidad,EDAD_VITALES,sexo)%>%
       summarise(poblacion=sum(value))%>%
       filter(codigo != 1)%>%
       mutate(codigo= case_when(as.numeric(codigo) < 10 ~ paste0(0,codigo),
                                TRUE ~ as.character(codigo)))
#Pob epidemio
pob_epidemio <- df_pobl_22_27 %>%
  mutate(EDAD_VITALES= case_when(id_edad <= 3 ~ "01.De a 0  a 14 anios",
                                 id_edad %in% c(4,5,6,7) ~ "02.De 15 a 34 anios",
                                 id_edad %in% c(8,9,10,11) ~ "03.De 35 a 54 anios",
                                 id_edad %in% c(12,13,14,15) ~ "04.De 55 a 74 anios",
                                 id_edad %in% c(16,17,18,19,20,21) ~ "05.De 75 anios y mas",
                                 id_edad == 22 ~ "06.TODAS LAS EDADES"),
         EDAD_EPIDEMIO= case_when(id_edad %in% c(14,15,16,17,18,19,20,21) ~ "14.De 65 anios y mas",
                                  id_edad == 22 ~ "15.TODAS LAS EDADES",
                                  TRUE ~ gredad))%>%
  group_by(anio,codigo,localidad,EDAD_EPIDEMIO,sexo)%>%
  summarise(poblacion=sum(value))%>%
  filter(codigo != 1)%>%
  mutate(codigo= case_when(as.numeric(codigo) < 10 ~ paste0(0,codigo),
                           TRUE ~ as.character(codigo)))


#Guardo la poblacion de epidemiologia

write.csv2(pob_epidemio,"src/pob_epidemio.csv",row.names = F)

#Agrego la poblacion a las defunciones

names(all)
names(poblacion)

defunciones_argentina_2022 <-  all %>%
  left_join(pob_vitales %>% filter(anio == 2022), by= c("IDPROV"="codigo",
                             "EDAD"="EDAD_VITALES",
                             "SEXO"= "sexo"))%>%
    select(-localidad) %>%
  rename("POBLACION"=poblacion)%>%
    pivot_wider(id_cols= c(IDPROV,NOMPROV,EDAD,SEXO,POBLACION),
                names_from = CAUSA,
                values_from = c(DEFUNCIONES),
                values_fill = 0)


defunciones_argentina_2022[is.na(defunciones_argentina_2022)] <- 0

#Poblacion estandar

estandar_2000 <- poblacion %>%
  filter(ano == 2000) %>%
  mutate(EDAD= case_when(as.numeric(substring(gredad,1,2)) <= 3 ~ "01.De a 0  a 14 anios",
                         as.numeric(substring(gredad,1,2)) %in% c(4,5,6,7) ~ "02.De 15 a 34 anios",
                         as.numeric(substring(gredad,1,2)) %in% c(8,9,10,11) ~ "03.De 35 a 54 anios",
                         as.numeric(substring(gredad,1,2)) %in% c(12,13,14,15) ~ "04.De 55 a 74 anios",
                         as.numeric(substring(gredad,1,2)) %in% c(16,17) ~ "05.De 75 anios y mas"))%>%
  group_by(codigo,localidad,EDAD,sexo)%>%
  summarise(poblacion=sum(poblacion))%>%
  filter(codigo == 1 & sexo == 'Ambos sexos')

estandar_2010 <- poblacion %>%
  filter(ano == 2010) %>%
  mutate(EDAD= case_when(as.numeric(substring(gredad,1,2)) <= 3 ~ "01.De a 0  a 14 anios",
                         as.numeric(substring(gredad,1,2)) %in% c(4,5,6,7) ~ "02.De 15 a 34 anios",
                         as.numeric(substring(gredad,1,2)) %in% c(8,9,10,11) ~ "03.De 35 a 54 anios",
                         as.numeric(substring(gredad,1,2)) %in% c(12,13,14,15) ~ "04.De 55 a 74 anios",
                         as.numeric(substring(gredad,1,2)) %in% c(16,17) ~ "05.De 75 anios y mas"))%>%
  group_by(codigo,localidad,EDAD,sexo)%>%
  summarise(poblacion=sum(poblacion))%>%
  filter(codigo == 1 & sexo == 'Ambos sexos')

#tasas estandar

tasas_estandar <- all %>%
  group_by(EDAD,SEXO,CAUSA)%>%
  summarise(DEFUNCIONES= sum(DEFUNCIONES))%>%
  left_join(pob_vitales %>%
              filter(anio == 2023) %>%
              group_by(sexo,EDAD_VITALES) %>%
              summarise(POBLACION= sum(poblacion)), by= c("EDAD"="EDAD_VITALES",
                       "SEXO"= "sexo")) %>%
    mutate(TASA= DEFUNCIONES*100000/POBLACION)%>%
    replace(is.na(.),0) %>%
    filter(EDAD != '07.TODAS LAS EDADES' & SEXO == 'Ambos sexos') %>%
    pivot_wider(id_cols= c(EDAD,SEXO),
                names_from = CAUSA,
                values_from = c(DEFUNCIONES,POBLACION,TASA),
                values_fill = 0,
                names_vary = "slowest")

  
write.table(tasas_estandar,"clipboard",sep="\t", dec=",",row.names = FALSE)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb,"Defunciones")
openxlsx::addWorksheet(wb,"Defunciones Todas la Edades")
openxlsx::addWorksheet(wb,"Estandar 2010")
openxlsx::addWorksheet(wb,"Estandar 2000")
openxlsx::addWorksheet(wb,"Tasas Estandar")


openxlsx::writeData(wb,"Defunciones",defunciones_argentina_2023 %>% filter(EDAD != "07.TODAS LAS EDADES"))
openxlsx::writeData(wb,"Defunciones Todas la Edades",defunciones_argentina_2023 %>% filter(EDAD == "07.TODAS LAS EDADES"))
openxlsx::writeData(wb,"Estandar 2010",estandar_2010)
openxlsx::writeData(wb,"Estandar 2000",estandar_2000)
openxlsx::writeData(wb,"Tasas Estandar",tasas_estandar)

openxlsx::saveWorkbook(wb,"defunciones_argentina_2023.xlsx",overwrite = TRUE)


