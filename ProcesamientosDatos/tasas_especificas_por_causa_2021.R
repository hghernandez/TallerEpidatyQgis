library(tidyverse)


#Cargo el tabulado de causas

data_2019 <- openxlsx::read.xlsx("CausaDef2019.xlsx")


data %>%
  mutate(filtro= substring(GRUPO.DE.CAUSAS.DE.MUERTE,1,1)) %>%
  filter(filtro != " " & !is.na(TOTAL))%>%
  select("Grupo_Causa_Def"=GRUPO.DE.CAUSAS.DE.MUERTE,TOTAL)%>%
  mutate(Tasa= round(TOTAL*100000/45808747,1))


data_2021 <- openxlsx::read.xlsx("CausaDef2021.xlsx")


data_2021  %>%
  mutate(filtro= substring(GRUPO.DE.CAUSAS.DE.MUERTE,1,1)) %>%
  filter(filtro != " " & !is.na(TOTAL))%>%
  select("Grupo_Causa_Def"=GRUPO.DE.CAUSAS.DE.MUERTE,TOTAL)%>%
  mutate(Tasa= round(TOTAL*100000/44938712,1))



#Calculo los APVP por INF y CV con limite 70 y limite 85

data_2020  <- openxlsx::read.xlsx("CausaDef2020.xlsx")

tabla_apvp <- data_2020 %>%
  mutate(GRUPO.DE.CAUSAS.DE.MUERTE= stringr::str_trim(GRUPO.DE.CAUSAS.DE.MUERTE))%>%
  filter(GRUPO.DE.CAUSAS.DE.MUERTE %in% c("Enfermedad por Covid-19","17. Causas externas"))%>%
    reshape2::melt()%>%
    pivot_wider(id_cols = variable,
                names_from = GRUPO.DE.CAUSAS.DE.MUERTE,
                values_from = value) %>%
  rename("EdadAgr"="variable","COVID"="Enfermedad por Covid-19",
           "CausasExternas"= "17. Causas externas") %>%
  filter(!EdadAgr %in% c("TOTAL","Sin.especificar")) 



apvp_85 <- tabla_apvp %>%
  filter(!is.na(COVID))%>%
  mutate(PMI= c(0.5,2.5,7.5,12.5,17.5,22.5,27.5,32.5,37.5,42.5,47.5,52.5,57.5,62.5,67.5,72.5,77.5,82.5,85),
         `85-PMI` = 85-PMI,
         APVP_COVID= COVID*`85-PMI`,
         APVP_CEXT = CausasExternas*`85-PMI`,
         Poblacion= c(737050,	2975939,	3763405,	3571271,	3506525,	3534762,	3549243,	3334543,	3148120,	3080074,	2663552,	2278318,	2103579,	1902660,	1657878,	1348279,	975114,	641693,	604758)
)

apvp_85 %>%
  summarise(APVP_COVID= sum(APVP_COVID),
            indice_APVP_COVID = sum(APVP_COVID)/sum(Poblacion)*100000,
            APVP_CEXT=sum(APVP_CEXT),
            indice_APVP_CEXT = sum(APVP_CEXT)/sum(Poblacion)*100000)
  

tabla_apvp_mod <- edit(tabla_apvp)

apvp_70 <- tabla_apvp_mod %>%
  filter(!is.na(COVID))%>%
  mutate(PMI= c(0.5,2.5,7.5,12.5,17.5,22.5,27.5,32.5,37.5,42.5,47.5,52.5,57.5,62.5,67.5,70),
         `70-PMI` = 70-PMI,
         APVP_COVID= COVID*`70-PMI`,
         APVP_CEXT = CausasExternas*`70-PMI`,
         Poblacion= c(737050,	2975939,	3763405,	3571271,	3506525,	3534762,	3549243,	3334543,	3148120,	3080074,	2663552,	2278318,	2103579,	1902660,	1657878, 3569844)
  )

apvp_70 %>%
  summarise(APVP_COVID= sum(APVP_COVID),
            indice_APVP_COVID = sum(APVP_COVID)/sum(Poblacion)*100000,
            APVP_CEXT=sum(APVP_CEXT),
            indice_APVP_CEXT = sum(APVP_CEXT)/sum(Poblacion)*100000)
