library(tidyverse)

periodos <- c(20,21,22,23)

data_nac <- list()

for (i in periodos){

  data_nac[[i]] <- readr::read_csv2(paste0("https://www.argentina.gob.ar/sites/default/files/2021/03/nacweb",i,".csv"),
                                   locale = readr::locale(encoding = "UTF-8"),col_names = FALSE)
  
  names(data_nac[[i]]) <- c("PROVRES","TIPPARTO","SEXO","IMEDAD","ITIEMGEST","IMINSTRUC","IPESONAC","CUENTA")
  
  data_nac[[i]]$ANIO <- paste0("20",i)
  
  print(i)
}

nacimientos <- do.call("rbind",data_nac)

#Genero un dataset con los siguientes indicadores por provincia

#Total nacidos vivos
# Nacidos vivos BPN
# Nacidos vivos de madres adolescentes
# Nacidos vivos pretermino

nacimientos$CUENTA <- as.numeric(nacimientos$CUENTA)

nacimientos <- nacimientos %>%
  group_by(ANIO,PROVRES) %>%
  summarise(TOTAL_NV = sum(CUENTA),
         BPN = sum(CUENTA[IPESONAC== "1.Menos de 2500 gramos"]),
         NV_MADRES_ADO = sum(CUENTA[IMEDAD %in% c("1.Menor de 15","2.15 a 19")]),
         NV_PRETERMINO = sum(CUENTA[as.numeric(substring(ITIEMGEST,1,1)) < 6]),
         NV_MADRE_ANALF = sum(CUENTA[IMINSTRUC == "1.Hasta Primaria/C.EGB Completa"]))%>%
  mutate(PROP_BPN = round(BPN*100/TOTAL_NV,2),
         PROP_MADRE_ADO = round(NV_MADRES_ADO*100/TOTAL_NV,2),
         PROP_PRETERMINO = round(NV_PRETERMINO*100/TOTAL_NV,2),
         PROP_MADRE_ANALF = round(NV_MADRE_ANALF*100/TOTAL_NV,2))

# Guardo todo en una shett
wb <- openxlsx::createWorkbook()

for(i in unique(nacimientos$ANIO)){
 
  openxlsx::addWorksheet(wb,paste0("AÑO ",i)) 
  openxlsx::writeData(wb,paste0("AÑO ",i),nacimientos %>% filter(ANIO == i))
print(i)
  }

openxlsx::saveWorkbook(wb,"nacimientos_argentina.xlsx",overwrite = TRUE)


