library(tidyverse)

if(file.exists("EPH_final.Rda")) {
  load("EPH_final.Rda")
} else {
  
  # Descarga de archivos
  if(!file.exists("EPH_usu_3_Trim_2018_txt.zip")) {
    download.file("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_3_Trim_2018_txt.zip",
                  destfile="EPH_usu_3_Trim_2018_txt.zip")
    download.file("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_2_Trim_2018_txt.zip",
                  destfile="EPH_usu_2_Trim_2018_txt.zip")
    download.file("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_1_Trim_2018_txt.zip",
                  destfile="EPH_usu_1_Trim_2018_txt.zip")
    download.file("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_4_Trim_2017_txt.zip",
                  destfile="EPH_usu_4_Trim_2017_txt.zip")
    download.file("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_3_Trim_2017_txt.zip",
                  destfile="EPH_usu_3_Trim_2017_txt.zip")
    download.file("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_2_Trim_2017_txt.zip",
                  destfile="EPH_usu_2_Trim_2017_txt.zip")
    download.file("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_1er_Trim_2017_txt.zip",
                  destfile="EPH_usu_1_Trim_2017_txt.zip")
    download.file("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_4toTrim_2016_txt.zip",
                  destfile="EPH_usu_4_Trim_2016_txt.zip")
    download.file("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_3erTrim_2016_txt.zip",
                  destfile="EPH_usu_3_Trim_2016_txt.zip")
    download.file("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_2doTrim_2016_txt.zip",
                  destfile="EPH_usu_2_Trim_2016_txt.zip")
  } 
  
  # Lectura de las bases
  T3_2018_u <- read.delim(unz("EPH_usu_3_Trim_2018_txt.zip", "usu_individual_T318.txt"), header = TRUE, sep=";")
  T3_2018_h <- read.delim(unz("EPH_usu_3_Trim_2018_txt.zip", "usu_hogar_T318.txt"), header = TRUE, sep=";")
  T2_2018_u <- read.delim(unz("EPH_usu_2_Trim_2018_txt.zip", "usu_individual_t218.txt"), header = TRUE, sep=";")
  T2_2018_h <- read.delim(unz("EPH_usu_2_Trim_2018_txt.zip", "usu_hogar_t218.txt"), header = TRUE, sep=";")
  T1_2018_u <- read.delim(unz("EPH_usu_1_Trim_2018_txt.zip", "usu_individual_t118.txt"), header = TRUE, sep=";")
  T1_2018_h <- read.delim(unz("EPH_usu_1_Trim_2018_txt.zip", "usu_hogar_t118.txt"), header = TRUE, sep=";")
  T4_2017_u <- read.delim(unz("EPH_usu_4_Trim_2017_txt.zip", "Usu_Individual_T417.txt"), header = TRUE, sep=";")
  T4_2017_h <- read.delim(unz("EPH_usu_4_Trim_2017_txt.zip", "Usu_hogar_T417.txt"), header = TRUE, sep=";")
  T3_2017_u <- read.delim(unz("EPH_usu_3_Trim_2017_txt.zip", "usu_individual_t317.txt"), header = TRUE, sep=";")
  T3_2017_h <- read.delim(unz("EPH_usu_3_Trim_2017_txt.zip", "usu_hogar_t317.txt"), header = TRUE, sep=";")
  T2_2017_u <- read.delim(unz("EPH_usu_2_Trim_2017_txt.zip", "usu_individual_t217.txt"), header = TRUE, sep=";")
  T2_2017_h <- read.delim(unz("EPH_usu_2_Trim_2017_txt.zip", "usu_hogar_t217.txt"), header = TRUE, sep=";")
  T1_2017_u <- read.delim(unz("EPH_usu_1_Trim_2017_txt.zip", "usu_individual_t117.txt"), header = TRUE, sep=";")
  T1_2017_h <- read.delim(unz("EPH_usu_1_Trim_2017_txt.zip", "usu_hogar_t117.txt"), header = TRUE, sep=";")
  T4_2016_u <- read.delim(unz("EPH_usu_4_Trim_2016_txt.zip", "usu_individual_t416.txt"), header = TRUE, sep=";")
  T4_2016_h <- read.delim(unz("EPH_usu_4_Trim_2016_txt.zip", "usu_hogar_t416.txt"), header = TRUE, sep=";")
  T3_2016_u <- read.delim(unz("EPH_usu_3_Trim_2016_txt.zip", "usu_individual_t316.txt"), header = TRUE, sep=";")
  T3_2016_h <- read.delim(unz("EPH_usu_3_Trim_2016_txt.zip", "usu_hogar_t316.txt"), header = TRUE, sep=";")
  T2_2016_u <- read.delim(unz("EPH_usu_2_Trim_2016_txt.zip", "usu_individual_T216.txt"), header = TRUE, sep=";")
  T2_2016_h <- read.delim(unz("EPH_usu_2_Trim_2016_txt.zip", "usu_hogar_T216.txt"), header = TRUE, sep=";")
  
  # Creación de la variable que indica el trimestre
  T3_2018_u$T <- 10
  T3_2018_h$T <- 10
  T2_2018_u$T <- 9
  T2_2018_h$T <- 9
  T1_2018_u$T <- 8
  T1_2018_h$T <- 8
  T4_2017_u$T <- 7
  T4_2017_h$T <- 7
  T3_2017_u$T <- 6
  T3_2017_h$T <- 6
  T2_2017_u$T <- 5
  T2_2017_h$T <- 5
  T1_2017_u$T <- 4
  T1_2017_h$T <- 4
  T4_2016_u$T <- 3
  T4_2016_h$T <- 3
  T3_2016_u$T <- 2
  T3_2016_h$T <- 2
  T2_2016_u$T <- 1
  T2_2016_h$T <- 1
  
  # Cálculo del ingreso medio por trimestre y
  # del factor de actualización
  ingreso_medio <- rbind(T3_2018_u, T2_2018_u, T1_2018_u, T4_2017_u, T3_2017_u, T2_2017_u, T1_2017_u, T4_2016_u, T3_2016_u, T2_2016_u) %>%
    group_by(T) %>%
    filter(P47T>0) %>%
    summarise(promedio = mean(P47T, na.rm = TRUE)) 
  tasa <- 1
  for(i in (9:1)) {
    tasa_n <- ingreso_medio$promedio[i+1]/ingreso_medio$promedio[i]
    tasa <- c(tasa, tasa_n)
  }
  for(i in (2:10)) {
    tasa[i] <- tasa[i]*tasa[i-1]
  }
  tasa_inv <- rep(0,10)
  for(i in (1:10)) {
    tasa_inv[i] <- tasa[10-i+1]
  }
  actualizar_ingreso <- data.frame(T=c(1:10), factor=tasa_inv)
  rm(tasa, tasa_inv, i)
  
  bases_ind <- rbind(T3_2018_u, T2_2018_u, T1_2018_u, T4_2017_u, T3_2017_u, T2_2017_u, T1_2017_u, T4_2016_u, T3_2016_u, T2_2016_u) %>%
    select(CODUSU, NRO_HOGAR, COMPONENTE, MAS_500, REGION, CH03, CH04, CH06, CH15, CH16, P47T, T, ESTADO, NIVEL_ED) %>%
    group_by(CODUSU, NRO_HOGAR, T) %>%
    mutate(n_trabaj_hogar = sum(ESTADO %in% c(1,2))) %>%
    ungroup() %>%
    mutate(REGION = as.factor(REGION),
           CH03 = as.factor(CH03),
           CH15 = as.factor(CH15),
           CH16 = as.factor(CH16),
           COMPONENTE = as.factor(COMPONENTE),
           MAS_500 = as.factor(MAS_500))
  
  # Se detectaron valores NA en el ingreso individual (P47T)
  # En todos los casos, la persona es inactiva o no se realizaó la encuesta
  bases_ind %>% filter(is.na(P47T)) %>% select(P47T, ESTADO) %>% group_by(ESTADO) %>% summarise(n())
  # Por lo tanto, se reemplaza con ingreso = 0
  bases_ind$P47T[is.na(bases_ind$P47T)] <- 0
  # También se reemplazan las no respuestas (-9) por 0
  bases_ind$P47T[bases_ind$P47T<0] <- 0
  
  # Se tiene la edad de todos los jefes de hogar y sus conyuges
  bases_ind %>% filter(CH06==-1) %>% filter(CH03=="1")
  bases_ind %>% filter(CH06==-1) %>% filter(CH03=="2")
  
  # Pero no de todos los demás. 
  # Se asignan las siguientes edades:
  # A los hijos, la edad del jefe de hogar menos 25 años (0 si la cuenta da negativa)
  # A los nietos, la edad del jefe de hogar menos 50 años (0 si la cuenta da negativa)
  # Al yerno/nuera/hermanos/otro/no familiar, la misma edad del jefe de hogar
  # Al madre/padre/suegros, la edad del jefe más 25 años
  
  edad_jefe <- bases_ind %>%
    filter(CH03=="1") %>%
    group_by(CODUSU, NRO_HOGAR, T) %>%
    mutate(Edad_jefe=CH06) %>%
    select(Edad_jefe) %>%
    ungroup() 
  bases_ind <- bases_ind %>%
    left_join(edad_jefe, by=c("CODUSU", "NRO_HOGAR", "T"))
  bases_ind$CH06[bases_ind$CH06==-1 & bases_ind$CH03=="3"] <- bases_ind$Edad_jefe[bases_ind$CH06==-1 & bases_ind$CH03=="3"]-25
  bases_ind$CH06[bases_ind$CH06==-1 & bases_ind$CH03=="5"] <- bases_ind$Edad_jefe[bases_ind$CH06==-1 & bases_ind$CH03=="5"]-50
  bases_ind$CH06[bases_ind$CH06==-1 & bases_ind$CH03 %in% c("4","8","9","10")] <- bases_ind$Edad_jefe[bases_ind$CH06==-1 & bases_ind$CH03 %in% c("4","8","9","10")]
  bases_ind$CH06[bases_ind$CH06==-1 & bases_ind$CH03 %in% c("6","7")] <- bases_ind$Edad_jefe[bases_ind$CH06==-1 & bases_ind$CH03 %in% c("6","7")]+25
  bases_ind$CH06[bases_ind$CH06<0] <- 0
  
  names(bases_ind) <- c("CODUSU", "NRO_HOGAR", "COMPONENTE", "MAS_500", "REGION", "Parentesco", "Genero",
                        "Edad", "Nacimiento", "Resid_5_años",
                        "Ingreso", "Trimestre", "Estado", "NIVEL_ED", "n_trabaj_hogar", "Edad_jefe")
  
  # Poner nivel educativo en orden
  bases_ind$NIVEL_ED[bases_ind$NIVEL_ED==7] <- 0
  bases_ind$NIVEL_ED[bases_ind$NIVEL_ED==9] <- -1
  
  # Agregar variables del padre
  hogares_padres <- bases_ind %>%
    filter(Parentesco==6 & Genero==1) %>%
    group_by(CODUSU, NRO_HOGAR) %>%
    filter(NIVEL_ED==max(NIVEL_ED)) %>%
    filter(Trimestre==max(Trimestre)) %>%
    ungroup() %>%
    select(CODUSU, NRO_HOGAR, NIVEL_ED) %>%
    mutate(NIVEL_ED_p = NIVEL_ED) %>%
    select(-c(NIVEL_ED)) %>%
    mutate(Jefe=1, Hermano=8) %>%
    gather(aux, Parentesco, -c(CODUSU, NRO_HOGAR,NIVEL_ED_p)) %>%
    select(-aux)
  
  # Agregar variables de la madre
  hogares_madres <- bases_ind %>%
    filter(Parentesco==6 & Genero==2) %>%
    group_by(CODUSU, NRO_HOGAR) %>%
    filter(NIVEL_ED==max(NIVEL_ED)) %>%
    filter(Trimestre==max(Trimestre)) %>%
    ungroup() %>%
    select(CODUSU, NRO_HOGAR, NIVEL_ED) %>%
    mutate(NIVEL_ED_m = NIVEL_ED) %>%
    select(-c(NIVEL_ED)) %>%
    mutate(Jefe=1, Hermano=8) %>%
    gather(aux, Parentesco, -c(CODUSU, NRO_HOGAR, NIVEL_ED_m)) %>%
    select(-aux)
  
  # Agregar variables del suegro
  hogares_suegro <- bases_ind %>%
    filter(Parentesco==7 & Genero==1) %>%
    group_by(CODUSU, NRO_HOGAR) %>%
    filter(NIVEL_ED==max(NIVEL_ED)) %>%
    filter(Trimestre==max(Trimestre)) %>%
    ungroup() %>%
    select(CODUSU, NRO_HOGAR, NIVEL_ED) %>%
    mutate(NIVEL_ED_s1 = NIVEL_ED) %>%
    select(-c(NIVEL_ED)) %>%
    mutate(Parentesco=2) 
  
  # Agregar variables de la suegra
  hogares_suegra <- bases_ind %>%
    filter(Parentesco==7 & Genero==2) %>%
    group_by(CODUSU, NRO_HOGAR) %>%
    filter(NIVEL_ED==max(NIVEL_ED)) %>%
    filter(Trimestre==max(Trimestre)) %>%
    ungroup() %>%
    select(CODUSU, NRO_HOGAR, NIVEL_ED) %>%
    mutate(NIVEL_ED_s2 = NIVEL_ED) %>%
    select(-c(NIVEL_ED)) %>%
    mutate(Parentesco=2) 
  
  # Juntar todo
  bases_ind <- bases_ind %>%
    mutate(Parentesco=as.numeric(Parentesco)) %>%
    left_join(hogares_padres, by=c("CODUSU", "NRO_HOGAR", "Parentesco")) %>%
    left_join(hogares_madres, by=c("CODUSU", "NRO_HOGAR", "Parentesco")) %>%
    left_join(hogares_suegro, by=c("CODUSU", "NRO_HOGAR", "Parentesco")) %>%
    left_join(hogares_suegra, by=c("CODUSU", "NRO_HOGAR", "Parentesco"))
  
  # Quedarse con el padre con nivel educativo mas alto
  bases_ind$NIVEL_ED_p <- pmax(bases_ind$NIVEL_ED_p, bases_ind$NIVEL_ED_m,
                              bases_ind$NIVEL_ED_s1, bases_ind$NIVEL_ED_s2,  na.rm = TRUE)
  bases_ind <- bases_ind %>% select(-c(NIVEL_ED_m, NIVEL_ED_s1, NIVEL_ED_s2))
  
  # Agregar variables base hogar
  bases_hog <- rbind(T3_2018_h, T2_2018_h, T1_2018_h, T4_2017_h, T3_2017_h, T2_2017_h, T1_2017_h, T4_2016_h, T3_2016_h, T2_2016_h) %>%
    select(CODUSU, NRO_HOGAR, II7, IX_TOT, IX_MEN10, T) %>%
    mutate(II7 = as.factor(II7))
  names(bases_hog) <- c("CODUSU", "NRO_HOGAR", "Reg_Tenencia", "Miembros_hogar", "Menores_10", "Trimestre")
  
  # Juntar ambas bases y filtrar por ingreso y edad
  bases_eph <- bases_ind %>%
    left_join(bases_hog, by=c("CODUSU", "NRO_HOGAR", "Trimestre")) %>%
    filter(Ingreso>0) %>%
    filter(Edad>=30 & Edad<=59) %>%
    mutate(Genero = as.factor(Genero),
           NIVEL_ED_p = as.factor(NIVEL_ED_p)) %>%
    select(-c(Estado, Edad_jefe, Edad, NIVEL_ED, Parentesco))
  
  save(bases_eph, ingreso_medio, actualizar_ingreso, file="EPH_final.Rda")
}