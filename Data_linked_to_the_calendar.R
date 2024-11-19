setwd("C:/Users/TOURE/Mes documents/REPOSITORIES")
library(tidyverse)
library(dplyr)
library(sf)
library(flextable)
library(stringr)
library(stringi)
library(lubridate)
library(readxl)


# lqas <- read_csv("C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/AFRO_LQAS_data_c.csv")
polioutbrk <- read_excel("C:/Users/TOURE/Mes documents/REPOSITORIES/SCOPE/data.xlsx")
calendar <- read_csv("C:/Users/TOURE/Mes documents/REPOSITORIES/SCOPE/calendar.csv")

polioutbrk$Country[polioutbrk$Country == "CÔTE D’IVOIRE"] <- "COTE D IVOIRE"

Polioutbrk<-polioutbrk |>
  rename(roundn = `Round Number`,
         response = `OBR Name`,
         vaccine.type = Vaccines,
         country = Country,
         round_start_date = `Round Start Date`) |> 
  mutate(roundNumber = case_when(
    roundn =="Round 0"~ "Rnd0",
    roundn =="Round 1"~ "Rnd1",
    roundn =="Round 2"~ "Rnd2",
    roundn =="Round 3"~ "Rnd3",
    roundn =="Round 4"~ "Rnd4",
    roundn =="Round 5"~ "Rnd5",
    roundn =="Round 6"~ "Rnd6")) |> 
  dplyr::select(country, response, roundNumber, vaccine.type, round_start_date)

calend<-calendar |>
  rename(rndn = `Round Number`,
         response = `OBR Name`,
         country = `Admin 0`,
         province = `Admin 1`,
         district = `Admin 2`,
         vaccine.type = Vaccine) |> 
  mutate(roundNumber = case_when(
    rndn =="R0"~ "Rnd0",
    rndn =="R1"~ "Rnd1",
    rndn =="R2"~ "Rnd2",
    rndn =="R3"~ "Rnd3",
    rndn =="R4"~ "Rnd4",
    rndn =="R5"~ "Rnd5",
    rndn =="R6"~ "Rnd6"))  |> 
  dplyr::select(country, province, district, response, vaccine.type, roundNumber) |> 
  filter(vaccine.type !="NA")
calend$country[calend$country == "CÔTE D’IVOIRE"] <- "COTE D IVOIRE"
Calendare <- merge(x=calend, y=Polioutbrk,  
                      by=c("country" = "country",
                           "response" = "response",
                           "roundNumber" = "roundNumber"),
                  all.x = TRUE) |>
  mutate(round_start_date = as_date(round_start_date)) |> 
  mutate(month_yr = format(as.Date(round_start_date), "%Y-%m")) |> 
  mutate(
    province = case_when(
      province == "SIKASSO" & response =="MAI-2024-02-01_nOPV" & round_start_date == "2024-09-27"~ "KAYES",
      TRUE~ province)) |> 
  mutate( district = case_when(
    district =="SELINGUE" & response =="MAI-2024-02-01_nOPV" & round_start_date == "2024-09-27"~ "KENIEBA",
    district =="YANFOLILA" & response =="MAI-2024-02-01_nOPV" & round_start_date == "2024-09-27"~ "SAGABARI",
    TRUE~ district)) |> 
  mutate(response = case_when(
    response =="MAI-2024-02-01_nOPV" & round_start_date == "2024-09-27"~ "Small-scale SIA-2Ds",
    TRUE~ response)) |>
  mutate(round_start_date = case_when(
    response =="MAI-2024-02-01_nOPV" & round_start_date == "2024-09-27"~ as_date("2024-09-06"),
    response =="GHA-2024-10-01_nOPV" & roundNumber == "Rnd1"~ as_date("2024-10-17"),
    response =="DRC-sNID-09-2024_bOPV" & roundNumber == "Rnd1"~ as_date("2024-09-19"),
    response =="DRC-sNID-01-2024-bOPV" & roundNumber == "Rnd1"~ as_date("2024-02-02"),
    response =="DRC-NID-03-2024-nOPV" & roundNumber == "Rnd1"~ as_date("2024-03-28"),
    response =="CIV-2024-04-01_nOPV" & roundNumber == "Rnd1"~ as_date("2024-05-10"),
    response =="CIV-2024-04-01_nOPV" & roundNumber == "Rnd2"~ as_date("2024-09-06"),
    TRUE~ round_start_date)) |> 
  dplyr::select(country, province, district, response, vaccine.type = vaccine.type.x, roundNumber, round_start_date) |> 
  mutate(round_start_date = as_date(round_start_date)) |> 
  filter(response !="ZIM-2024-01-01_nOPV",
         response != "ANGOLA-NID2-2024-nOPV2",
         response != "ANG-2024-05-01_bOPV",
         response != "TNZ-2024-01-01_nOPV",
         response != "SLE-2024-09-NID_nOPV",
         response != "NIE-2024-02-01_bOPV",
         response != "DRC-NID-05-2024-nOPV",
         response != "CIV-2024-09-NIDs_nOPV",
         response != "CNG-2024-02-nOPV",
         response != "TEST") 
c<-Calendare |>
  mutate(month = month(round_start_date)) |>
  filter(month != "NA") |> 
  arrange(month)|>
  mutate(district = toupper(district)) |> 
  mutate(district = case_when(
    country	=="ANGOLA"&	district	=="N'HARÃŠA" ~	"NHAREA",
    country	=="ANGOLA"&	district	=="NGONGUEMBO" ~	"GONGUEMBO",
    country	=="ANGOLA"&	district	=="NÃ“QUI" ~	"NOQUI",
    country	=="ANGOLA"&	district	=="PANGO-ALUQUEM" ~	"PANGO ALUQUEM",
    country	=="ANGOLA"&	district	=="TÃ”MBUA (EX. PORTO ALEXANDRE)" ~	"TOMBUA",
    country	=="ANGOLA"&	district	=="UCUMA" ~	"UKUMA",
    country	=="ANGOLA"&	district	=="UÃGE" ~	"UIGE",
    country	=="ANGOLA"&	district	=="XÃ-MUTEBA" ~	"XA MUTEBA",
    country	=="ANGOLA"&	district	=="NZETU" ~	"NZETO",
    country	=="ANGOLA"&	district	=="CELA (EX. UACU-CUNGO)" ~	"CELA",
    country	=="ANGOLA"&	district	=="OMBADJA (EX. CUAMATO)" ~	"OMBADJA",
    country	=="ANGOLA"&	district	=="TCHICALA TCHOLOHANGA" ~	"TCHIKALA-TCHOLOHAN",
    country	=="ANGOLA"&	district	=="BUNDAS" ~	"LUMBALA NGUIMBO (BUNDAS)",
    country	=="ANGOLA"&	district	=="AMBOIM (EX. GABELA)" ~	"AMBOIM",
    country	=="ANGOLA"&	district	=="AMBUÃLA" ~	"AMBUILA",
    country	=="ANGOLA"&	district	=="BAÃA FARTA" ~	"BAIA FARTA",
    country	=="ANGOLA"&	district	=="BUENGAS (EX. NOVA ESPERANÃ‡A)" ~	"BUENGAS",
    country	=="ANGOLA"&	district	=="BULA-ATUMBA" ~	"BULA ATUMBA",
    country	=="ANGOLA"&	district	=="QUIUABA-N'ZOGI" ~	"KIWABA NZOGI",
    country	=="ANGOLA"&	district	=="SAMBA CAJÃš" ~	"SAMBA CAJU",
    country	=="ANGOLA"&	district	=="SELES (EX. UCU SELES)" ~	"SELES",
    country	=="ANGOLA"&	district	=="SUMBE (EX. NGUNZA)" ~	"SUMBE",
    country	=="ANGOLA"&	district	=="CAMEIA" ~	"LUMEJE (CAMEIA)",
    country	=="ANGOLA"&	district	=="CATABOLA (EX. NOVA SINTRA)" ~	"CATABOLA",
    country	=="ANGOLA"&	district	=="LÃ‰UA" ~	"LEUA",
    country	=="ANGOLA"&	district	=="LIBOLO (EX. CALULO)" ~	"LIBOLO",
    country	=="ANGOLA"&	district	=="LÃ“VUA" ~	"LOVUA",
    country	=="ANGOLA"&	district	=="BUNDAS-LUMBALA-NGUIMBO" ~	"LUMBALA NGUIMBO (BUNDAS)",
    country	=="ANGOLA"&	district	=="CAÃLA" ~	"CAALA",
    country	=="ANGOLA"&	district	=="CACONGO (EX. LÃ‚NDANA)" ~	"CACONGO",
    country	=="ANGOLA"&	district	=="DANDE (CAXITO)" ~	"DANDE",
    country	=="ANGOLA"&	district	=="DEMBOS-QUIBAXE" ~	"DEMBOS (QUIBAXE)",
    country	=="ANGOLA"&	district	=="GAMBOS (EX. CHIANGE)" ~	"GAMBOS",
    country	=="ANGOLA"&	district	=="CUNDA-DIA-BAZE" ~	"KUNDA-DIA-BAZE",
    country	=="ANGOLA"&	district	=="CUNHINGA (VOUGA)" ~	"CUNHINGA",
    country	=="ANGOLA"&	district	=="MUCABA (EX. QUINZALA)" ~	"MUCABA",
    country	=="ANGOLA"&	district	=="MUCARI" ~	"CACULAMA (MUCARI)",
    country	=="ANGOLA"&	district	=="TCHIKALA TCHOLOHANG" ~	"TCHIKALA-TCHOLOHAN",
    country	=="ANGOLA"&	district	=="CUROCA (EX. ONCOCUA)" ~	"CUROCA",
    country	=="ANGOLA"&	district	=="MILUNGA (SANTA CRUZ)" ~	"MILUNGA",
    country	=="ANGOLA"&	district	=="LUENA" ~	"MOXICO (LUENA)",
    country	=="NIGER"&	district	=="AGUIÃ‰" ~	"AGUIÉ",
    country	=="NIGE"&	district	=="TCHIROZÃ‰RINE" ~	"TCHIROZÉRINE",
    country	=="NIGER"&	district =="TÃ‰RA" ~	"TERA",
    country	=="NIGER"&	district	=="GOURÃ‰" ~	"GOURÉ",
    country	=="NIGER"&	district	=="IFÃ‰ROUANE" ~	"IFÉROUANE",
    country	=="NIGER"&	district	=="ILLÃ‰LA" ~	"ILLÉLA",
    country	=="NIGER"&	district	=="MATAMÃˆYE" ~	"MATAMEYE",
    country	=="NIGER"&	district	=="FILINGUÃ‰" ~	"FILINGUE",
    country	=="NIGER"&	district	=="KANTCHÃ‰" ~	"KANTCHÉ",
    country	=="NIGER"&	district	=="GOTHÃˆYE" ~	"GOTHÈYE",
    country	=="NIGER"&	district	=="JINJA CITY‰" ~	"JINJA",
    country	=="NIGER"&	district	=="MBALE CITY" ~	"MBALE",
    country	=="MAURITANIA"&	district	=="RIYADH" ~	"RIYAD",
    country	=="MAURITANIA"&	district	=="LEKSEIBE" ~	"LEXEIBA",
    country	=="MAURITANIA"&	district	=="WOMPOU" ~	"WOMPO",
    country	=="MAURITANIA"&	district	=="ADEL BAGHROU" ~	"ADEL BEGROU",
    country	=="MAURITANIA"&	district	=="AKJOUJET" ~	"AKJOUJT",
    country	=="MAURITANIA"&	district	=="BABABE" ~	"BABABÉ",
    country	=="MAURITANIA"&	district	=="BIR OUMGREINE" ~	"BIR MOGHREN",
    country	=="MAURITANIA"&	district	=="BOGHE" ~	"BOGHÉ",
    country	=="MAURITANIA"&	district	=="BARKEOL" ~	"BARKÉOLE",
    country	=="MAURITANIA"&	district	=="CHINGUITTI" ~	"CHINGUITTY",
    country	=="MAURITANIA"&	district	=="D_HAR" ~	"D'HAR",
    country	=="MAURITANIA"&	district	=="BOUTILIMITT" ~	"BOUTILIMIT",
    country	=="MAURITANIA"&	district	=="F_DERIK" ~	"F'DERICK",
    country	=="MAURITANIA"&	district	=="GUERROU" ~	"GUÉRROU",
    country	=="MAURITANIA"&	district	=="KANKOUSSA" ~	"KANKOSSA",
    country	=="MAURITANIA"&	district	=="KOBENNI" ~	"KOBENI",
    country	=="MAURITANIA"&	district	=="M_BAGNE" ~	"M'BAGNE",
    country	=="MAURITANIA"&	district	=="M_BOUT" ~	"M'BOUT",
    country	=="MAURITANIA"&	district	=="MAGHTA LEHJAR" ~	"MAGTA LAHJAR",
    country	=="MAURITANIA"&	district	=="MOUDJRIA" ~	"MOUDJÉRIA",
    country	=="MAURITANIA"&	district	=="NEMA" ~	"NÉMA",
    country	=="MAURITANIA"&	district	=="OUAD-NAGA" ~	"OUAD NAGA",
    country	=="MAURITANIA"&	district	=="R_KIZ" ~	"R'KIZ",
    country	=="MAURITANIA"&	district	=="SEILIBABY" ~	"SELIBABY",
    country	=="MAURITANIA"&	district	=="TAMCHEKETT" ~	"TAMCHAKET",
    country	=="MAURITANIA"&	district	=="TEVRAGH ZEINE" ~	"TEVRAGH ZEINA",
    country	=="MAURITANIA"&	district	=="TICHITT" ~	"TICHIT",
    country	=="MAURITANIA"&	district	=="TIMBEDRA" ~	"TIMBÉDRA",
    country	=="MAURITANIA"&	district	=="ZOUERATE" ~	"ZOUÉRAT",
    country	=="MOZAMBIQUE"&	district	=="CHIÃšRE" ~	"CHIÚRE",
    country	=="MOZAMBIQUE"&	district	=="MARÃVIA" ~	"MARÁVIA",
    country	=="MOZAMBIQUE"&	district	=="MAÃšA" ~	"MAUA",
    country	=="MOZAMBIQUE"&	district	=="ALTO MOLÃ“CUÃˆ" ~	"ALTO MOLOCUE",
    country	=="MOZAMBIQUE"&	district	=="ANGÃ“NIA" ~	"ANGONIA",
    country	=="MOZAMBIQUE"&	district	=="MOCÃMBOA DA PRAIA" ~	"MACIMBOA DA PRAI",
    country	=="MOZAMBIQUE"&	district	=="MÃGOÃˆ" ~	"MÁGOÈ",
    country	=="MOZAMBIQUE"&	district	=="GURUÃ‰" ~	"GURUE",
    country	=="MOZAMBIQUE"&	district	=="GILÃ‰" ~	"GILÉ",
    country	=="MOZAMBIQUE"&	district	=="NGAÃšMA" ~	"NGAÚMA",
    TRUE~district))
 
# GHA-2024-10-01_nOPV 2024-10-17
# DRC-sNID-09-2024_bOPV 2024-09-19
# DRC-sNID-01-2024_bOPV 2024-02-02 
# DRC-NID-03-2024-nOPV 2024-03-28
# CIV-2024-04-01_nOPV 2024-05-10
# CIV-2024-04-01_nOPV 2024-09-06
# 



#missed
#BFA-2024-08-sNID_nOPV 2024-09-06

write_csv(c,"C:/Users/TOURE/Mes documents/REPOSITORIES/SCOPE/scope_.csv")




