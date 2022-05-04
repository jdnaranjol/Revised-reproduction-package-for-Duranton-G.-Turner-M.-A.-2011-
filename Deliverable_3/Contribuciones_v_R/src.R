#
rm(list = ls())
# Cargue de los paquetes
require(pacman)
p_load(argparse,janitor,tidyverse,ggplot2,sf,readxl,classInt,scales,stringi,data.table,writexl,openxlsx,
       haven)
require(plm)
#install.packages("plm")

#install.packages('fastDummies')
library('fastDummies')

dir <- here::here()
parser <- ArgumentParser()

parser$add_argument("--base",
                    default = paste0(dir,"/Deliverable_1/data/Duranton_Turner_AER_2010.dta"))

parser$add_argument("--output",
                    default = paste0(dir,"/Deliverable_2/output"))

args <- parser$parse_args()

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")


base <- read_stata(args$base)

## Crear variables, limpiar base de datos donde l_ln_km_IH_83!=0


base1 <- base %>% filter(l_ln_km_IH_83!=0)%>%
  rename( sprawl_1993=sprawl_1992, sprawl_1983=sprawl_1976, S_somecollege_1983=S_somecollege_80,
          S_somecollege_2003= S_somecollege_00, S_somecollege_1993=S_somecollege_90,
          S_poor_1983=S_poor_80,S_poor_1993=S_poor_90,S_poor_2003=S_poor_00,
          l_mean_income_1983=l_mean_income_80,l_mean_income_1993=l_mean_income_90,
          l_mean_income_2003=l_mean_income_00, S_manuf_1983=S_manuf83,
          S_manuf_1993=S_manuf93, S_manuf_2003=S_manuf03,S_truck_1983=S_truck83,
          S_truck_1993=S_truck93, S_truck_2003=S_truck03, l_bus_1983=l_max_84bus, 
          l_bus_1993=l_max_94bus,l_bus_2003=l_max_04bus,l_transit_1983=l_transit84,
          l_transit_1993=l_transit94,l_transit_2003=l_transit04,
          l_ln_km_IHU_1983=l_ln_km_IHU_83,l_ln_km_IHU_1993=l_ln_km_IHU_93,
          l_ln_km_IHU_2003=l_ln_km_IHU_03,l_ln_km_IH_1983=l_ln_km_IH_83,
          l_ln_km_IH_1993=l_ln_km_IH_93,l_ln_km_IH_2003=l_ln_km_IH_03,
          l_ln_km_MRU_1983=l_ln_km_MRU_83,l_ln_km_MRU_1993=l_ln_km_MRU_93,
          l_ln_km_MRU_2003=l_ln_km_MRU_03,l_vmt_IHU_1983=l_vmt_IHU_83,l_vmt_IHU_1993=l_vmt_IHU_93,
          l_vmt_IHU_2003=l_vmt_IHU_03,l_vmt_IH_1983=l_vmt_IH_83,l_vmt_IH_1993=l_vmt_IH_93,
          l_vmt_IH_2003=l_vmt_IH_03,l_vmt_MRU_1983=l_vmt_MRU_83,l_vmt_MRU_1993=l_vmt_MRU_93,
          l_vmt_MRU_2003=l_vmt_MRU_03)%>%
  mutate(sprawl_2003 = sprawl_1993,elevat_range_msa = elevat_range_msa/1000,
         ruggedness_msa = ruggedness_msa/1000,heating_dd = heating_dd/100,
         cooling_dd = cooling_dd/100, l_pop_1983 = l_pop80, 
         l_pop_1993 = l_pop90, l_pop_2003 = l_pop00)
