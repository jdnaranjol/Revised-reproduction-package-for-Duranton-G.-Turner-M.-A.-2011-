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


## dado que R no tiene la capacidad de reshape, se debe hacer por ada una de las variables.
## La siguinete fucnión, coge cada variable de la lista y le hace, un reshape a traves del indicador MSA y
## la información se separa por año
## las variables que no use son proque no cambian a nivel de observación, son efectos fijos por año, estas las trabajo ahora

fr <- function(x){
  base2 <- base1 %>% select(msa,starts_with(x))%>%pivot_longer( cols= starts_with(x),names_to = "year", names_prefix = x , values_to = x )
  
  return(base2)
}

## variables a las cuales le haré el proceso

names <- c("l_ln_km_IH_", "l_ln_km_IHU","l_ln_km_MRU",
           "l_vmt_IHU_","l_vmt_IHU", "l_vmt_MRU", "l_bus", 
           "l_transit","sprawl","S_somecollege","l_mean_income",
           "S_poor", "S_manuf","S_truck","l_pop_")


de <- map(names, ~fr(.x))


ba <- do.call(cbind,de)

duplicate <- duplicated(colnames(ba))
ba <- ba %>%select(unique(colnames(.)))

## Para poder construir la base, debo eliminar los años duplicados
## ahora la base 

names <- c("l_ln_km_IH", "l_ln_km_IHU","l_ln_km_MRU",
           "l_vmt_IHU","l_vmt_IHU", "l_vmt_MRU", "l_bus", 
           "l_transit","sprawl","S_somecollege","l_mean_income",
           "S_poor", "S_manuf","S_truck","l_pop")

gr <- c(1993,2003,1983)

names <- expand.grid(names,gr)%>% mutate(id=paste0(Var1,"_",Var2))

base21 <- base1 %>% select(-names$id)


### Unir la base con la trasnformación de cada variable y las variables que son efectos fijos

basef <- ba %>% full_join(base21)

## El proceso original, lo que cambia son los instrumentos utilziados y al especificación


## Estimación 1. Instrumentos: l_rail1898+ l_hwy1947+ l_pix1835

model1 <- basef %>% mutate(l_vmt = l_vmt_IHU,
                           l_ln  = l_ln_km_IHU)
a <- dummy_cols(model1, select_columns = 'year')


model <- plm(formula =  l_vmt ~ -1 +l_ln + year_1983 + year_1993  
             | year_1983 + year_1993+l_rail1898+ l_hwy1947+ l_pix1835 ,
             data=a,model = "fd" )

model1 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993  
              | year_1983 + year_1993+l_pop_+l_rail1898+ l_hwy1947+ l_pix1835 ,
              data=a,model = "fd" )
model2 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993  
              | year_1983 + year_1993+l_pop_+l_rail1898+ l_hwy1947+ l_pix1835 ,
              data=a,model = "fd" )

model3 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993+
                +div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+
                elevat_range_msa+ ruggedness_msa +heating_dd+cooling_dd +sprawl+
                | year_1983 + year_1993+l_pop_+ year_1983 + year_1993+
                +div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+l_rail1898+ l_hwy1947+ l_pix1835 ,
              data=a,model = "fd" )

model4 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993+
                elevat_range_msa+ ruggedness_msa +heating_dd+cooling_dd +sprawl+
                S_somecollege +l_mean_income +S_poor +S_manuf+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9
              | year_1983 + year_1993+elevat_range_msa+ ruggedness_msa +heating_dd+
                cooling_dd +sprawl+l_pop_+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+S_somecollege +l_mean_income +S_poor +S_manuf+
                l_rail1898+ l_hwy1947+ l_pix1835 ,
              data=a,model = "fd" )

model5 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993+
                elevat_range_msa+ ruggedness_msa +heating_dd+cooling_dd +sprawl+
                S_somecollege +l_mean_income +S_poor +S_manuf+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+l_pop80+l_pop70+l_pop60+ l_pop50+ l_pop40+ l_pop30+ l_pop20
              | year_1983 + year_1993+elevat_range_msa+ ruggedness_msa +heating_dd+
                cooling_dd +sprawl+l_pop_+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+S_somecollege +l_mean_income +S_poor +S_manuf+
                l_rail1898+ l_hwy1947+ l_pix1835+div5 +div6+ div7 +div8 +div9+l_pop80+l_pop70+
                l_pop60+ l_pop50+ l_pop40+ l_pop30+ l_pop20 ,
              data=a,model = "fd" )
## Estimación 2, instrumentos:l_hwy1947

model1 <- basef %>% mutate(l_vmt = l_vmt_IHU,
                           l_ln  = l_ln_km_IHU)
a <- dummy_cols(model1, select_columns = 'year')


model <- plm(formula =  l_vmt ~ -1 +l_ln + year_1983 + year_1993  
             | year_1983 + year_1993+l_hwy1947 ,
             data=a,model = "fd" )

model1 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993  
              | year_1983 + year_1993+l_pop_+l_hwy1947 ,
              data=a,model = "fd" )
model2 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993  
              | year_1983 + year_1993+l_pop_+l_hwy1947 ,
              data=a,model = "fd" )

model3 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993+
                +div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+
                elevat_range_msa+ ruggedness_msa +heating_dd+cooling_dd +sprawl+
                | year_1983 + year_1993+l_pop_+ year_1983 + year_1993+
                +div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+l_hwy1947 ,
              data=a,model = "fd" )

model4 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993+
                elevat_range_msa+ ruggedness_msa +heating_dd+cooling_dd +sprawl+
                S_somecollege +l_mean_income +S_poor +S_manuf+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9
              | year_1983 + year_1993+elevat_range_msa+ ruggedness_msa +heating_dd+
                cooling_dd +sprawl+l_pop_+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+S_somecollege +l_mean_income +S_poor +S_manuf+
                l_hwy1947 ,
              data=a,model = "fd" )

model5 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993+
                elevat_range_msa+ ruggedness_msa +heating_dd+cooling_dd +sprawl+
                S_somecollege +l_mean_income +S_poor +S_manuf+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+l_pop80+l_pop70+l_pop60+ l_pop50+ l_pop40+ l_pop30+ l_pop20
              | year_1983 + year_1993+elevat_range_msa+ ruggedness_msa +heating_dd+
                cooling_dd +sprawl+l_pop_+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+S_somecollege +l_mean_income +S_poor +S_manuf+
                l_hwy1947+div5 +div6+ div7 +div8 +div9+l_pop80+l_pop70+
                l_pop60+ l_pop50+ l_pop40+ l_pop30+ l_pop20 ,
              data=a,model = "fd" )

## Estimación 3, instrumentos:l_rail1898

model1 <- basef %>% mutate(l_vmt = l_vmt_IHU,
                           l_ln  = l_ln_km_IHU)
a <- dummy_cols(model1, select_columns = 'year')


model <- plm(formula =  l_vmt ~ -1 +l_ln + year_1983 + year_1993  
             | year_1983 + year_1993+l_rail1898 ,
             data=a,model = "fd" )

model1 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993  
              | year_1983 + year_1993+l_pop_+l_rail1898 ,
              data=a,model = "fd" )
model2 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993  
              | year_1983 + year_1993+l_pop_+l_rail1898 ,
              data=a,model = "fd" )

model3 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993+
                +div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+
                elevat_range_msa+ ruggedness_msa +heating_dd+cooling_dd +sprawl+
                | year_1983 + year_1993+l_pop_+ year_1983 + year_1993+
                +div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+l_rail1898 ,
              data=a,model = "fd" )

model4 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993+
                elevat_range_msa+ ruggedness_msa +heating_dd+cooling_dd +sprawl+
                S_somecollege +l_mean_income +S_poor +S_manuf+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9
              | year_1983 + year_1993+elevat_range_msa+ ruggedness_msa +heating_dd+
                cooling_dd +sprawl+l_pop_+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+S_somecollege +l_mean_income +S_poor +S_manuf+
                l_rail1898 ,
              data=a,model = "fd" )

model5 <- plm(formula =  l_vmt ~ -1 +l_ln +l_pop_+ year_1983 + year_1993+
                elevat_range_msa+ ruggedness_msa +heating_dd+cooling_dd +sprawl+
                S_somecollege +l_mean_income +S_poor +S_manuf+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+l_pop80+l_pop70+l_pop60+ l_pop50+ l_pop40+ l_pop30+ l_pop20
              | year_1983 + year_1993+elevat_range_msa+ ruggedness_msa +heating_dd+
                cooling_dd +sprawl+l_pop_+div1+ div2 +div3+ div4+ 
                div5 +div6+ div7 +div8 +div9+S_somecollege +l_mean_income +S_poor +S_manuf+
                l_rail1898+div5 +div6+ div7 +div8 +div9+l_pop80+l_pop70+
                l_pop60+ l_pop50+ l_pop40+ l_pop30+ l_pop20 ,
              data=a,model = "fd" )


