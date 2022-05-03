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
