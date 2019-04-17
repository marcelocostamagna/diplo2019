library("tidyverse")
library("readr")
library("knitr")


# Cargar dataset ----------------------------------------------------------

hfi_cc_2018 <- read_csv("Data/hfi_cc_2018.csv")

# Análisis Exploratorio ---------------------------------------------------

summary(hfi_cc_2018 )
