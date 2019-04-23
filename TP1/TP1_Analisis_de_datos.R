library("tidyverse")
library("readr")
library("knitr")
library("data.table")
library("openxlsx")
library("moments")
library("GGally")


# Cargar dataset ----------------------------------------------------------

hfi_cc_2018 <- read_csv("Data/hfi_cc_2018.csv")
#hfi_cc_2018 <- read.csv("G:/Personal/DiploDatos/hfi_cc_2018.csv")
hfi_cc_2018 <- read.csv("C:/Users/User02/Google Drive/DiploDatos/the-human-freedom-index/hfi_cc_2018.csv")

# Análisis Exploratorio ---------------------------------------------------
###PRUEBA
#aangelina

table(hfi_cc_2018$year, useNA = "always")
table(hfi_cc_2018$countries, useNA = "always")
table(hfi_cc_2018$region, useNA = "always")

#columnas: 
#important_cols = ['year', 'ISO_code', 'countries', 'region']
#important_cols += [col for col in dataset.columns if 'pf_identity' in col]
#important_cols += [
# 'ef_score', # Economic Freedom (score)
# 'ef_rank', # Economic Freedom (rank)
# 'hf_score', # Human Freedom (score)
# 'hf_rank', # Human Freedom (rank)
# ]

hfi <- hfi_cc_2018[, c(1:4, 53:61, 119:122)]

#1 - Estadisticos descriptivos
#1.1 - Rango de variables


T0 <- hfi %>%
  group_by(countries) %>%
  summarise_all(mean, na.rm = T)

T1 <- hfi %>%
  group_by(countries) %>%
  summarise(q = n())

T2 <- hfi %>%
  group_by(region, year) %>%
  summarise_all(mean, na.rm = T)


#con dplyr prueba

hfi %>% 
  select( starts_with("pf"),starts_with("hf")) %>% 
  sapply(., range, na.rm = TRUE) %>% 
  as.data.frame(rangos, row.names=c("min","max")) %>% 
  t()



#1.2 - Media, mediana, ds y moda de pf_identity y hf_score 
#mundo y Latinoamérica 

hfi$alca <- ifelse(hfi$region == "Latin America & the Caribbean",
                   "Am. Latina y el Caribe", "Resto del mundo")

T3 <- hfi %>%
  group_by(alca) %>%
  summarise(mediana_pf = round(median(pf_identity, na.rm = T), 2),
            media_pf = round(mean(pf_identity, na.rm = T), 2),
            desv_pf = round(sd(pf_identity, na.rm = T),2),
            mediana_hf = round(median(hf_score, na.rm = T),2),
            media_hf = round(mean(hf_score, na.rm = T),2),
            desv_hf = round(sd(hf_score, na.rm = T),2))

write.table(T3, "clipboard", sep = "\t")

g121 <- ggplot(hfi, aes(alca, pf_identity, fill= alca)) +
  geom_boxplot(show.legend = F) +
  xlab("América Latina y El Caribe y Resto del Mundo") + 
  ylab("Indice de Identidad (pf_identity)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))

g122 <- ggplot(hfi, aes(alca, hf_score, fill= alca)) +
  geom_boxplot(show.legend = F) +
  xlab("América Latina y El Caribe y Resto del Mundo") + 
  ylab("Indice de Libertad Humana (hf_score)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))



T4 <- hfi %>%
  group_by(region) %>%
  summarise(mediana_pf = round(median(pf_identity, na.rm = T), 2),
            media_pf = round(mean(pf_identity, na.rm = T), 2),
            desv_pf = round(sd(pf_identity, na.rm = T),2),
            mediana_hf = round(median(hf_score, na.rm = T),2),
            media_hf = round(mean(hf_score, na.rm = T),2),
            desv_hf = round(sd(hf_score, na.rm = T),2))

write.table(T4, "clipboard", sep = "\t")

round(median(hfi$pf_identity, na.rm = T), 2)
round(mean(hfi$pf_identity, na.rm = T), 2)
round(sd(hfi$pf_identity, na.rm = T),2)

round(median(hfi$hf_score, na.rm = T),2)
round(mean(hfi$hf_score, na.rm = T),2)
round(sd(hfi$hf_score, na.rm = T),2)


g123 <- ggplot(hfi, aes(region, pf_identity, fill= region)) +
  geom_boxplot(show.legend = F) +
  xlab("Region") + 
  ylab("Indice de Identidad (pf_identity)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


g124 <- ggplot(hfi, aes(region, hf_score, fill= region)) +
  geom_boxplot(show.legend = F) +
  xlab("Region") + 
  ylab("Indice de Libertad Humana (hf_score)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#1.5 - Outliers

g151 <- ggplot(hfi, aes("pf_identity", pf_identity)) +
  geom_boxplot(show.legend = F) +
  xlab("Todo el Mundo") + 
  ylab("Indice de Identidad (pf_identity)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))

hfi$year <- as.factor(hfi$year)

g152 <- ggplot(hfi, aes(year, pf_identity, fill = year)) +
  geom_boxplot(aes(group = year), show.legend = F) +
  xlab("Año") + 
  ylab("Indice de Identidad (pf_identity)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))


g153 <- ggplot(hfi, aes("hf_score", hf_score)) +
  geom_boxplot(show.legend = F) +
  xlab("Todo el Mundo") + 
  ylab("Indice de Libertad Humana (hf_score)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))


g154 <- ggplot(hfi, aes(year, hf_score, fill = year)) +
  geom_boxplot(aes(group = year), show.legend = F) +
  xlab("Año") + 
  ylab("Indice de Libertad Humana (hf_score)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))

summary(hfi[hfi$year == "2013", c("hf_score")])

summary(hfi[hfi$year == "2016", c("hf_score")])

#2 - Agregacion de datos
#2.1 - box plot con media

media <- aggregate(pf_identity ~ year, hfi, mean)
media$pf_identity <- round(media$pf_identity, 3)

media_hf <- aggregate(hf_score ~ year, hfi, mean)
media_hf$hf_score <- round(media_hf$hf_score, 3)

g211 <- ggplot(hfi, aes(year, pf_identity, fill = year)) +
  geom_boxplot(show.legend = F) +
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=18, size=1, show.legend = F) +
  geom_text(data = media, aes(label = pf_identity, y = pf_identity - 0.75)) +
  xlab("Año") + 
  ylab("Indice de Identidad (pf_identity)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))

g212 <- ggplot(hfi, aes(year, hf_score, fill = year)) +
  geom_boxplot(show.legend = F) +
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=18, size=1, show.legend = F) +
  geom_text(data = media_hf, aes(label = hf_score, y = hf_score + 0.3)) +
  xlab("Año") + 
  ylab("Indice de Libertad Humana (hf_score)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))

#Alternativa

medias <- merge(media, media_hf, by = "year")
#medias$year <- as.Date(medias$year, format= "%Y")
medias$year <- as.numeric(as.character(medias$year))

g213 <- medias %>%
  gather(key, value, pf_identity, hf_score) %>%
  ggplot(aes(year, value, colour = key)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = unique(medias$year)) +
  xlab("Año") + 
  ylab("Indice promedio") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))

#2.2 - Grrafico por variable y region

b221 <- hfi %>%
  group_by(year, region) %>%
  summarise(pf_identity = mean(pf_identity, na.rm = T))

b221 <-  b221 %>%
  gather(key, value, pf_identity)
b221$year <- as.numeric(as.character(b221$year))

g221 <- ggplot(b221, aes(year, value, colour = region)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = unique(b221$year)) +
  xlab("Año") + 
  ylab("Indice de Identidad Promedio") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))

b222 <- hfi %>%
  group_by(year, region) %>%
  summarise(hf_score = mean(hf_score, na.rm = T))

b222 <-  b222 %>%
  gather(key, value, hf_score)
b222$year <- as.numeric(as.character(b222$year))

g222 <- ggplot(b222, aes(year, value, colour = region)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = unique(b222$year)) +
  xlab("Año") + 
  ylab("Indice de Libertad Humana Promedio") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))

#2.3 Paises en latinoamerica - pf_identity

b23 <- hfi %>%
  filter(region == "Latin America & the Caribbean")
b23 <- b23[ , c("year", "countries", "pf_identity")] 

b23 <-  b23 %>%
  gather(key, value, pf_identity)
b23$year <- as.numeric(as.character(b23$year))

length(unique(b23$countries))
#26 paises de america latina, no tiene sentido graficarlo

# g23 <- ggplot(b23, aes(year, value, colour = countries)) +
#   geom_line(size = 1) +
#   scale_x_continuous(breaks = unique(b23$year)) +
#   xlab("Año") + 
#   ylab("Indice de Libertad en Identidad") + 
#   theme_classic() + 
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
#                                    vjust = 0.5))

#3 - Distribuciones
#3.1 - histograma distribución pf_identity global y Latinoamérica
## Ver: Utilice datos del año 2016, creo que no tiene sentido usar mezcla de años

y2016 <- hfi[hfi$year == 2016, c("alca", "pf_identity", "hf_score")]

# Overlaid histograms
g311 <- ggplot(y2016, aes(x=pf_identity, fill=alca)) +
  geom_histogram(aes(y=..density..),
                 binwidth=.5, 
                 alpha=.5, 
                 position="identity") + 
  geom_density(alpha=.3) +
  xlab("Indice de Libertad en Identidad") +
  theme_classic() 

g312 <- ggplot(y2016, aes(x=hf_score, fill=alca)) +
  geom_histogram(aes(y=..density..),
                 binwidth=.5, 
                 alpha=.5, 
                 position="identity") + 
  geom_density(alpha=.3) +
  xlab("Indice de Libertad Humana") +
  theme_classic() 

#3.2 - Test kolmogorov
#Gráficos Alternativos para probar normalidad en toda la variable

g321 <- ggplot(data=hfi, aes(hfi$pf_identity)) + 
  geom_histogram(aes(y = stat(density)),
                 bins = 15, 
                 col="lightblue", 
                 fill="lightblue") +
  stat_function(fun = dnorm,
                args = list(mean = mean(hfi$pf_identity, na.rm = T), 
                            sd = sd(hfi$pf_identity, na.rm = T)),
                lwd = 0.7,
                col = 'blue') +
  geom_density(col = "red") +
  labs(title = "Histogram Indice de Identidad") +
  labs(x="Indice de Identidad", y="Count") +
  theme(plot.title = element_text(hjust = 0.5))

g322 <- ggplot(data=hfi, aes(hfi$hf_score)) + 
  geom_histogram(aes(y = stat(density)),
                 bins = 15, 
                 col="lightblue", 
                 fill="lightblue") +
  stat_function(fun = dnorm,
                args = list(mean = mean(hfi$hf_score, na.rm = T), 
                            sd = sd(hfi$hf_score, na.rm = T)),
                lwd = 0.7,
                col = 'blue') +
  geom_density(col = "red") +
  labs(title = "Histogram Indice de Libertad Humana") +
  labs(x="Indice de Libertad", y="Count") +
  theme(plot.title = element_text(hjust = 0.5))

##Todos los años

ks.test(hfi$pf_identity, pnorm, 
        mean(hfi$pf_identity, na.rm = T), 
        sd(hfi$pf_identity, na.rm = T))

ks.test(hfi$hf_score, pnorm, 
        mean(hfi$hf_score, na.rm = T), 
        sd(hfi$hf_score, na.rm = T))

##Año 2016

ks.test(y2016$pf_identity, pnorm, 
        mean(y2016$pf_identity), 
        sd(y2016$pf_identity))

ks.test(y2016$hf_score, pnorm, 
        mean(y2016$hf_score), 
        sd(y2016$hf_score))

aml <- y2016[y2016$alca == "Am. Latina y el Caribe", ]

ks.test(aml$hf_score, pnorm, 
        mean(aml$hf_score), 
        sd(aml$hf_score))

#3.3 - qqplot

qqnorm(y2016$hf_score) 
qqline(y2016$hf_score)


qqnorm(aml$hf_score)
qqline(aml$hf_score)

qqnorm(hfi$hf_score)
qqline(hfi$hf_score)

qqnorm(hfi$pf_identity)
qqline(hfi$pf_identity)

#3.4 - Asimetria y Curtosis - 

skewness(hfi$pf_identity, na.rm = T) 
kurtosis(hfi$pf_identity, na.rm = T)

#pf_identity = asimétrica izquierda y 
#levemente más achatada que la distribución normal

skewness(hfi$hf_score, na.rm = T) 
kurtosis(hfi$hf_score, na.rm = T)

#hf_score = asimétrica izquierda y 
#levemente más achatada que la distribución normal

skewness(y2016$hf_score, na.rm = T) 
kurtosis(y2016$hf_score, na.rm = T)

#hf_score = asimétrica izquierda y 
#levemente más achatada que la distribución normal

g34 <- ggplot(data=y2016, aes(y2016$hf_score)) + 
  geom_histogram(aes(y = stat(density)),
                 bins = 15, 
                 col="lightblue", 
                 fill="lightblue") +
  stat_function(fun = dnorm,
                args = list(mean = mean(y2016$hf_score, na.rm = T), 
                            sd = sd(y2016$hf_score, na.rm = T)),
                lwd = 0.7,
                col = 'blue') +
  geom_density(col = "red") +
  labs(title = "Histogram Indice de Libertad Humana, año 2016") +
  labs(x="Indice de Libertad", y="Count") +
  theme(plot.title = element_text(hjust = 0.5))

#4 - Correlaciones
#4.1 - pf_identity, hf_score y ef_score

g41 <- ggpairs(hfi[, c("pf_identity", "hf_score", "ef_score")]) + 
  theme_classic()

#4.3 - Correlacion de Pearson, spearman, coeficientes de tau y de kendall

cor(hfi[, c("pf_identity", "hf_score", "ef_score")], 
    use= "complete.obs", method="pearson")

cor(hfi[, c("pf_identity", "hf_score", "ef_score")], 
    use= "complete.obs", method="spearman")

