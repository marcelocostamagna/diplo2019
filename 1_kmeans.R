library(tidyverse)
library(RCurl)
library(GGally)
library(gridExtra)

#K-means retail
#idea: armar una base con caracteristicas agrupadas del store y clasificarlos

#https://raw.githubusercontent.com/maxiarmesto/retail_data_analytics/master/sales.csv

#https://raw.githubusercontent.com/maxiarmesto/retail_data_analytics/master/stores.csv

#https://raw.githubusercontent.com/maxiarmesto/retail_data_analytics/master/features.csv

x <- getURL("https://raw.githubusercontent.com/maxiarmesto/retail_data_analytics/master/sales.csv")
sales <- read.csv(text = x)

x <- getURL("https://raw.githubusercontent.com/maxiarmesto/retail_data_analytics/master/stores.csv")
stores <- read.csv(text = x)

x <- getURL("https://raw.githubusercontent.com/maxiarmesto/retail_data_analytics/master/features.csv")
features <- read.csv(text = x)

table(sales$Dept, sales$Store, useNA = "always")

length(unique(sales$Dept))

##Existen ventas repetidas
# 8452 duplicaddos

dup <- sales %>%
  group_by(Sale.Id) %>%
  mutate(q = n()) %>%
  filter(q > 1)

sales <- sales %>%
  distinct(Sale.Id, .keep_all = T)

#Cantidad de semanas de información por store y departamento 

T1 <- sales %>%
  group_by(Store, Dept) %>%
  summarise(q = n())

#Cantdad de departamentos, fecha min y max

sales$Date1 <- as.Date(as.character(sales$Date), format = "%d/%m/%Y")

T2 <- sales %>%
  group_by(Store) %>%
  summarise(dptos = n_distinct(Dept),
            sem = n(),
            fi = min(Date1),
            ff = max(Date1),
            total_ventas = sum(Weekly_Sales, na.rm = T),
            ventas_prom_sem = round(total_ventas/sem, 0))

##Base para Cluster

stores <- merge(stores, T2, by = "Store", all.x = T)

##Se verifican 6 casos sin tamaño => se imputa por la media del tipo

size <- stores %>%
  group_by(Type) %>%
  summarise(prom = round(mean(Size, na.rm = T), 0))

stores$Size1 <- ifelse(stores$Type == "A" & is.na(stores$Size) == T, as.numeric(size[1,2]),
                       ifelse(stores$Type == "B" & is.na(stores$Size) == T, as.numeric(size[2,2]),
                              ifelse(stores$Type == "C" & is.na(stores$Size) == T, as.numeric(size[3,2]),
                                     stores$Size)))

#Cluster 1: Todas las variables sin estandarizar con 3 cluster
set.seed(20)

Cluster_1 <- kmeans(stores[, c(5, 6, 9:11)], 3, nstart = 20)
Cluster_1

### ejemplo de K-means:
  
table(Cluster_1$cluster, stores$Type)

Cluster_1$cluster <- as.factor(Cluster_1$cluster)

c1_1 <- ggplot(stores, aes(ventas_prom_sem, dptos, color = Cluster_1$cluster)) + 
  geom_point()

c1_2 <- ggplot(stores, aes(ventas_prom_sem, sem, color = Cluster_1$cluster)) + 
  geom_point()

c1_3 <- ggplot(stores, aes(ventas_prom_sem, total_ventas, color = Cluster_1$cluster)) + 
  geom_point()

c1_4 <- ggplot(stores, aes(ventas_prom_sem, Size1, color = Cluster_1$cluster)) + 
  geom_point()

c1_5 <- ggplot(stores, aes(total_ventas, dptos, color = Cluster_1$cluster)) + 
  geom_point()

c1_6 <- ggplot(stores, aes(total_ventas, sem, color = Cluster_1$cluster)) + 
  geom_point()

c1_7 <- ggplot(stores, aes(total_ventas, Size1, color = Cluster_1$cluster)) + 
  geom_point()

c1_8 <- ggplot(stores, aes(dptos, sem, color = Cluster_1$cluster)) + 
  geom_point()

c1_9 <- ggplot(stores, aes(dptos, Size1, color = Cluster_1$cluster)) + 
  geom_point()

c1_10 <- ggplot(stores, aes(Size1, sem, color = Cluster_1$cluster)) + 
  geom_point()

grid.arrange(c1_1, c1_2, c1_3, c1_4, c1_5, c1_6, c1_7, c1_8, c1_9, c1_10, 
             ncol=3, nrow =4)

##Cluster 2: Elimino deptos y sem

set.seed(20)

Cluster_2 <- kmeans(stores[, c(9:11)], 3, nstart = 20)
Cluster_2

table(Cluster_2$cluster, stores$Type)

Cluster_2$cluster <- as.factor(Cluster_2$cluster)

c2_1 <- ggplot(stores, aes(ventas_prom_sem, total_ventas, color = Cluster_2$cluster)) + 
  geom_point()

c2_2 <- ggplot(stores, aes(ventas_prom_sem, Size1, color = Cluster_2$cluster)) + 
  geom_point()

c2_3 <- ggplot(stores, aes(Size1, total_ventas, color = Cluster_2$cluster)) + 
  geom_point()

grid.arrange(c2_1, c2_2, c2_3, ncol=3, nrow =1)

##Cluster 3: Estandarizo las variables
stores.st <- scale(stores[, c(5,6, 9:11)])
  
set.seed(20)

Cluster_4 <- kmeans(stores.st, 3, nstart = 20)
Cluster_4

table(Cluster_4$cluster, stores$Type)

Cluster_4$cluster <- as.factor(Cluster_4$cluster)

stores.st <- as.data.frame(stores.st)

c1_1 <- ggplot(stores.st, aes(ventas_prom_sem, dptos, color = Cluster_4$cluster)) + 
  geom_point()

c1_2 <- ggplot(stores.st, aes(ventas_prom_sem, sem, color = Cluster_4$cluster)) + 
  geom_point()

c1_3 <- ggplot(stores.st, aes(ventas_prom_sem, total_ventas, color = Cluster_4$cluster)) + 
  geom_point()

c1_4 <- ggplot(stores.st, aes(ventas_prom_sem, Size1, color = Cluster_4$cluster)) + 
  geom_point()

c1_5 <- ggplot(stores.st, aes(total_ventas, dptos, color = Cluster_4$cluster)) + 
  geom_point()

c1_6 <- ggplot(stores.st, aes(total_ventas, sem, color = Cluster_4$cluster)) + 
  geom_point()

c1_7 <- ggplot(stores.st, aes(total_ventas, Size1, color = Cluster_4$cluster)) + 
  geom_point()

c1_8 <- ggplot(stores.st, aes(dptos, sem, color = Cluster_4$cluster)) + 
  geom_point()

c1_9 <- ggplot(stores.st, aes(dptos, Size1, color = Cluster_4$cluster)) + 
  geom_point()

c1_10 <- ggplot(stores.st, aes(Size1, sem, color = Cluster_4$cluster)) + 
  geom_point()

grid.arrange(c1_1, c1_2, c1_3, c1_4, c1_5, c1_6, c1_7, c1_8, c1_9, c1_10, 
             ncol=3, nrow =4)

##Cluster 5: Estandarizo las variables
stores.st <- scale(stores[, c(9:11)])

set.seed(20)

Cluster_5 <- kmeans(stores.st, 3, nstart = 20)
Cluster_5

table(Cluster_5$cluster, stores$Type)

Cluster_5$cluster <- as.factor(Cluster_5$cluster)

stores.st <- as.data.frame(stores.st)

c2_1 <- ggplot(stores.st, aes(ventas_prom_sem, total_ventas, color = Cluster_5$cluster)) + 
  geom_point()

c2_2 <- ggplot(stores.st, aes(ventas_prom_sem, Size1, color = Cluster_5$cluster)) + 
  geom_point()

c2_3 <- ggplot(stores.st, aes(Size1, total_ventas, color = Cluster_5$cluster)) + 
  geom_point()

grid.arrange(c2_1, c2_2, c2_3, ncol=3, nrow =1)

###NUMERO DE CLUSTER
stores.st <- scale(stores[, c(9:11)])

wssplot <- function(data, nc=10, seed=20){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Numero de Clusters",
       ylab="Within groups sum of squares")}

wssplot(stores.st, nc=5) 
