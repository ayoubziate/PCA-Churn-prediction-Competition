start.time <- Sys.time()

library(data.table)
testdata <- fread("C:/Users/Ayoub/Desktop/PCA Competition/train.csv", sep=";", header=T)[,2:9]
head(testdata)
summary(testdata)
factor(testdata$client_id)
# export data
write.table(pd_left_join1, file = "PCAData.csv", row.names = F, sep = ";")

# on remarque que la colonnes 'sales_net' contient des valeurs négatives se qui est impossible
# donc on supprime toutes les lignes qui contiennent une valeur négatif
#cleanData <- testdata[! testdata$sales_net<=0,]
cleanData <- testdata
summary(cleanData)
# order_channel = {'at the store', 'by phone', 'during the visit of a sales rep', 'online', 'other'}
factor(cleanData$order_channel)
# branch_id = 546 région
factor(cleanData$branch_id)
# dataset contient 48,717 clients
factor(cleanData$client_id)

# transform date column from char to Date type
cleanData$date_order<- as.Date(cleanData$date_order, format = "%Y-%m-%d")
cleanData$date_invoice<- as.Date(cleanData$date_invoice, format = "%Y-%m-%d")


# ------ Tmax = "2019-05-31"
#library(sqldf)
#Tmax <- sqldf("select MAX(date_order) from cleanData;")
#Tmax
library(data.table)
setDT(cleanData)[, date_order := as.IDate(date_order)]
cleanData[, as.list(range(date_order)), by = client_id] # the max date and min date for each client
min(cleanData$date_order)
# date difference in days
as.Date(as.character(cleanData$date_invoice[1:5]), format="%Y-%m-%d") - as.Date(as.character(cleanData$date_order[1:5]), format="%Y-%m-%d")
as.Date("2019-05-31") - as.Date(cleanData$date_order)
# ---- table de clients, date_max d'ordre, différence entre Tmax et date_max
tableclient <- cleanData[, max(date_order), by = client_id]
# Ajouter Time differences in days
tableclient$diff_days <- as.Date("2019-05-31") - as.Date(tableclient$V1)
tableclient$score <- ifelse(tableclient$diff_days>60, 1, 0)
# change column name
colnames(tableclient)[2] <- "recent_order_date"

# ------- ETL de la table 'tableclient' ----------
library(dplyr)
count(cleanData, vars = "client_id")
library(sqldf)
# retourne Nombre total de cmd par clients
td = sqldf("SELECT client_id, count(date_order) as 'Nbr_total_cmd' 
           FROM cleanData
           WHERE date_order >= '2017-09-22'
           GROUP BY client_id;")

cleanData$date_order = as.character(cleanData$date_order)
# Total de cmd effectuer par chaque client en 2017
tdt = sqldf("SELECT client_id, count(date_order) as 'Nbr_total_cmd_2017' 
           FROM cleanData 
           WHERE date_order < '2018-01-01' AND date_order >= '2017-09-22'
           GROUP BY client_id;")
   # insertion en 'tableclient'
pd_left_join1 <- left_join(tableclient, tdt, by="client_id")
pd_left_join1$Nbr_total_cmd_2017[which(is.na(pd_left_join1$Nbr_total_cmd_2017))] <- 0 #Substitutes NA values for 0 in that column

# Total de cmd effectuer par chaque client en 2017
tdt = sqldf("SELECT client_id, count(date_order) as 'Nbr_total_cmd_2018' 
           FROM cleanData 
           WHERE date_order < '2019-01-01' AND date_order >= '2018-01-01'
           GROUP BY client_id;")
   # insertion en 'tableclient'
pd_left_join1 <- left_join(pd_left_join1, tdt, by="client_id")
pd_left_join1$Nbr_total_cmd_2018[which(is.na(pd_left_join1$Nbr_total_cmd_2018))] <- 0 #Substitutes NA values for 0 in that column

# Total de cmd effectuer par chaque client en 2017
tdt = sqldf("SELECT client_id, count(client_id) as 'Nbr_total_cmd_2019' 
           FROM cleanData 
           WHERE date_order >= '2019-01-01'
           GROUP BY client_id;")
   # insertion en 'tableclient'
pd_left_join1 <- left_join(pd_left_join1, tdt, by="client_id")
pd_left_join1$Nbr_total_cmd_2019[which(is.na(pd_left_join1$Nbr_total_cmd_2019))] <- 0 #Substitutes NA values for 0 in that column

# delete a column
#tableclient$Nbr_total_cmd_2019 <- NULL

# Total de ventes effectuer par chaque client en 2017
tdt = sqldf("SELECT client_id, SUM(sales_net) as 'sales_net_2017'
            FROM cleanData
            WHERE date_order < '2018-01-01' AND date_order >= '2017-09-22'
            GROUP BY client_id;")
# insertion en 'pd_left_join1'
pd_left_join1 <- left_join(pd_left_join1, tdt, by="client_id")
pd_left_join1$sales_net_2017[which(is.na(pd_left_join1$sales_net_2017))] <- 0 #Substitutes NA values for 0 in that column

# Total de ventes effectuer par chaque client en 2018
tdt = sqldf("SELECT client_id, SUM(sales_net) as 'sales_net_2018'
            FROM cleanData
            WHERE date_order < '2019-01-01' AND date_order >= '2018-01-01'
            GROUP BY client_id;")
# insertion en 'pd_left_join1'
pd_left_join1 <- left_join(pd_left_join1, tdt, by="client_id")
pd_left_join1$sales_net_2018[which(is.na(pd_left_join1$sales_net_2018))] <- 0 #Substitutes NA values for 0 in that column

# Total de ventes effectuer par chaque client en 2019
tdt = sqldf("SELECT client_id, SUM(sales_net) as 'sales_net_2019'
            FROM cleanData
            WHERE date_order >= '2019-01-01'
            GROUP BY client_id;")
# insertion en 'pd_left_join1'
pd_left_join1 <- left_join(pd_left_join1, tdt, by="client_id")
pd_left_join1$sales_net_2019[which(is.na(pd_left_join1$sales_net_2019))] <- 0 #Substitutes NA values for 0 in that column

# Total de quantité de prods commander par chaque client en 2017
tdt = sqldf("SELECT client_id, SUM(quantity) as 'qte_produit_2017'
            FROM cleanData
            WHERE date_order < '2018-01-01' AND date_order >= '2017-09-22'
            GROUP BY client_id;")
# insertion en 'pd_left_join1'
pd_left_join1 <- left_join(pd_left_join1, tdt, by="client_id")
pd_left_join1$qte_produit_2017[which(is.na(pd_left_join1$qte_produit_2017))] <- 0 #Substitutes NA values for 0 in that column

# Total de quantité de prods commander par chaque client en 2018
tdt = sqldf("SELECT client_id, SUM(quantity) as 'qte_produit_2018'
            FROM cleanData
            WHERE date_order < '2019-01-01' AND date_order >= '2018-01-01'
            GROUP BY client_id;")
# insertion en 'pd_left_join1'
pd_left_join1 <- left_join(pd_left_join1, tdt, by="client_id")
pd_left_join1$qte_produit_2018[which(is.na(pd_left_join1$qte_produit_2018))] <- 0 #Substitutes NA values for 0 in that column

# Total de quantité de prods commander par chaque client en 2019
tdt = sqldf("SELECT client_id, SUM(quantity) as 'qte_produit_2019'
            FROM cleanData
            WHERE date_order >= '2019-01-01'
            GROUP BY client_id;")
# insertion en 'pd_left_join1'
pd_left_join1 <- left_join(pd_left_join1, tdt, by="client_id")
pd_left_join1$qte_produit_2019[which(is.na(pd_left_join1$qte_produit_2019))] <- 0 #Substitutes NA values for 0 in that column

# transformer le type de la variable order_channel de categorical to numeric 
library(varhandle)
binary_order_channel <- to.dummy(cleanData$order_channel, "order_channel")
head(binary_order_channel[,1])
cleanData$order_channel.other <- NULL
cleanData$at_the_store = binary_order_channel[,1]
cleanData$by_phone = binary_order_channel[,2]
cleanData$during_the_visit_of_a_sales_rep = binary_order_channel[,3]
cleanData$online = binary_order_channel[,4]
cleanData$other = binary_order_channel[,5]

tdt = sqldf("SELECT client_id, SUM(at_the_store) as 'by_store', SUM(by_phone) as 'by_phone', SUM(during_the_visit_of_a_sales_rep) as 'by_visit', SUM(online) as 'by_online', SUM(other) as 'by_other'
            FROM cleanData
            GROUP BY client_id;")
# insertion en 'pd_left_join1'
pd_left_join1 <- left_join(pd_left_join1, tdt, by="client_id")
pd_left_join1$qte_produit_2019[which(is.na(pd_left_join1$qte_produit_2019))] <- 0 #Substitutes NA values for 0 in that column

# Combien de différent régions par client

# ---------- Let's goo ---------

require(FactoMineR)
library(Amelia)
library(rpart)
library(caTools)
library(lattice)
library(ggplot2)
library(caret)
library(sqldf)
library(MASS)
library(ROCR)

barplot(prop.table(table(pd_left_join1$score)), 
        col = rainbow(2),
        ylim = c(0,1),
        main="Distribution des classes")

#proportion
#        0         1 
#0.5166892 0.4833108 
prop.table(table(pd_left_join1$score))

hist(pd_left_join1$by_visit, freq=FALSE, breaks="Sturges", col="darkgray")

set.seed(123)
data = downSample(x=pd_left_join1[, -ncol(pd_left_join1)], y=factor(pd_left_join1$score))
prop.table(table(data$score))

split = sample.split(data$score, SplitRatio = 0.7)

TrainingData = subset(data, split == TRUE)    # <------ le problème
prop.table(table(TrainingData$score))
TestData = subset(data, split == FALSE)
prop.table(table(TestData$score))
head(TestData)

#-------- Model work --------#

library(e1071)
mymodel = svm(score~., data = TrainingData, kernel = "radial")
summary(mymodel)
pred = predict(mymodel, TestData)
tab = table(Predis = pred, Réel = TestData$score)
tab
#précision
sum(diag(tab))/sum(tab)


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# ----- Big data work ------

library(bigmemory)
library(biganalytics)

start.time <- Sys.time()

datab <- read.big.matrix("C:/Users/Ayoub/Desktop/PCA Competition/train.csv", sep=";", header=T)
head(datab)
dim(datab)
# y a pas de missing data
#library(Amelia)
#missmap(testdata)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# ----- first try --------
trainData <- read.csv("C:/Users/Ayoub/Desktop/PCA Competition/train.csv", sep=";", header=T)
head(trainData['date_order'])