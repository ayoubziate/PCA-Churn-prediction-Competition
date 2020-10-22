
library(data.table)
library(caret)
library(rpart)
library(MASS)
library(caTools)

data <- fread("C:/Users/Ayoub/Desktop/PCA Competition/PCAData.csv", sep=";", header=T)

# --------- Data scaling ----------- ( values arround 0)
# Malgrés normalisation le résultat est le meme
data$Nbr_total_cmd_2017 <- scale(data$Nbr_total_cmd_2017)
data$Nbr_total_cmd_2018 <- scale(data$Nbr_total_cmd_2018)
data$Nbr_total_cmd_2019 <- scale(data$Nbr_total_cmd_2019)

data$sales_net_2017 <- scale(data$sales_net_2017)
data$sales_net_2018 <- scale(data$sales_net_2018)
data$sales_net_2019 <- scale(data$sales_net_2019)

data$qte_produit_2017 <- scale(data$qte_produit_2017)
data$qte_produit_2018 <- scale(data$qte_produit_2018)
data$qte_produit_2019 <- scale(data$qte_produit_2019)

data$by_store <- scale(data$by_store)
data$by_phone <- scale(data$by_phone)
data$by_visit <- scale(data$by_visit)
data$by_online <- scale(data$by_online)
data$by_other <- scale(data$by_other)


data$recent_order_date <- NULL
#data$diff_days <- NULL

# -------------- Data partition --------------
inTrain <- createDataPartition(y = data$score, p=.75, list = FALSE)
dataTrain <- data[inTrain,]
dataTest <- data[-inTrain,]

## Model 1 Régression logistique
Model <- glm(score ~., family = "binomial", dataTrain)
summary(Model)

## Model 2 random forest
library(randomForest)
Model <- randomForest(score ~., data= dataTrain, proximity = TRUE)
Model

## Model 3 QDA
library(MASS)
Model <- qda(score ~., data = dataTrain)

## Model 4 LDA
Model <- lda(score ~., data = dataTrain)


## Model prediction
#dataTest$client_id <- NULL
Prediction <- predict(Model, data, type = 'response')

## Generate ROC curve
AUC <- colAUC(Prediction, data$score, plotROC = T)
abline(h=AUC, col = 'Green')
text(.2, .9, cex = .8, labels = paste("Optimal cutoff:", round(AUC, 4)))

## Convert probabilities to classes
Class <- ifelse(Prediction > 0.9902, 1, 0)

# Transform
Class <- factor(Class)
data$score <- factor(data$score)

## ConfusionMatrix
confusionMatrix(Class, data$score)

# ------------ OutPut ------------
client_id <- data$client_id
output.df <- as.data.frame(client_id)
output.df$score <- Class

summary(output.df)

write.csv(output.df, file = "sales_test.csv", row.names = FALSE)


# ---------- submission ---------
library(httr)
library(R.utils)
submit_prediction <- function(predictions, comment = '') {
  #write the dataset with predictions on your current directory
  token <- '77731b8bd74d1cae4334f35561698d10b9ae5eede9f71d690f97eb08552d8b2da65feacfc43d4fed1c90d3e5145ff4b5109109ec86ab2cb378f5fc95d916f67c'
  url <- 'http://40.66.49.56/api/submissions'
  f <- write.csv(predictions, file = 'temporary.csv', row.names = F)
  fgzip <- gzip('temporary.csv', overwrite=TRUE)
  response <- POST(url = url,
                   add_headers(Authorization = paste0('Bearer ', token)),
                   body = list(datafile = upload_file('temporary.csv.gz'),
                               compression = 'gzip',
                               comment = comment)
  )
  
  if (response$status_code == 429) {
    stop(sprintf('Submissions are too close. Next submission is only allowed in %s seconds.',
                 ceiling(strtoi(response$headers$'x-rate-limit-remaining') / 1000.0)
    ))
  }
  else if (response$status_code != 200) {
    stop(content(response, type = 'text'))
  }
}
submit_prediction(output.df)