library(tidyverse)
library(scales)
library(gridExtra)
library(ggrepel)
library(glue)
library(gridExtra)
library(caTools)
library(ROCR)
library(pROC)
knitr::opts_chunk$set(warning = FALSE)
theme_set(theme_bw())


telecom <- read.csv("C:\\Users\\pinte\\Downloads\\telecom_users.csv", header = T, sep = "," , stringsAsFactors = F)

telecom %>% glimpse()

#REMOÇÃO DE COLUNAS

telecom <- telecom %>%
  select(-X)

telecom <- telecom %>%
  select(-customerID)


##CRIAÇÃO DE UMA LISTA DE VARIÁVEIS
##CATEGÓRICAS E NUMÉRICAS

categorical <- names(telecom)[c(1:4,6:17)]
numerical <- names(telecom)[c(5,18,20)]
numerical

##TRANSFORMANDO VARIÁVEIS CATEGÓRICAS EM FATORES

telecom <-
  telecom %>%
  mutate(across(all_of(categorical),as.factor),
         Churn = as.factor(Churn))


#VISUALIZAÇÃO DAS VARIÁVEIS CATEGÓRICAS


simple_barplot <- function(dta,catvar) {
  p <- 
    dta %>%
    count(!!sym(catvar)) %>%
    mutate(ordererVar = fct_reorder(!!sym(catvar),n),
           percent = n/sum(n)) %>%
    ggplot(aes(x = ordererVar,y = n)) + 
    geom_col(width = 0.5,color = "black", fill = "skyblue2") +
    labs(y = element_blank(), x = element_blank(), title = paste("Barplot de ",catvar)) +
    coord_flip() +
    theme(plot.title = element_text(color = "red", size = 10, face = "bold")) +
    geom_text(aes(label = paste0(round(percent,2)*100,"%")), hjust = 1)
  
  p 
}
telecom %>% glimpse()

# Plotting bar charts in groups of 4
for (i in 0:3){
  grid.arrange(simple_barplot(telecom,categorical[(i*4)+1]),
               simple_barplot(telecom,categorical[(i*4)+2]),
               simple_barplot(telecom,categorical[(i*4)+3]),
               simple_barplot(telecom,categorical[(i*4)+4]),
               
               ncol = 2)
}

###Conclusões

#                         Em relação às características do cliente:

# 1 - Género e Parceiro são distribuídos uniformemente
# 2 - a maioria dos clientes não são aposentados e 70% não têm Dependentes.

#                           Em relação aos serviços:

# 1 - A grande maioria dos clientes tem serviço telefónico
# 2 - Sobre ter de clientes com serviço telefónico ter múltiplas linhas
# 3 -     22% não têm serviço de Internet

#                          Em relação à segurança e apoio

# 1 - Para clientes com serviço de Internet, há uma proporção de cerca de 
#dois para um em clientes que não têm segurança Online, o mesmo para suporte Técnico
# 2 - Há mais clientes que não têm backup online e protecção de dispositivos cerca de 
#(34% a 44% divididos).  

#                           Em relação às capacidades de streaming:

# 1 - as duas variáveis parecem muito semelhantes, com proporções 
#aproximadamente iguais para os clientes que utilizam o serviço de streaming de Filmes
#e o serviço de streaming de TV.

#                            Em relação aos pagamentos

# 1 - 60% dos clientes optam pela facturação sem papel e o método de pagamento mais comum
#é o cheque electrónico.
# 2 -Cerca de metade dos clientes tem um contrato mensal, a outra metade é dividida 
#entre um contacto de 1 ano e um contacto de 2 anos.

#                                Em relação a variável alvo 

# 1 -  Há uma taxa de 27% de rotatividade, o que parece muito elevado para um serviço de telecomunicações.

####VISUALIZAÇÕES DAS VÁRIAVÉIS NUMÉRICAS    

numeric_explore <- function(dta,numvar) {
  summary(dta[numvar])
  
  numeric_plot <-
    dta %>%
    ggplot(aes_string(x = numvar)) + 
    geom_histogram(bins = 25,fill = "skyblue3",color = "black") + 
    labs(y = element_blank(), x = element_blank(), title = paste("Histograma e resumo de",numvar))
  
  numeric_summary <- 
    data.frame(as.list(summary(dta[numvar[1]],digits = 3)))
  
  grob_list = list(tableGrob(numeric_summary,rows = NULL,cols  = NULL),numeric_plot)
  grid.arrange(grobs = grob_list,
               layout_matrix = rbind(c(2),
                                     c(2),
                                     c(1)))
}

numeric_explore(telecom,numerical[1])
numeric_explore(telecom,numerical[2])
numeric_explore(telecom,numerical[3])




###Passo 2 - IDENTIFICAÇÃO DAS RELAÇÕES CATEGÓRICAS COM A VARIÁVEL DEPENDENTE
#### CHURN

catRelation <- function(dta,catVar) {
  
  
  temp_df <- 
    dta %>%
    select(!!sym(catVar),Churn)
  
  temp_pvalue <- round(chisq.test(table(temp_df))$p.value,4)
  temp_df <- as.data.frame(prop.table(table(temp_df),margin = 1))
  
  
  p <- temp_df %>%
    ggplot(aes(x = !!sym(catVar),y = Freq, fill = Churn)) +
    geom_col(color = "black") +
    labs(y = element_blank(),x = element_blank(),
         title = paste0("Churn and ",catVar),
         subtitle = paste0("P.value:",temp_pvalue)) +
    theme(legend.position = "none") +
    coord_flip()
  p
}

for (i in 0:7) {
  grid.arrange(catRelation(telecom,categorical[(i*2)+1]),
               catRelation(telecom,categorical[(i*2)+2]),
               ncol = 2)  
}

##Relação entre características categóricas - mapa térmico de correlações
temp = expand.grid(categorical,categorical)
temp <- temp %>%
  mutate(Var1 = as.character(Var1),
         Var2 = as.character(Var2))
for (i in 1:nrow(temp)) {
  temp[i,"p.value"] <-
    round(chisq.test(table(telecom %>%
                             select(!!sym(temp$Var1[i]),!!sym(temp$Var2[i]))))$p.value,4)
}



temp %>%
  ggplot(aes(x = Var1, y = Var2,fill = p.value)) +
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.35)) +
  labs(x = element_blank(), y = element_blank(),title = "Mapa de calor das variáveis categóricas",subtitle = "P.value é retirado do teste chisq.test de correlação") +
  scale_fill_continuous(type = "viridis")

###  TRANSFORMAÇÃO DO DATASET

#RENOMEAR AS COLUNAS DO DATASET

telecom <-
  telecom %>%
  rename("Sexo" = "gender",
         "Aposentado" = "SeniorCitizen",
         "Parceiro" = "Partner",
         "Tempo do cliente na empresa" = "tenure",
         "Serviço telefônico"= "PhoneService",
         "Linhas telefonicas" = "MultipleLines",
         "Serviço de internet" = "InternetService",
         "Serviço de segurança online"= "OnlineSecurity",
         "Serviço de Backup Online" = "OnlineBackup",
         "Seguro do equipamento"= "DeviceProtection",
         "Suporte técnico" = "TechSupport",
         "Sreaming de TV" = "StreamingTV",
         "Streaming de Filmes" = "StreamingMovies",
         "Faturamento sem papel" = "PaperlessBilling",
         "Contrato" = "Contract",
         "Método de pagamento" = "PaymentMethod",
         "Pagamento mensal atual" = "MonthlyCharges",
         "Valor total do serviço durante tempo de uso" = "TotalCharges",
         "Dependentes" = "Dependents")

telecom$Parceiro = as.character(factor(telecom$Parceiro,
                                       levels = c('Yes', 'No'),
                                       labels = c('0', '1')))

telecom$Aposentado = as.character(factor(telecom$Aposentado,
                                          levels = c('0', '1'),
                                          labels = c('0', '1')))

telecom$Sexo = as.character(factor(telecom$Sexo,
                                   levels = c('Male', 'Female'),
                                   labels = c('0', '1')))

telecom$`Serviço telefônico` = as.character(factor(telecom$`Serviço telefônico`,
                                                   levels = c('Yes', 'No'),
                                                   labels = c('0', '1')))

telecom$`Linhas telefonicas` = as.character(factor(telecom$`Linhas telefonicas`,
                                                   levels = c('Yes', 'No', 'No phone service'),
                                                   labels = c('0', '1', '2')))

telecom$`Serviço de internet` = as.character(factor(telecom$`Serviço de internet`,
                                                    levels = c('Fiber optic', 'No', 'DSL'),
                                                    labels = c('0', '1', '2')))

telecom$`Serviço de segurança online` = as.character(factor(telecom$`Serviço de segurança online`,
                                                            levels = c('Yes', 'No', 'No internet service'),
                                                            labels = c('0', '1', '2')))

telecom$`Serviço de Backup Online`= as.character(factor(telecom$`Serviço de Backup Online`,
                                                        levels = c('Yes', 'No', 'No internet service'),
                                                        labels = c('0', '1', '2')))

telecom$`Seguro do equipamento`= as.character(factor(telecom$`Seguro do equipamento`,
                                                     levels = c('Yes', 'No', 'No internet service'),
                                                     labels = c('0', '1', '2')))

telecom$`Suporte técnico`= as.character(factor(telecom$`Suporte técnico`,
                                               levels = c('Yes', 'No', 'No internet service'),
                                               labels = c('0', '1', '2')))

telecom$`Sreaming de TV`= as.character(factor(telecom$`Sreaming de TV`,
                                              levels = c('Yes', 'No', 'No internet service'),
                                              labels = c('0', '1', '2')))

telecom$`Streaming de Filmes`= as.character(factor(telecom$`Streaming de Filmes`,
                                                   levels = c('Yes', 'No', 'No internet service'),
                                                   labels = c('0', '1', '2')))
telecom$Contrato= as.character(factor(telecom$Contrato,
                                      levels = c('Month-to-month', 'One year', 'Two year'),
                                      labels = c('0', '1', '2')))

telecom$`Faturamento sem papel` = as.character(factor(telecom$`Faturamento sem papel`,
                                                      levels = c('Yes', 'No'),
                                                      labels = c('0', '1')))


telecom$`Método de pagamento`= as.character(factor(telecom$`Método de pagamento`,
                                                   levels = c('Credit card (automatic)', 'Bank transfer (automatic)', 
                                                              'Electronic check', 'Mailed check'),
                                                   labels = c('0', '1', '2', '3')))

telecom$Churn = as.character(factor(telecom$Churn,
                                    levels = c('Yes', 'No'),
                                    labels = c('0', '1')))

telecom$Dependentes = as.character(factor(telecom$Dependentes,
                                          levels = c('Yes', 'No'),
                                          labels = c('0', '1')))

##TRANSFORMANDO AS VARIÁVEIS QUE ERAM FATORES EM NUMÉRICAS
numerical <- names(telecom)[c(1:20)]

telecom <-
  telecom %>%
  mutate(across(all_of(numerical),as.numeric),
         Churn = as.numeric(Churn))
telecom %>% glimpse() 



##DIVISÃO DO CONJUNTO DE DADOS EM CONJUNTO DE TREINO E TESTE
set.seed(123)

divisãodaamostra = sample.split(telecom$Churn, SplitRatio = 0.80)
conjuntodetreino = subset(telecom, divisãodaamostra == T)
conjuntodeteste = subset(telecom, divisãodaamostra == F)
test.feature.vars <- telecom[,-20]
test.class.var <- telecom[,20]

indexes <- sample(1:nrow(telecom), size=0.6*nrow(telecom))
conjuntodetreino <- telecom[indexes,]
conjuntodeteste <- telecom[-indexes,]

#Adaptação do XGBoost ao conjunto de Formação
install.packages("xgboost")
library(xgboost)

classificador = xgboost(data = as.matrix(conjuntodetreino[-20]), label = conjuntodetreino$Churn, nrounds = 10, )
classificador
#PREVISÃO DOS RESULTADOS NO CONJUNTO DE TESTE
y_pred = predict(classificador, newdata = as.matrix(conjuntodeteste[-20]))
y_pred = (y_pred >= 0.5)

#MATRIZ DE CONFUSÃO
cm = table(conjuntodeteste[, 20], y_pred)

cm

#VALIDAÇÃO CRUZADA
library(caret)
folds = createFolds(conjuntodetreino$Churn, k = 10)
cv = lapply(folds, function(x) {
  training_fold = conjuntodetreino[-x, ]
  test_fold = conjuntodetreino[x, ]
  classificador = xgboost(data = as.matrix(training_fold[-20]), label = training_fold$Churn, nrounds = 10)
  y_pred = predict(classificador, newdata = as.matrix(test_fold[-20]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 20], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy

#################################################################

telecom$Churn <- as.factor(as.character(telecom$Churn))
# Roc Curvas
xgbpred <-predict(classificador, newdata = as.matrix(conjuntodeteste[-20], type = "logistic"))
xgbpred


xgbpred = (xgbpred >= 0.5)

rocxgb <- roc(conjuntodeteste$Churn, xgbpred)

ROC_xgb_auc <- auc(rocxgb)
  
plot(ROC_xgb_auc, col = "green", main = "ROC para gxboost")
paste("Acurácia de Modelo gxboost foi de: ", mean(conjuntodeteste$Churn == round(xgbpred, digits = 0)))
paste("Área sob curva do modelo gxboost foi de: ", ROC_xgb_auc)


