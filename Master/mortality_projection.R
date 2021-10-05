########################################
####### Dominik Tyszkowski 68033 #######
########## Praca magisterska ###########
########################################

setwd('...//GitHub//')
options(warn=-1)

####### Uzyte biblioteki #######

library(tidyverse)
library(data.table)
library(keras)
library(tfruns)
library(dplyr)
library(ggplot2)
library(splitstackshape)
library(writexl)
library(forecast)
library(readxl)
library(gridExtra)
library(varhandle)

######## Import bazy danych ########

dta <- read.csv2('mortality.csv', sep = ',')
dta$log_mortality <- as.numeric(dta$log_mortality)
dta$Mortality <- exp(dta$log_mortality)

countries <- as.data.frame(unique(dta$Country))
Countries <- data.frame(Country = c("CHE","DEUT","DNK", "ESP", "FRATNP", "ITA", "JPN", "POL", "USA"),
                        Country_name = c("Szwajcaria", "Niemcy", "Dania", "Hiszpania", "Francja", "Włochy", "Japonia", "Polska", "USA"))
Countries2 <- Countries

for (i in 1:nrow(Countries)) {
 x <- Countries$Country[i]
 Countries$startyear[i] <- min(dta$Year[dta$Country == x])  
 Countries$endyear[i] <- max(dta$Year[dta$Country == x])
 y <- plyr::count(unique(dta$Year[dta$Country == x]))
 Countries$obs[i] <- nrow(dta[dta$Country == x,])
 Countries$Male[i] <- nrow(dta[dta$Country == x & dta$Gender == 'Male',])
 Countries$Female[i] <- nrow(dta[dta$Country == x & dta$Gender == 'Female',])
 Countries$Gender_check[i] <- Countries$obs[i] - Countries$Male[i] - Countries$Female[i] 
 Countries$Age_obs[i] <- nrow(dta[dta$Country == x,]) / (2 * (Countries$endyear[i] - Countries$startyear[i] + 1)) 
 cat('Podsumowanie zmiennych dla państwa',Countries$Country_name[Countries$Country == x], '\n')
 print(summary(dta[dta$Country == x,]))
 }

data <- filter(dta,Country %in% c("DNK","DEUT","ESP","POL"))
data <- data[data$Year>=1958,]

data$Płeć <- data$Gender
data$Płeć[data$Płeć == 'Female'] <- "Kobieta" 
data$Płeć[data$Płeć == 'Male'] <- "Mężczyzna" 
data <- inner_join(data, Countries2, by = 'Country')

# 1958
summary(data[data$Country == "DEUT" & data$Year == 1958,])
summary(data[data$Country == "DNK" & data$Year == 1958,])
summary(data[data$Country == "ESP" & data$Year == 1958,])
summary(data[data$Country == "POL" & data$Year == 1958,])
# 1987
summary(data[data$Country == "DEUT" & data$Year == 1987,])
summary(data[data$Country == "DNK" & data$Year == 1987,])
summary(data[data$Country == "ESP" & data$Year == 1987,])
summary(data[data$Country == "POL" & data$Year == 1987,])
# 2016
summary(data[data$Country == "DEUT" & data$Year == 2016,])
summary(data[data$Country == "DNK" & data$Year == 2016,])
summary(data[data$Country == "ESP" & data$Year == 2016,])
summary(data[data$Country == "POL" & data$Year == 2016,])

# Wykres 1

options(repr.plot.width = 14, repr.plot.height = 8)
ggplot(data[which(data$Year %in% c(1958, 1987, 2016) & data$Country %in% c("POL","DEUT","DNK", "ESP")),], aes(Age, log_mortality, color = Płeć)) + 
  geom_line() + 
  facet_grid(Country_name ~ Year)+
  theme(plot.title = element_text(color="black", size=18, hjust = 0.5, face="bold"),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0),
        axis.title.x=element_text(size=10,face="bold"),
        axis.title.y=element_text(size=10,face="bold"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12)
  ) + 
  scale_x_continuous("Wiek") +
  scale_y_continuous("Logarytm prawdopodobieństwa zgonu")

# Wykres 2

options(repr.plot.width = 14, repr.plot.height = 8)
ggplot(data[data$Gender=='Female',],aes(Age,log_mortality,color=Year))+
  geom_point(size=0.8)+
  facet_wrap(~Country_name,ncol=4)+
  labs(color = 'Rok') +
  ylim(-12, 0) +
  theme(plot.title = element_text(color="black", size=14, hjust = 0.5, face="bold"),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 11),
        axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.title.x=element_text(size=11,face="bold"),
        axis.title.y=element_text(size=11,face="bold"),
        strip.text.x = element_text(size = 12)
  ) +
  scale_x_continuous("Wiek") +
  scale_y_continuous("Logarytm prawdopodobieństwa zgonu")

# Wykres 3

options(repr.plot.width = 14, repr.plot.height = 8)
ggplot(data[data$Gender=='Male',],aes(Age,log_mortality,color=Year))+
  geom_point(size=0.8)+
  facet_wrap(~Country_name,ncol=4)+
  labs(color = 'Rok') +
  ylim(-12, 0) +
  theme(plot.title = element_text(color="black", size=14, hjust = 0.5, face="bold"),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 11),
        axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.title.x=element_text(size=11,face="bold"),
        axis.title.y=element_text(size=11,face="bold"),
        strip.text.x = element_text(size = 12)
  ) +
  scale_x_continuous("Wiek") +
  scale_y_continuous("Logarytm prawdopodobieństwa zgonu")

# Funkcja generująca heatmapy

myHeatmap <- function(country,gender) {
  Year.min <- 1958
  Year.max <- 2016
  hmrsel <- dta[which(dta$Country == country & dta$Gender == gender),]
  m0 <- c(min(hmrsel$log_mortality), max(hmrsel$log_mortality)) 
  log_mortality <- t(matrix(as.matrix(hmrsel[which(hmrsel$Gender==gender),"log_mortality"]), nrow=101, ncol=(Year.max-Year.min+1) ))
  image(z=log_mortality, useRaster=TRUE,  zlim=m0, col=rev(rainbow(n=60, start=0, end=.72)), xaxt='n', yaxt='n', 
        main=list(paste(if(country == 'DEUT') {"Niemcy"} else if(country == 'ESP') {'Hiszpania'}
                        else if(country == 'DNK') {'Dania'} else {'Polska'}, sep=" "), cex=1.2), 
        cex.lab=1, ylab="Wiek") 
  axis(1, at=c(0:(Year.max-Year.min))/(Year.max-Year.min), c(Year.min:Year.max))                    
  axis(2, at=c(0:49)/50, labels=c(0:49)*2)   
  contour(z=log_mortality, add = TRUE, drawlabels = TRUE)
}

options(repr.plot.width = 12, repr.plot.height = 8)
par(mfrow=c(2,2),mar=c(4, 4, 2, 1)) 

# Wykres 4

myHeatmap("DNK","Female") 
myHeatmap("ESP","Female") 
myHeatmap("DEUT","Female") 
myHeatmap("POL","Female") 

# Wykres 5

myHeatmap("DNK","Male") 
myHeatmap("ESP","Male") 
myHeatmap("DEUT","Male") 
myHeatmap("POL","Male") 

# Wykres 6

options(repr.plot.width = 14, repr.plot.height = 8)
ggplot(data[data$Gender=='Female',],aes(Age,Mortality,color=Year))+
  geom_point(size=0.8)+
  facet_wrap(~Country_name, ncol=4)+
  labs(color = 'Rok') +
  theme(plot.title = element_text(color="black", size=14, hjust = 0.5, face="bold"),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 11),
        axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.title.x=element_text(size=11,face="bold"),
        axis.title.y=element_text(size=11,face="bold"), 
        strip.text.x = element_text(size = 12)
  ) +
  scale_x_continuous("Wiek") +
  scale_y_continuous("Prawdopodobieństwo zgonu")

# Wykres 7

options(repr.plot.width = 14, repr.plot.height = 8)
ggplot(data[data$Gender=='Male',],aes(Age,Mortality,color=Year))+
  geom_point(size=0.8)+
  facet_wrap(~Country_name, ncol=4)+
  labs(color = 'Rok') +
  theme(plot.title = element_text(color="black", size=14, hjust = 0.5, face="bold"),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 11),
        axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.title.x=element_text(size=11,face="bold"),
        axis.title.y=element_text(size=11,face="bold"), 
        strip.text.x = element_text(size = 12)
  ) +
  scale_x_continuous("Wiek") +
  scale_y_continuous("Prawdopodobieństwo zgonu")


######## Przetwarzanie danych ########

## Kategoryzacja zmiennych

cat_features <- c("Country","Gender","Age")
cat_variable <- c("Country_cat","Gender_cat","Age_cat")

data[cat_variable] <- lapply(data[cat_features], factor)
data[cat_features] <- lapply(data[cat_variable], as.integer)
data[cat_features]<-data[cat_features]-1

data$Year <- as.numeric(as.character(data$Year))
data <- data %>% select(Country_cat,Gender_cat,Age_cat,Country_name,
                      Country,Year,Gender,Age, log_mortality, Mortality)

country_mapping=unique(data[,c('Country_cat','Country')])
row.names(country_mapping) <- NULL

age_mapping=unique(data[,c('Age_cat','Age')])
row.names(age_mapping) <- NULL

gender_mapping=unique(data[,c('Gender_cat','Gender')])
row.names(gender_mapping) <- NULL


####### Przygotowanie zbioru treningowego i testowego #######

training <- filter(data,Year%in%1958:2000)

# Zbiór walidacyjny

col_vector <- c("Year","Age","Country","Gender","log_mortality")
Training <- training %>% select(one_of(col_vector))
val <- stratified(Training, c('Year','Age'), 0.25)

X_validation <- val[,c("Year","Age","Country","Gender")]
X_val <- list(as.matrix(X_validation$Year),as.matrix(X_validation$Age),
              as.matrix(X_validation$Country),as.matrix(X_validation$Gender))

y_validation <- val[, "log_mortality"]
y_val <- as.matrix(y_validation)

# Zbiór treningowy

train<-setdiff(Training,val)

X_training <- train[,c("Year","Age","Country","Gender")]
X_dev <- list(as.matrix(X_training$Year),as.matrix(X_training$Age),
              as.matrix(X_training$Country),as.matrix(X_training$Gender))

y_training <- train[, "log_mortality"]
y_dev <- as.matrix(y_training)


# Zbiór testowy

test <- filter(data,Year%in%2001:2016)

X_test <- test[,c("Year","Age","Country","Gender")]
X_test_1st <- list(as.matrix(X_test$Year),as.matrix(X_test$Age),
                   as.matrix(X_test$Country),as.matrix(X_test$Gender))

y_test <- test[, "log_mortality"]
y_test_1st <- as.matrix(y_test)


######## Ustawienie architektury sieci neuronowych #########

# Podstawowa architektura

Year <- layer_input(shape=c(1),dtype="float32",name="Year")
Age <- layer_input(shape=c(1),dtype="int32",name="Age")
Country <- layer_input(shape=c(1),dtype="int32",name="Country")
Gender <- layer_input(shape=c(1),dtype="int32",name="Gender")

# Ustawienie warstw wejściowych 

Age_embed <- Age %>% 
  layer_embedding(input_dim = 101,output_dim=5,input_length=1,name="Age_embed") %>% 
  layer_flatten()
Gender_embed <- Gender %>% 
  layer_embedding(input_dim=2,output_dim=5,input_length = 1,name="Gender_embed") %>% 
  layer_flatten()
Country_embed <- Country %>% 
  layer_embedding(input_dim=4,output_dim = 5,input_length = 1,name="Country_embed") %>% 
  layer_flatten()

features <- layer_concatenate(list(Year,Age_embed,Gender_embed,Country_embed))

# Ustawienie warstw ukrytych

middle <- features %>%     
  layer_dense(units=128,activation="tanh") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(0.05) %>% 
  
  layer_dense(units=128,activation="tanh") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(0.05) %>%  
  
  layer_dense(units=128,activation="tanh") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(0.05) %>%    
  
  layer_dense(units=128,activation="tanh") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(0.05)     

# Ustawienie warstwy wyjściowej

main_output <- layer_concatenate(list(features,middle)) %>% 
  layer_dense(units=128,activation="tanh") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(0.05) %>% 
  layer_dense(units = 1, activation = 'sigmoid', name = 'main_output') 

# Ustawienie modelu zbudowanego z powyższych warstw

model_rw <- keras_model(inputs=c(Year,Age,Country,Gender),outputs=c(main_output))
summary(model_rw)

# Wyszukiwanie hiperparametrów 

printer = file("Model/nn_mortality.R","w")
writeLines("FLAGS <- flags(
  flag_integer('layers', 3),
  flag_numeric('dropout', 0.05),
  flag_integer('neurons', 128),
  flag_numeric('lr', 0.01),
  flag_integer('patience', 35),
  flag_integer('pats', 20),
  flag_integer('batchsize', 1200),
  flag_string('activation', 'relu')   
)

build_model <- function() {
  
  Year <- layer_input(shape=c(1),dtype='float32',name='Year')
  Age <- layer_input(shape=c(1),dtype='int32',name='Age')
  Country <- layer_input(shape=c(1),dtype='int32',name='Country')
  Gender <- layer_input(shape=c(1),dtype='int32',name='Gender')
    
  ##### set up the embedding layer of the neural nets
  
  Age_embed <- Age %>% 
    layer_embedding(input_dim = 101,output_dim=5,input_length=1,name='Age_embed') %>% 
    keras::layer_flatten()
  
  Gender_embed <- Gender %>% 
    layer_embedding(input_dim=2,output_dim=5,input_length = 1,name='Gender_embed') %>% 
    keras::layer_flatten()
  
  Country_embed <- Country %>% 
    layer_embedding(input_dim=4,output_dim = 5,input_length = 1,name='Country_embed') %>% 
    keras::layer_flatten()
  
  ##### merge all the feature vectors 
  
  features <- layer_concatenate(list(Year,Age_embed,Gender_embed,Country_embed)) 

   middle<-features 

for (i in (1: FLAGS$layers)){  
      middle <- middle %>% 
      layer_dense(units=FLAGS$neurons,activation=FLAGS$activation) %>% 
      layer_batch_normalization() %>% 
      layer_dropout(FLAGS$dropout)      
  }  

  ##### set up the output layer
  
  main_output <- layer_concatenate(list(features,middle)) %>% 
     layer_dense(units=FLAGS$neurons,activation=FLAGS$activation) %>%
     layer_batch_normalization() %>% 
     layer_dropout(FLAGS$dropout)%>% 
    layer_dense(units=1,name='main_output')
  
  #### set up the model combining input layers and output layer
  
  model <- keras_model(inputs=c(Year,Age,Country,Gender),outputs=c(main_output))

  model %>% compile(
    optimizer_adam(lr = FLAGS$lr),
    loss='mse',
    metrics=c('mae')
  )
  model
}
model <- build_model()

##### the folowing two callback functions are used to control the training process by monitoring the validation loss
early_stop <- callback_early_stopping(monitor = 'val_loss', patience = FLAGS$patience)
lr_reducer <- callback_reduce_lr_on_plateau(monitor = 'val_loss', factor = 0.1,
                                            patience = FLAGS$pats, verbose = 0, mode = 'min',
                                            min_delta = 1e-04, cooldown = 0, min_lr = 0)

### Fit the neural network specified as above
history <- model %>% fit(
  x  = X_dev, 
  y  = y_dev,
  batch_size = FLAGS$batchsize, 
  epochs = 100,
  validation_data =list(X_val, y_val),
  verbose = 1,
  callbacks = list(early_stop,lr_reducer)
)

# plot(history)
score <- model %>% evaluate(X_test_1st, y_test_1st, verbose = 0)
save_model_hdf5(model, 'Model/model.h5')
# cat('Test loss:', score$loss)
",con=printer,sep=" ")
close(printer)

set.seed(68033)

par <- list( 
  layers = c(3,6),
  dropout = c(0.01, 0.03, 0.05),
  neurons = c(128, 160, 192),
  batchsize = c(400, 800),
  lr = c(0.05, 0.1),
  patience = c(35, 45),
  pats = c(20, 30),
  activation = c("relu"))

runs <- tuning_run('Model/nn_mortality.R', runs_dir = 'Model/D_tuning', sample = 0.05, flags = par)

# Wyniki hiperparametrów

results <- ls_runs(order = metric_val_loss, decreasing= F, runs_dir = 'Model//D_tuning')

write_xlsx(results, "results.xlsx")

inputdir <- file.path(getwd(), "Model/D_tuning")

# Najlepszy oszacowany model

id<-results[1,1]
path<-file.path(getwd(),id,"Model/model.h5")
model <- load_model_hdf5(path)
summary(model)

######## Projekcia prawdopodobieństwa śmierci za pomocą sieci neuronowych ########

# Predykcja dla zbioru testowego

pred_raw <- filter(data,Year%in%2001:2016)
id <- c(results[1,1],results[2,1],results[3,1],results[4,1],results[5,1],results[6,1])
predicted_log_mortality <- replicate(dim(y_test_1st)[1], 0)

for (i in id){
  path<-file.path(getwd(),i, "Model/model.h5")
  model <- load_model_hdf5(path)
  predicted_log_mortality <- model %>% predict(X_test_1st)+predicted_log_mortality
}

predicted_log_mortality <- predicted_log_mortality/length(id)

NN_prediction <- cbind(pred_raw,predicted_log_mortality)
NN_prediction <- NN_prediction %>% mutate(NN_mortality=exp(predicted_log_mortality))

# Predykcja na przyszłe lata

input_2000=filter(data,Year==2000)%>% select(Country_cat,Gender_cat,Age_cat,
                                             Country,Year,Gender,Age,log_mortality)
input_2000 <- input_2000%>% 
  rename(Year2000 = Year,
        log_mortality_2000 = log_mortality)

input_2010=filter(data,Year==2010)%>% select(Country_cat,Gender_cat,Age_cat,
                                             Country,Year,Gender,Age,log_mortality)
input_2010 <- input_2010 %>% 
  rename(Year2010 = Year,
        log_mortality_2010 = log_mortality)

input_2000 <- inner_join(input_2000, input_2010, by = c('Country_cat', 'Gender_cat', 'Age_cat', 'Country', 'Gender', 'Age'))

input <- input_2000 %>% mutate(Age=Age, Year2020=Year2000+20, Year2030=Year2000+30,
                                        Year2040=Year2000+40, Year2050=Year2000+50)

input2020 <- input %>% select(Country, Year2020, Gender,Age)
input2030 <- input %>% select(Country, Year2030, Gender,Age)
input2040 <- input %>% select(Country, Year2040, Gender,Age)
input2050 <- input %>% select(Country, Year2050, Gender,Age)

X_2020 <- list(as.matrix(input2020$Year2020),as.matrix(input2020$Age),
               as.matrix(input2020$Country),as.matrix(input2020$Gender))
X_2030 <- list(as.matrix(input2030$Year2030),as.matrix(input2030$Age),
               as.matrix(input2030$Country),as.matrix(input2030$Gender))
X_2040 <- list(as.matrix(input2040$Year2040),as.matrix(input2040$Age),
               as.matrix(input2040$Country),as.matrix(input2040$Gender))
X_2050 <- list(as.matrix(input2050$Year2050),as.matrix(input2050$Age),
               as.matrix(input2050$Country),as.matrix(input2050$Gender))

path<-file.path(getwd(),results[1,1], "Model/model.h5")
model <- load_model_hdf5(path)

log_mortality_2020 <- model %>% predict(X_2020)
log_mortality_2030 <- model %>% predict(X_2030)
log_mortality_2040 <- model %>% predict(X_2040)
log_mortality_2050 <- model %>% predict(X_2050)

projections<- cbind(input,log_mortality_2020, log_mortality_2030, 
                          log_mortality_2040, log_mortality_2050)

projections<-projections %>% mutate(mortality_2020=exp(log_mortality_2020),
                                    mortality_2030=exp(log_mortality_2030),
                                    mortality_2040=exp(log_mortality_2040),
                                    mortality_2050=exp(log_mortality_2050),
                                    observed_mortality_2000=exp(log_mortality_2000),
                                    observed_mortality_2010=exp(log_mortality_2010))

# Funkcja wykorzystana do edycji legendy wykresu

extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)}

projections$Płeć <- projections$Gender_cat
projections$Płeć <- unfactor(projections$Płeć)
projections$Płeć[projections$Płeć == "Female"] <- "Kobieta"
projections$Płeć[projections$Płeć == "Male"] <- "Mężczyzna"

options(repr.plot.width = 20, repr.plot.height = 8)

DNK_log_mort <- projections %>% filter(Country_cat=="DNK")%>% ggplot()+
  geom_line(aes(x=Age, y=log_mortality_2000,color="2000",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2010,color="2010",group=Płeć))+
  geom_line(aes(x=Age, y=log_mortality_2020,color="2020",group=Płeć))+
  geom_line(aes(x=Age, y=log_mortality_2030,color="2030",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2040,color="2040",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2050,color="2050",group=Płeć))+ 
  facet_grid(. ~ Płeć)+
  xlab("") + ylab("Dania")+
  ylim(-15.0, 0)+
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.title.x=element_text(size=12,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 14)
  )

DNK_mort <- projections %>% filter(Country_cat=="DNK")%>% ggplot()+
  geom_line(aes(x=Age, y=observed_mortality_2000,color="2000",group=Płeć))+ 
  geom_line(aes(x=Age, y=observed_mortality_2010,color="2010",group=Płeć))+
  geom_line(aes(x=Age, y=mortality_2020,color="2020",group=Płeć))+
  geom_line(aes(x=Age, y=mortality_2030,color="2030",group=Płeć))+ 
  geom_line(aes(x=Age, y=mortality_2040,color="2040",group=Płeć))+ 
  geom_line(aes(x=Age, y=mortality_2050,color="2050",group=Płeć))+ 
  facet_grid(. ~ Płeć)+
  xlab("") + ylab("Dania")+
  ylim(0, 0.5)+
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.title.x=element_text(size=12,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 14)
  )  

ESP_log_mort <- projections %>% filter(Country_cat=="ESP")%>% ggplot()+
  geom_line(aes(x=Age, y=log_mortality_2000,color="2000",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2010,color="2010",group=Płeć))+
  geom_line(aes(x=Age, y=log_mortality_2020,color="2020",group=Płeć))+
  geom_line(aes(x=Age, y=log_mortality_2030,color="2030",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2040,color="2040",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2050,color="2050",group=Płeć))+ 
  facet_grid(. ~ Płeć)+
  xlab("") + ylab("Hiszpania")+
  ylim(-15.0, 0)+
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.title.x=element_text(size=12,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 14)
  )

ESP_mort <- projections %>% filter(Country_cat=="ESP")%>% ggplot()+
  geom_line(aes(x=Age, y=observed_mortality_2000,color="2000",group=Płeć))+ 
  geom_line(aes(x=Age, y=observed_mortality_2010,color="2010",group=Płeć))+
  geom_line(aes(x=Age, y=mortality_2020,color="2020",group=Płeć))+
  geom_line(aes(x=Age, y=mortality_2030,color="2030",group=Płeć))+ 
  geom_line(aes(x=Age, y=mortality_2040,color="2040",group=Płeć))+ 
  geom_line(aes(x=Age, y=mortality_2050,color="2050",group=Płeć))+ 
  facet_grid(. ~ Płeć)+
  xlab("") + ylab("Hiszpania")+
  ylim(0, 0.5)+
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.title.x=element_text(size=12,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 14)
  )  

DEUT_log_mort <- projections %>% filter(Country_cat=="DEUT")%>% ggplot()+
  geom_line(aes(x=Age, y=log_mortality_2000,color="2000",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2010,color="2010",group=Płeć))+
  geom_line(aes(x=Age, y=log_mortality_2020,color="2020",group=Płeć))+
  geom_line(aes(x=Age, y=log_mortality_2030,color="2030",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2040,color="2040",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2050,color="2050",group=Płeć))+ 
  facet_grid(. ~ Płeć)+
  xlab("") + ylab("Niemcy")+
  ylim(-15.0, 0)+
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.title.x=element_text(size=12,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size=14)
  )

DEUT_mort <- projections %>% filter(Country_cat=="DEUT")%>% ggplot()+
  geom_line(aes(x=Age, y=observed_mortality_2000,color="2000",group=Płeć))+ 
  geom_line(aes(x=Age, y=observed_mortality_2010,color="2010",group=Płeć))+
  geom_line(aes(x=Age, y=mortality_2020,color="2020",group=Płeć))+
  geom_line(aes(x=Age, y=mortality_2030,color="2030",group=Płeć))+ 
  geom_line(aes(x=Age, y=mortality_2040,color="2040",group=Płeć))+ 
  geom_line(aes(x=Age, y=mortality_2050,color="2050",group=Płeć))+ 
  facet_grid(. ~ Płeć)+
  xlab("") + ylab("Niemcy")+
  ylim(0, 0.5)+
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.title.x=element_text(size=12,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 14)
  )

POL_log_mort <- projections %>% filter(Country_cat=="POL")%>% ggplot()+
  geom_line(aes(x=Age, y=log_mortality_2000,color="2000",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2010,color="2010",group=Płeć))+
  geom_line(aes(x=Age, y=log_mortality_2020,color="2020",group=Płeć))+
  geom_line(aes(x=Age, y=log_mortality_2030,color="2030",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2040,color="2040",group=Płeć))+ 
  geom_line(aes(x=Age, y=log_mortality_2050,color="2050",group=Płeć))+ 
  facet_grid(. ~ Płeć)+
  xlab("Wiek") + ylab("Polska")+
  ylim(-15.0, 0)+
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.title.x=element_text(size=14,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 14)
  )

POL_mort <- projections %>% filter(Country_cat=="POL")%>% ggplot()+
  geom_line(aes(x=Age, y=observed_mortality_2000,color="2000",group=Płeć))+ 
  geom_line(aes(x=Age, y=observed_mortality_2010,color="2010",group=Płeć))+
  geom_line(aes(x=Age, y=mortality_2020,color="2020",group=Płeć))+
  geom_line(aes(x=Age, y=mortality_2030,color="2030",group=Płeć))+ 
  geom_line(aes(x=Age, y=mortality_2040,color="2040",group=Płeć))+ 
  geom_line(aes(x=Age, y=mortality_2050,color="2050",group=Płeć))+ 
  facet_grid(. ~ Płeć)+
  xlab("Wiek") + ylab("Polska")+
  ylim(0, 0.5)+
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=12, angle=0),
        axis.title.x=element_text(size=14,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 14)
  )  

plot_legend <- projections %>% filter(Country_cat=="DEUT")%>% ggplot()+
  geom_line(aes(x=Age, y=observed_mortality_2000,color="2000",group=Płeć))+ 
  geom_line(aes(x=Age, y=observed_mortality_2010,color="2010",group=Płeć))+
  geom_line(aes(x=Age, y=mortality_2020,color="2020",group=Płeć))+
  geom_line(aes(x=Age, y=mortality_2030,color="2030",group=Płeć))+ 
  geom_line(aes(x=Age, y=mortality_2040,color="2040",group=Płeć))+ 
  geom_line(aes(x=Age, y=mortality_2050,color="2050",group=Płeć))+ 
  labs(color = 'Rok') +
  theme(legend.position = 'bottom',   
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 16),) +
  guides(col = guide_legend(nrow = 1))


shared_legend <- extract_legend(plot_legend)

# Wykres 8

grid.arrange(arrangeGrob(DNK_log_mort, ESP_log_mort, DEUT_log_mort, POL_log_mort, nrow = 4),
             shared_legend, nrow = 2, heights = c(18,1))

# Wykres 9

grid.arrange(arrangeGrob(DNK_mort, ESP_mort, DEUT_mort, POL_mort, nrow = 4),
             shared_legend, nrow = 2, heights = c(18,1))


# Wygenerowanie grup wiekowych dla państw

proj_age <- projections[(projections$Age_cat == 0 | projections$Age_cat == 25 | 
                           projections$Age_cat == 50 | projections$Age_cat == 75 | 
                           projections$Age_cat == 100),]

proj_age <- proj_age[, c('Country_cat','Age_cat', 'Gender_cat',
                           'log_mortality_2000', 'log_mortality_2010', 
                           'log_mortality_2020', 'log_mortality_2030', 
                           'log_mortality_2040', 'log_mortality_2050',
                           'observed_mortality_2000', 'observed_mortality_2010',
                           'mortality_2020', 'mortality_2030',
                           'mortality_2040', 'mortality_2050')]

write_xlsx(proj_age, "proj_age.xlsx")

proj_DEUT <- proj_age[proj_age$Country_cat == 'DEUT',]
proj_DNK <- proj_age[proj_age$Country_cat == 'DNK',]
proj_ESP <- proj_age[proj_age$Country_cat == 'ESP',]
proj_POL <- proj_age[proj_age$Country_cat == 'POL',]

write_xlsx(proj_DEUT, "proj_DEUT.xlsx")
write_xlsx(proj_DNK, "proj_DNK.xlsx")
write_xlsx(proj_ESP, "proj_ESP.xlsx")
write_xlsx(proj_POL, "proj_POL.xlsx")

######## Predykcja na zbiorze testowym stosując model Lee-Cartera ########

datalc <- fread(file="mortality.csv")

datalc$Gender <- as.factor(datalc$Gender)

country <- c("DEUT", "DNK", "ESP", "POL")
gender <- c("Male", "Female")
Year.min <- 1958
ObsYear <- 2000

lc_list <- list()

# Model dokonujący predykcji metodą LC

for(c in country){
  lc_within <- list()
  for(g in gender){
    train <- subset(datalc, Year >= Year.min & Year<=ObsYear &  Gender == g & Country == c)
    train[,ax:= mean(log_mortality), by = (Age)]
    train[,mx_adj:= log_mortality-ax]  
    rates_mat <- as.matrix(train %>% dcast.data.table(Age~Year, value.var = "mx_adj", sum))[,-1]
    svd_fit <- svd(rates_mat)
    ax <- train[,unique(ax)]
    bx <- svd_fit$u[,1]*svd_fit$d[1]
    kt <- svd_fit$v[,1]
    c1 <- mean(kt)
    c2 <- sum(bx)
    ax <- ax+c1*bx
    bx <- bx/c2
    kt <- (kt-c1)*c2
    
    vali <- subset(datalc, Year>ObsYear &  Gender == g & Country == c)
    t_forecast <- vali[,unique(Year)] %>% length()
    forecast_kt  =kt %>% rwf(t_forecast, drift = T)
    kt_forecast = forecast_kt$mean
    
    fitted = (ax+(bx)%*%t(kt)) %>% melt
    train$pred_LC_svd = fitted$value %>% exp
    fitted_vali = (ax+(bx)%*%t(kt_forecast)) %>% melt
    vali$pred_LC_svd =   fitted_vali$value %>% exp
    lc_within[[g]] <- vali
  }
  lc_list[[c]] <- rbindlist(lc_within)
}

lc_prediction <- rbindlist(lc_list)


######### Porównanie predykcji Lee-Cartera i sieci neuronowych ########

NN_prediction$Country2 <- NN_prediction$Country_cat
lc_prediction$Country2 <- lc_prediction$Country
NN_prediction$Gender2 <- NN_prediction$Gender_cat
lc_prediction$Gender2 <- lc_prediction$Gender

predictions <- inner_join(NN_prediction,lc_prediction,by=c("Country2","Year","Gender2","Age", "log_mortality"))

predictions<- predictions %>% rename(observed_mortality= Mortality,
                                    lc_mortality=pred_LC_svd)

col_vector <-c('Country2', 'Year', 'Gender2', 'Age',
               'observed_mortality', 'NN_mortality', 'lc_mortality')

predictions<-select(predictions, one_of(col_vector))

colnames(predictions)[colnames(predictions) == "Country2"] <- "Country"
colnames(predictions)[colnames(predictions) == "Gender2"] <- "Gender"

forecast<- predictions %>% gather('observed_mortality',
                                  'NN_mortality',
                                  'lc_mortality',key="Models",value="mortality") 

forecast$Model <- forecast$Models
forecast$Model[forecast$Model == "observed_mortality"] <- "Dane historyczne"
forecast$Model[forecast$Model == "NN_mortality"] <- "NN"
forecast$Model[forecast$Model == "lc_mortality"] <- "LC"
forecast$Model <- as.factor(forecast$Model)

my_colors <- c("red","black","green")   
names(my_colors) <- levels(factor(levels(forecast$Model))) 
forecast <- inner_join(forecast, Countries, by = 'Country')
my_scale <- scale_color_manual(name = "Model", values = my_colors) 

options(repr.plot.width = 17, repr.plot.height = 9)

# Logarytm prawdopodobieństwa zgonu

fst_male <- forecast %>% filter(Year==2005) %>% filter(Gender=='Male') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, log_mortality,color=Model)) +
  facet_wrap(~Country_name,ncol=4)+
  ylim(-10.5, 0) +
  xlab("Wiek") + ylab("Mężczyzna")+ my_scale +
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.title.x=element_text(size=14,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 12)
  )

fst_female <- forecast %>% filter(Year==2005) %>% filter(Gender=='Female') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, log_mortality,color=Model)) +
  facet_wrap(~Country_name,ncol=4)+
  ylim(-10.5, 0) +
  xlab("") + ylab("Kobieta")+ my_scale +
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.title.x=element_text(size=14,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 12)
  )

fst_legend <- forecast %>% filter(Year==2005) %>% filter(Gender=='Female') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, log_mortality,color=Model)) +
  facet_wrap(~Country,ncol=2)+
  xlab("Age") + ylab("Mortality")+ my_scale +
  theme(legend.position = 'bottom',
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 13)
  )


# Wykres 10

fst_legend <- extract_legend(fst_legend)
grid.arrange(arrangeGrob(fst_female, fst_male, nrow = 2),
             fst_legend, nrow = 2, heights = c(15,1))

fst_male <- forecast %>% filter(Year==2015) %>% filter(Gender=='Male') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, log_mortality,color=Model)) +
  facet_wrap(~Country_name,ncol=4)+
  ylim(-10.5, 0) +
  xlab("Wiek") + ylab("Mężczyzna")+ my_scale +
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.title.x=element_text(size=14,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 12)
  )

fst_female <- forecast %>% filter(Year==2015) %>% filter(Gender=='Female') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, log_mortality,color=Model)) +
  facet_wrap(~Country_name,ncol=4)+
  ylim(-10.5, 0) +
  xlab("") + ylab("Kobieta")+ my_scale +
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.title.x=element_text(size=14,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 12)
  )

fst_legend <- forecast %>% filter(Year==2015) %>% filter(Gender=='Female') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, log_mortality,color=Model)) +
  facet_wrap(~Country,ncol=2)+
  xlab("Age") + ylab("Mortality")+ my_scale +
  theme(legend.position = 'bottom',
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 13)
  )


# Wykres 11

fst_legend <- extract_legend(fst_legend)
grid.arrange(arrangeGrob(fst_female, fst_male, nrow = 2),
             fst_legend, nrow = 2, heights = c(15,1))


# Prawdopodobieństwo zgonu

fst_male <- forecast %>% filter(Year==2005) %>% filter(Gender=='Male') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, mortality,color=Model)) +
  facet_wrap(~Country_name,ncol=4)+
  ylim(0, 0.7) +
  xlab("Wiek") + ylab("Mężczyzna")+ my_scale +
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.title.x=element_text(size=14,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 12)
  )

fst_female <- forecast %>% filter(Year==2005) %>% filter(Gender=='Female') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, mortality,color=Model)) +
  facet_wrap(~Country_name,ncol=4)+
  ylim(0, 0.7) +
  xlab("") + ylab("Kobieta")+ my_scale +
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.title.x=element_text(size=14,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 12)
  )

fst_legend <- forecast %>% filter(Year==2005) %>% filter(Gender=='Female') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, mortality,color=Model)) +
  facet_wrap(~Country_name,ncol=2)+
  xlab("Age") + ylab("Mortality")+ my_scale +
  theme(legend.position = 'bottom',
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 13)
  )

# Wykres 12

fst_legend <- extract_legend(fst_legend)
grid.arrange(arrangeGrob(fst_female, fst_male, nrow = 2),
             fst_legend, nrow = 2, heights = c(15,1))


fst_male <- forecast %>% filter(Year==2015) %>% filter(Gender=='Male') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, mortality,color=Model)) +
  facet_wrap(~Country_name,ncol=4)+
  ylim(0, 0.6) +
  xlab("Wiek") + ylab("Mężczyzna")+ my_scale +
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.title.x=element_text(size=14,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 12)
  )

fst_female <- forecast %>% filter(Year==2015) %>% filter(Gender=='Female') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, mortality,color=Model)) +
  facet_wrap(~Country_name,ncol=4)+
  ylim(0, 0.6) +
  xlab("") + ylab("Kobieta")+ my_scale +
  theme(legend.position = 'none',
        axis.text.x = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=10, angle=0),
        axis.title.x=element_text(size=14,face="plain"),
        axis.title.y=element_text(size=14,face="plain"),
        strip.text.x = element_text(size = 12)
  )

fst_legend <- forecast %>% filter(Year==2015) %>% filter(Gender=='Female') %>% mutate(
  log_mortality = log(mortality)
)  %>% ggplot()+geom_line(aes(Age, mortality,color=Model)) +
  facet_wrap(~Country_name,ncol=2)+
  xlab("Age") + ylab("Mortality")+ my_scale +
  theme(legend.position = 'bottom',
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 13)
  )

# Wykres 13

fst_legend <- extract_legend(fst_legend)
grid.arrange(arrangeGrob(fst_female, fst_male, nrow = 2),
             fst_legend, nrow = 2, heights = c(15,1))

fst_age <- forecast[(forecast$Age == 0 | forecast$Age == 25 | 
                    forecast$Age == 50 | forecast$Age == 75 | forecast$Age == 100) & 
                    (forecast$Year == 2005 | forecast$Year == 2015),]

fst_DEUT <- fst_age[1:6][fst_age$Country == 'DEUT',]
fst_DNK <- fst_age[1:6][fst_age$Country == 'DNK',]
fst_ESP <- fst_age[1:6][fst_age$Country == 'ESP',]
fst_POL <- fst_age[1:6][fst_age$Country == 'POL',]

write_xlsx(fst_DEUT, "fst_DEUT.xlsx")
write_xlsx(fst_DNK, "fst_DNK.xlsx")
write_xlsx(fst_ESP, "fst_ESP.xlsx")
write_xlsx(fst_POL, "fst_POL.xlsx")


######### Błąd średniokwadratowy - porównanie #########

prediction_error <- predictions %>% mutate(
  NN_error = (NN_mortality-observed_mortality)^2,
  lc_error = (lc_mortality-observed_mortality)^2,
)

mse_NN<-mean(prediction_error$NN_error)
mse_lc<-mean(prediction_error$lc_error)

# Per państwo

mse <- prediction_error %>% 
       group_by(Country) %>%  summarize(mse_NN=mean(NN_error),
                              mse_lc=mean(lc_error,na.rm=TRUE))

mse %>% ungroup()%>%filter(mse_NN<mse_lc)
mse %>% ungroup()%>%filter(mse_NN>mse_lc)

# Per płeć

mse <- prediction_error %>% 
        group_by(Gender) %>%  summarize(mse_NN=mean(NN_error),
                              mse_lc=mean(lc_error,na.rm=TRUE))

mse %>% ungroup()%>%filter(mse_NN<mse_lc)
mse %>% ungroup()%>%filter(mse_NN>mse_lc)

# Per państwo i płeć

mse <- prediction_error %>% 
        group_by(Country,Gender) %>%  summarize(mse_NN=mean(NN_error),
                                      mse_lc=mean(lc_error,na.rm=TRUE))

mse %>% ungroup()%>%filter(mse_NN<mse_lc)
mse %>% ungroup()%>%filter(mse_NN>mse_lc)
