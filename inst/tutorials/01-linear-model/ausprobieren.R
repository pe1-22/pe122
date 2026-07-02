library (ggplot2)
##### 01 lineares Modell
set.seed(12)

daten <- data.frame(
  Geschlecht = sample(c("m", "w"), 100, replace = TRUE),
  Groesse_cm = round(rnorm(100, mean = 179, sd = 18), 1),
  Gewicht_kg = round(rnorm(100, mean = 81, sd = 15), 1),
  trainingsumfang = round(pmax(0, rnorm(100, mean = 25, sd = 15))),
  VO2max = round(
    pmin(
      pmax(
        30 + round(pmax(0, rnorm(100, mean = 25, sd = 15))) * 0.6 + rnorm(100, 0, 5),
        25
      ),
      75
    ),
    1
  )
)

head (data)
plot (data$trainingsumfang, data$VO2max)
plot(trainingsumfang~VO2max, data= daten)

mean(data$trainingsumfang)
median(data$trainingsumfang)
sd(data$trainingsumfang)
summary(data$VO2max)

# 9

plot (data$trainingsumfang, data$VO2max)
ggplot(data = data, aes (y= vo2max, x = trainingsumfang))

# 10 
cor(data$trainingsumfang, data$VO2max)**2
r <- 0.866
R <- r**2
R <-  r^2

r = 0.866

mod<-lm(VO2max~trainingsumfang,data=daten)
broom::glance(mod)

mod<-lm(VO2max~trainingsumfang,data=data)
mod

coef(mod)[1]


#12.2
beta0 <- coef(mod)[1]
beta1 <- coef(mod)[2]

#13
p1 <- beta0+beta1*40
p1 <- coef(mod)[1]+coef(mod)[2]*40
p2 <- data.frame(trainingsumfang = 40)
predict (mod,p2)

predict(mod, p2, interval = "prediction")

performance::check_model(mod)



#### 02 erweiterte Lineare Regression 

# alter datensatz: 

set.seed(42)
n <- 68

gruppe <- factor(sample(c("Leistung", "Breitensport"), n, replace = TRUE, prob = c(0.4, 0.6)))
training <- ifelse(gruppe == "Leistung", round(runif(n, 12, 22)), round(runif(n, 3, 11)))
gewicht <- round(rnorm(n, mean = 78, sd = 7))

zeit <- 13.5 +
  (-0.12 * training) +
  (0.05 * gewicht) +
  (-0.8 * (gruppe == "Leistung")) +
  rnorm(n, mean = 0, sd = 0.3)

zeit <- round(zeit, 2)

daten <- data.frame(
  id       = 1:n,
  zeit     = zeit,
  training = training,
  gewicht  = gewicht,
  gruppe   = gruppe
)

# neuer Datensatz
set.seed(43)
n <- 100

daten <- data.frame(
  id = 1:n,
  gruppe = factor(sample(c("Leistung", "Breitensport"), n, replace = TRUE, prob = c(0.4, 0.6))),
  gewicht = round(rnorm(n, mean = 78, sd = 7))
)

daten$training <- ifelse(daten$gruppe == "Leistung",
                         round(runif(n, 12, 22)),
                         round(runif(n, 3, 11)))

daten$zeit <- 13.5 +
  (-0.12 * daten$training) +
  (0.05 * daten$gewicht) +
  (-0.8 * (daten$gruppe == "Leistung")) +
  rnorm(n, mean = 0, sd = 0.3)

head(daten)
daten


# Datensatz 1
library (ggplot2)
daten <- data.frame(
  zeit = c(68, 52, 71, 49, 65, 55, 70, 48, 66, 53),
  gruppe = factor(c("Breitensport","Leistungskader","Breitensport",
                    "Leistungskader","Breitensport","Leistungskader",
                    "Breitensport","Leistungskader","Breitensport",
                    "Leistungskader")),
  training = c(20, 45, 18, 50, 25, 42, 19, 52, 22, 44)
)

# Referenzkategorie: Breitensport
daten$gruppe <- relevel(daten$gruppe, ref = "Breitensport")

# Modell fiten 
mod <- lm(zeit ~ gruppe + training, data = daten)
summary(mod)
# Koeffizienten anezigen lassen
coef(mod)

#2 weitere Modelle schaffen 
mod_gruppe <- lm(zeit ~ gruppe, data=daten )
mod_training <- lm(zeit ~ training, data=daten )

mod_gruppe
mod_training
broom::glance(mod_gruppe)
broom::glance(mod_training)
# Dummy-Matrix anzeigen
model.matrix(mod)

performance::check_model(mod)

###########    Multiple Regression  ##########
# Datensatz 2
set.seed(42)
n <- 68

gruppe <- factor(sample(c("Leistung", "Breitensport"), n, replace = TRUE, prob = c(0.4, 0.6)))
training <- ifelse(gruppe == "Leistung", round(runif(n, 12, 22)), round(runif(n, 3, 11)))
gewicht <- round(rnorm(n, mean = 78, sd = 7))

# Sprintzeit abhängig von Prädiktoren + zufälliges Rauschen
zeit <- 13.5 +
  (-0.12 * training) +
  (0.05 * gewicht) +
  (-0.8 * (gruppe == "Leistung")) +
  rnorm(n, mean = 0, sd = 0.3)

zeit <- round(zeit, 2)

daten <- data.frame(
  id       = 1:n,
  zeit     = zeit,
  training = training,
  gewicht  = gewicht,
  gruppe   = gruppe
)


# Überblick
summary(data)

mod <- lm(zeit ~ training+gewicht+gruppe, data= daten)
mod
summary(mod)
coef(mod)

summary(mod)

mod_gruppe <- lm(zeit ~ gruppe, data= daten)
mod_training <- lm(zeit ~ training, data= daten)
mod_gewicht <- lm(zeit ~ gewicht, data= daten)
broom::glance(mod_gruppe)
broom::glance(mod_training)
broom::glance(mod_gewicht)



p1 <- data.frame(training = 12 , gewicht = 73 , gruppe= "Leistung")
predict(mod, p1, interval = "prediction")

plot(data$zeit, data$gewicht)
abline(mod)


 ###########   03  Logistische Regression  ##########

set.seed(99)
n <- 90

alter         <- round(runif(n, 18, 38))
trainingsload <- round(rnorm(n, mean = 35, sd = 10))   # km/Woche
schlaf        <- round(runif(n, 4, 9), 1)
position      <- sample(c("Stürmer", "Verteidiger", "Mittelfeld"), n,
                        replace = TRUE)

logit_p <- -5 + 0.12 * alter + 0.11 * trainingsload -
           0.60 * schlaf + 1.50 * (position == "Stürmer")
prob     <- plogis(logit_p)
verletzt <- rbinom(n, 1, prob)

daten <- data.frame(
  alter         = alter,
  trainingsload = trainingsload,   # km/Woche
  schlaf        = schlaf,
  position      = position,
  verletzt      = verletzt
)
##### DAten neu 

###########   03  Logistische Regression  ##########

set.seed(99)
n <- 90


daten <- data.frame(
  alter         = round(runif(n, 18, 38)),
  trainingsload = round(rnorm(n, mean = 35, sd = 10)),   # km/Woche
  schlaf        = round(runif(n, 4, 9), 1),
  position      = factor(sample(c("Stürmer", "Verteidiger", "Mittelfeld"), n, replace = TRUE))
)


daten$verletzt <- rbinom(n, 1, 
  plogis(-5 + 0.12 * daten$alter + 
         0.11 * daten$trainingsload - 
         0.60 * daten$schlaf + 
         1.50 * (daten$position == "Stürmer"))
)
daten


# Plots 
ggplot(daten, aes(x = alter, y = trainingsload, color = factor(verletzt))) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("steelblue", "tomato"),
                     labels = c("nicht verletzt", "verletzt"),
                     name = "") +
  labs(title = "Alter und Trainingsbelastung nach Verletzungsstatus",
       x = "Alter (Jahre)",
       y = "Trainingsbelastung")


mod <- glm(verletzt ~ alter + trainingsload + schlaf + position,
           data = daten, family = binomial)
broom::tidy(mod)
broom::glance(mod)
summary(mod)

p1 <- data.frame (alter = 28, trainingsload = 42, schlaf=5.5, position="Stürmer")
insight::get_predicted(mod, data = p1, predict = "link", ci = 0.95) |> as.data.frame()
insight::get_predicted(mod, data = p1, predict = "expectation", ci = 0.95) |> as.data.frame()


new_data <- data.frame(trainingsload = c(20,30,40,50, 60, 70))

new_data <- data.frame(
  alter          = rep(28, 6),           
  trainingsload  = c(20,30,40,50,60,70),
  schlaf         = rep(5.5, 6),
  position       = rep("Stürmer", 6)
)

p <- predict(mod, newdata = new_data, type = "response", se.fit = TRUE)
p

pred <- insight::get_predicted(
  mod,
  data = new_data,
  predict = "expectation",
  ci = 0.95
) |> as.data.frame()

mod$pred <- insight::get_predicted(mod, predict = "classification")
xtabs(~ mod$pred + mod$y)

install.packages("caret")
caret::confusionMatrix(
  factor(mod$pred, levels = c(0,1)),
  factor(mod$y, levels = c(0,1))
)

p1 <- data.frame (alter = 28, trainingsload = 42, schlaf=5.5, position="Stürmer")
insight::get_predicted(mod, data = p1, predict = "link")
insight::get_predicted(mod, data = p1, predict = "expectation")

pred_l <- insight::get_predicted(mod, data = p1, predict = "link", ci = 0.95)|> as.data.frame()
exp(pred_l$predicted)

caret::confusionMatrix(
  factor(mod$pred, levels = c(0,1)),
  factor(mod$y, levels = c(0,1))
)

performance::check_model(mod)
exp(coef(mod))


new_players <- data.frame(
  alter = c(25, 30, 22),
  trainingsload = c(35, 50, 28),
  schlaf = c(6, 5, 7),
  position = factor(c("Verteidiger", "Stürmer", "Mittelfeld"), 
                    levels = c("Stürmer", "Verteidiger", "Mittelfeld"))
)
insight::get_predicted(mod, data = new_players, predict = "expectation")




# Odds 
broom::tidy(mod, exponentiate = TRUE, conf.int = TRUE)
broom::tidy(mod, conf.int = TRUE)



mod$pred <- insight::get_predicted(mod, predict = "classification")
xtabs(~ mod$pred + mod$y)

performance::check_model(mod, base_size = 7)


###########   04  Machine Learning   ##########

#### ohne Variablen speichern 
set.seed(42)

daten <- data.frame(
  pe = round(pmax(0, rnorm(80, mean = 18, sd = 6))),
  schmerz   = round(pmin(pmax(rnorm(80, mean = 6,  sd = 2),  0), 10), 1),
  motivation       = round(pmin(pmax(rnorm(80, mean = 7,  sd = 1.5), 1), 10), 1),
  alter            = round(rnorm(80, mean = 45, sd = 12)),        # kein echter Effekt
  bmi              = round(pmin(pmax(rnorm(80, mean = 26, sd = 4), 18), 42), 1), # kein echter Effekt
  op_dauer    = round(rnorm(80, mean = 85, sd = 20))         # kein echter Effekt
)

daten$kniebeugetiefe <- round(pmin(pmax(
  60 +
    daten$pe * 2.5 +
    daten$motivation       * 3.0 +
    daten$schmerz  * (-4.0) +
    rnorm(80, 0, 8),
  30), 140), 1)
  
### Index erstellen (Train/Test)
set.seed(123)
index <- sample(1:80, size = 80)

train <- daten[index[1:64], ]
test  <- daten[index[65:80], ]

nrow(train)  
nrow(test)   

## Modelle erstellen 
set.seed(123)
# Modell 1: ein zentraler Prädiktor
mod_1 <- lm(kniebeugetiefe ~ pe, data = train)

# Modell 2: klinisch relevante Prädiktoren
mod_2 <- lm(kniebeugetiefe ~ pe + schmerz + motivation, data = train)

# Modell 3: volles Modell 
mod_3 <- lm(kniebeugetiefe ~ pe + schmerz + motivation + alter + bmi,
                 data = train)

install.packages("cv")
library(cv)
cv1 <- cv(mod_1, data = train, k = 5)
cv2 <- cv(mod_2, data = train, k = 5)
cv3 <- cv(mod_3, data = train, k = 5)

as.data.frame(cv1)
as.data.frame(cv2)
as.data.frame(cv3)

best_mod <- mod_2

###Bestes Modell und  Evaluierung 
vorhersagen <- predict(best_mod, newdata = test)
rmse_test   <- sqrt(mean((test$kniebeugetiefe - vorhersagen)^2))

rmse_train <- sqrt(mean((best_mod$fitted.values - train$kniebeugetiefe)^2))
rmse_train
rmse_test

