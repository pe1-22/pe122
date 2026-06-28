######## NR 1 Lösung Trainingstagebücher ##########

# Aufgabe 1
install.packages("mall")
library(mall)
library(dplyr)

llm_use("ollama", "gpt-oss:120b-cloud", seed = 42)

trainingsberichte <- read.csv("trainingsberichte.csv")

# Aufagbe 2 Belastungszustand
trainingsberichte <- trainingsberichte |>
  llm_classify(
    col = bericht,
    labels = c("overloaded", "normally loaded", "well recovered"),
    pred_name = "belastung_ki"
  )

trainingsberichte |> select(id, athlet, belastung_ki, bericht)


# 3 Motivationszustand
trainingsberichte <- trainingsberichte |>
  llm_sentiment(
    col = bericht,
    pred_name = "motivation"
  )

trainingsberichte |> select(id, athlet, motivation, bericht)


# 4 Verletzungsrisiko
trainingsberichte <- trainingsberichte |>
  llm_classify(
    col = bericht,
    labels = c(
      "injury present",
      "elevated injury risk",
      "no risk apparent"
    ),
    pred_name = "verletzung_ki"
  )

trainingsberichte |> select(id, athlet, verletzung_ki, bericht)


# ()
trainingsberichte |> summarise(
  na_belastung  = sum(is.na(belastung_ki)),
  na_sentiment  = sum(is.na(motivation)),
  na_verletzung = sum(is.na(verletzung_ki))
)

# 5 Reproduzierbarkeit
write.csv(trainingsberichte,
          "trainingsberichte_ki_kodiert.csv",
          row.names = FALSE)

llm_use()       # aktives Modell anzeigen
sessionInfo()   # R-Version + Paketversionen





############## NR 2 Lösung: Literatur clustern ##############
# Aufgabe 1
library(dplyr)
library(mall)

llm_use("ollama", "gpt-oss:120b-cloud", seed = 42)

literatur <- read.csv("literatur.csv")

# Aufgabe 2 Thema
literatur <- literatur |>
  llm_classify(
    col = abstract,
    labels = c(
      "sleep and recovery",
      "nutrition and supplementation",
      "psychological strategies",
      "physical interventions",
      "monitoring and diagnostics"
    ),
    pred_name = "Thema"
  )

# Aufgabe 3 Evidenz
literatur <- literatur |>
  llm_classify(
    col = abstract,
    labels = c("hoch", "moderat", "niedrig"),
    pred_name = "Evidenz"
  )

# Aufgabe 4
llm_use() # Welches Modell?
sessionInfo()# R-Version und Pakete 

literatur |> summarise(
  na_thema   = sum(is.na(Thema)),
  na_evidenz = sum(is.na(Evidenz))
) # NA-Check: Alles klassifiziert?

write.csv(literatur, "literatur_ki_klassifiziert.csv", row.names = FALSE)# ergebnis speichern