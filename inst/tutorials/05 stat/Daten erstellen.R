library(dplyr)

set.seed(42)

# ── Bausteine ────────────────────────────────────────────
athleten <- paste0("A", 1:20)

berichte_overloaded <- c(
  "Run 15 km, legs extremely heavy, no power at all, barely slept this week.",
  "Only managed 20 min easy jog, total exhaustion, resting heart rate elevated for 4 days.",
  "Skipped training, completely drained, muscles aching all over.",
  "Tried interval session but had to stop after 3 reps, body not responding.",
  "Long ride 120 km, felt destroyed afterwards, no appetite, very fatigued.",
  "Training load has been too high for two weeks, starting to feel burned out.",
  "Swam 4 km but technique broke down completely in the last 1000m, very tired.",
  "Resting heart rate 12 bpm above baseline, decided to take a rest day.",
  "Fell asleep during stretching after the run, clearly overtrained.",
  "Performance dropping despite same effort, classic sign of overreaching.",
  "Woke up with heavy legs and sore throat, trained anyway, probably a mistake.",
  "Heart rate unusually high at easy pace, stopped session after 30 min.",
  "Third hard day in a row, mood very low, motivation close to zero.",
  "Could not finish planned workout, had to lie down after 40 min.",
  "Resting HR elevated, HRV very low this morning, taking a recovery day."
)

berichte_normal <- c(
  "Interval training 8x400m, all times on target, feeling okay.",
  "Strength training core and legs, mild muscle soreness next day, all normal.",
  "Group training, had fun, but pace was slightly above target zone.",
  "Cycling 90 min in the rain, knee causing minor issues uphill, will monitor.",
  "Running technique session with coach, good feedback, motivated for next week.",
  "Easy long run 50 min, heart rate in base endurance zone, steady effort.",
  "Swimming 3 km, shoulder a bit stiff at the start, loosened up after 1 km.",
  "Threshold run 20 min, hit target pace, felt controlled throughout.",
  "Gym session, all lifts completed, nothing special to report.",
  "Trail run 10 km, moderate effort, enjoyed the scenery.",
  "Brick workout bike and run, transitions smooth, no issues.",
  "Fartlek session, mixed intensities, felt like a solid training day.",
  "Tempo run 8 km, slightly below target pace but close enough.",
  "Yoga and mobility session, good for recovery between hard days.",
  "Open water swimming 2 km, a bit cold, navigation needs work."
)

berichte_recovered <- c(
  "Perfect training today, all metrics on point, personal best in 5km test, very happy.",
  "Interval session went extremely well, times well below target, feeling explosive.",
  "Easy long run 45 min, heart rate in base endurance zone, recovery going well.",
  "Race 10 km, new personal best, everything clicked today.",
  "Felt fresh and strong all session, best I have felt in weeks.",
  "HRV at highest value this month, decided to push today and it paid off.",
  "Swim session, technique felt smooth and effortless, best feel in the water this year.",
  "Long ride 100 km, legs never faded, nutrition and pacing spot on.",
  "After a rest week, feeling completely recharged and ready to train hard again.",
  "Resting heart rate back to baseline, sleep quality excellent the past three nights.",
  "5km time trial, smashed previous best by 45 seconds, legs felt like springs.",
  "Track session, every rep faster than the last, coach very happy.",
  "Body weight back to normal, energy levels high, appetite good.",
  "Morning run before work, felt effortless and enjoyable, great start to the day.",
  "Competed at regional level, podium finish, all preparation paid off."
)

berichte_injury <- c(
  "Crash during mountain biking, abrasions on arm, knee swollen, saw a doctor.",
  "Sharp pain in left Achilles tendon during warm-up, stopped immediately.",
  "Ankle rolled badly during trail run, significant swelling, going to hospital.",
  "Pulled hamstring at 7 km mark of race, had to withdraw.",
  "Stress fracture suspected in right tibia, MRI scheduled.",
  "Shoulder pain during swim, clicking sound, saw physio after session.",
  "Fell off bike, collarbone very painful, cannot lift arm above shoulder.",
  "Knee locked up during squat, swelling appeared within an hour.",
  "Back spasm during deadlift, cannot stand straight, session terminated.",
  "Blistered both feet badly during long run, skin broken and bleeding."
)

berichte_risk <- c(
  "Knee causing minor discomfort uphill, will keep an eye on it.",
  "Shoulder has been hurting for two days, range of motion slightly restricted.",
  "Mild shin pain after long run, icing tonight and will reassess tomorrow.",
  "Hip flexor feels tight after the ride, probably needs some extra stretching.",
  "Small blister developing on right heel, taped it up for now.",
  "Lower back felt stiff during deadlifts, reduced weight and stopped early.",
  "Calf cramping repeatedly during swim, probably dehydration.",
  "Knee tracking feels off when running downhill, booked physio appointment.",
  "IT band tightness returning after increasing mileage too quickly.",
  "Foot numbness after long cycling session, may need cleat adjustment."
)

# ── Datensatz zusammenstellen ─────────────────────────────
n <- 100

# Repräsentative Verteilung der Kategorien
kategorie <- sample(
  c("overloaded", "normal", "recovered", "injury", "risk"),
  size = n,
  replace = TRUE,
  prob = c(0.25, 0.35, 0.20, 0.10, 0.10)
)

bericht_text <- sapply(kategorie, function(k) {
  switch(k,
    "overloaded" = sample(berichte_overloaded, 1),
    "normal"     = sample(berichte_normal, 1),
    "recovered"  = sample(berichte_recovered, 1),
    "injury"     = sample(berichte_injury, 1),
    "risk"       = sample(berichte_risk, 1)
  )
})

trainingsberichte <- tibble(
  id          = 1:n,
  athlet      = sample(athleten, n, replace = TRUE),
  woche       = sample(1:20, n, replace = TRUE),
  bericht     = bericht_text,
  wahre_kategorie = kategorie  # für Musterlösung / Lehrende
)

# ── Export: zwei Versionen ────────────────────────────────

# Version für Studierende (ohne wahre Kategorie)
trainingsberichte |>
  select(id, athlet, woche, bericht) |>
  write.csv("trainingsberichte.csv", row.names = FALSE)

# Version für Lehrende (mit wahrer Kategorie als Musterlösung)
#write.csv(trainingsberichte, "trainingsberichte_loesung.csv", row.names = FALSE)

#cat("Datensatz erstellt: 100 Einträge,", 
 #   nrow(trainingsberichte), "Zeilen\n")
#table(trainingsberichte$wahre_kategorie)









library(dplyr)
library(mall)
library(rentrez)   # install.packages("rentrez")

llm_use("ollama", "llama3.2", seed = 42)

# PubMed-Suche
suche <- entrez_search(
  db = "pubmed",
  term = "athlete recovery AND (sleep OR nutrition OR HRV OR compression OR mindfulness)",
  retmax = 50
)

cat("Gefundene Artikel:", suche$count, "\n")
cat("Geladene IDs:", length(suche$ids), "\n")

#Abstracts herunterladen 
raw <- entrez_fetch(
  db      = "pubmed",
  id      = suche$ids,
  rettype = "abstract",
  retmode = "text"
)

# Abstracts in Dataframe

bloecke <- strsplit(raw, "\n\n\n")[[1]]
bloecke <- bloecke[nchar(trimws(bloecke)) > 100]  # Kurze Fragmente entfernen

literatur <- tibble(
  id         = seq_along(bloecke),
  pmid       = suche$ids[seq_along(bloecke)],
  abstract   = trimws(bloecke)
)

cat("Datensatz bereit:", nrow(literatur), "Einträge\n")
glimpse(literatur)

# Autoren + Jahr 
details <- entrez_summary(db = "pubmed", id = suche$ids[1:nrow(literatur)])

literatur <- literatur |>
  mutate(
    autor_jahr = sapply(suche$ids[1:nrow(literatur)], function(id) {
      tryCatch({
        s <- details[[id]]
        paste0(s$sortfirstauthor, " (", substr(s$pubdate, 1, 4), ")")
      }, error = function(e) paste0("PMID:", id))
    })
  ) |>
  select(id, pmid, autor_jahr, abstract)

#  CSV speichern
write.csv(literatur, "literatur.csv", row.names = FALSE)
cat("literatur.csv gespeichert mit", nrow(literatur), "echten PubMed-Abstracts\n")