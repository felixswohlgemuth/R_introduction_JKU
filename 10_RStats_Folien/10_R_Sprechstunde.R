### R Sprechstunde 14.05.20

## Start: 11:45

## Keine Aufzeichnung wegen Datenschutz und Dateigröße

# Vorgehen:
# Wir sammeln heute Fragen am Anfang der Sprechstunde
# 1. Im Chat schreiben ob Sie heute eine Frage haben
# 2. Ich rufe Sie auf


# Fragen
## Fürst
# Matrix / Tabelle händisch erstellen

# haendsich tabelle erstellen

# methode 1: matrix erstellen
# Werte 188, 234949, 19, 1, 8823, 82930
# Verteilt auf 3 Reihen und 2 Spalten 
# Werten werden nach der Reihe auf Spalten verteilt

t1 <- matrix(c(188, 234949, 19, 1, 8823, 82930), nrow = 3, ncol = 2, byrow = FALSE)
colnames(t1) <- c("yes", "no")
rownames(t1) <- c("agree", "neither", "disagree")



# methode 2. dataframe erstellen
# 2 Variable = Spalten mit je 3 Werten erstellen
yes <- c(188, 23494, 19)
no <- c(1, 8823, 82930)
t2 <- data.frame(yes, no)
# Reihennamen definieren
row.names(t2) <- c("agree", "neither", "disagree")
t2