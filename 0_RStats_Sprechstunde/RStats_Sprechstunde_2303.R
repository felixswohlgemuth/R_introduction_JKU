# R Sprechstunden 
# 23.03.20 15:00 - 16:00

# Frage: In welcher Kodierung soll man das R Skript abspeichern?
## Am besten in mit UTF-8-Kodierung, dann können Sie es besser mit Leuten mit nicht deutschsprachigen PCs teilen
## Objektnamen sollten keine Umlaute enthalten

ueber <- c(1:10)
ueber

# Schritte am Anfang eines neuen Projektes in R
# 1. Neue Session starten: Session > New Session
##   alternativ alle geladenen Elemente löschen mit rm(list = ls())
# 2. neues R Skript erstellen
# 3. R Skript in Projektordner speichern
# 4. Projektordner als Working Directory setzen
##   Entweder in RStudio: Session > Set Working Directory > To Source File Location (das ist der Ordner mit dem R Srkipt)
##   Oder:
##   - aktueller Dateipfad mit getwd() nachschauen

getwd()

##   - kopieren Sie den Dateipfad und adaptieren Sie ihn so, dass er zu ihrem Projektordner führt
##   - Pfad zu Ihrem Projektorder in setwd() mit " " setzen und ausführen

setwd("ORDNERPFAD")

# Falls Sie die Working Directory per Befehlt festlegen, bitte nicht im R Skript speichern.
# Andere Personen die mit Ihrem R Skript arbeiten, haben nicht die gleiche Ordnerstruktur wie Sie.
# Falls Sie auf einem anderen Computer mit anderer Ordnerstrukur arbeiten, wird ihr setwd() Befehl
# im Skript nicht funktionieren
# rm(list = ls()) löscht alle Elemente. Andere Personen die mit Ihrem Skript arbeiten möchten das vielleicht nicht
# twitter Dikussion warum es kein Teil des Skriptes sein sollte:
# https://twitter.com/hadleywickham/status/940021008764846080?s=20



