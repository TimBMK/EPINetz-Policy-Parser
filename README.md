# EPINetz-Policy-Parser

# Workflow

Der Policy Parser besitzt zwei Hauptbestandteile: Regelmäßige, automatisierte Updates der Termlisten (im live-Betrieb via Cronjobs) und die Klassifizierung neuer Dokumente in Echtzeit. Während die Updates der Termlisten vergleichsweise ressourcenintensiv sind, ist das Berechnen der Policy Scores in den Dokumenten wenig aufwändig, da lediglich diese lediglich in ihre Tokens zerlegt,* diese Tokens mit den Termlisten abgeglichen und darauf basierend die Gesamtscore berechnet wird.

Da der Zugriff auf die Twitter API für Forschende bis auf weiteres gesperrt ist, findet keine Live-Klassifizierung statt. Stattdessen wurde der Prozess für den Zeitraum von Oktober 2017 (Beginn der 19. Legislaturperiode) bis zum Ende des Datenzugriffs am 22.6.2023 simuliert. Die Twitterdaten basieren auf dem [EPINetz Twitter Politicians Dataset 2021](https://doi.org/10.1007/s11615-022-00405-7) und dessen [Erweiterung bis zum Jahr 2023](https://doi.org/10.7802/2415) (inkl. der Wiederholungswahlen in Berlin). 

Die Daten wurden über die Heidelberger EPINetz Database bezogen.

Ein ausführlicher qualitativer Evaluationsprozess, welcher in den Issues dieses Repositoriums dokumentiert ist, wurde durchgeführt, um den Prozess zu finetunen und optimale Werte für das Modell zu wählen.

## 0. Tokenization 

Regelmäßige Tokenisierung der (neuen) Dokumente via Spacy / SpacyR. Neben einer vollständigen Database wird ein reduzierter Datensatz erstellt, welcher nur den maximal benötigten Zeitraum von einem Jahr beinhaltet. 

**Zuständige Funktionen: Tokenizer/regular_tokenization_update.R (live) / Tokenizer/init_tokenization.R (Oktober 2017 - Juni 2023)**

## 1. Regelmäßige Updates der Termlisten

Die Klassifizierung der Dokumente erfolgt auf Basis von Termlisten, welche in zwei Schritten erstellt werden: a) Seed-Term-Extraktion, b) Erweiterung der Seed-Terme mittels Random Walks. Seed-Terme werden mittels der Seed-Accounts aus dem letzten Jahr erstellt. Ausgehend von diesen Seed-Termen werden über alle Tweets der vorangegangenen drei Monaten [Random Walks with Restart (RNR)](https://github.com/alberto-valdeolivas/RandomWalkRestartMH) durchgeführt, um erweiterte Termlisten zu erhalten.

### Seed-Term-Extraktion

1. Read-in der Listen an Ausschüssen, Ministerien und Ausschussmitgliedern und der Twitter User IDs
2. Data Read-in der tokenisierten Daten I: tokenisierte Tweets der Seed-Accounts (Ministerien, Ausschüsse & Ausschussmitglieder) von Datum X - 12 Monate
3. Berechnung des Chi^2-Werts für Terme
4. Filterung der Terme nach Chi^2, Werte variabel (250 für Ausschussmitglieder, 150 für Ministerien, 40 für Ausschüsse); für Ministerien: maximal 230 Seedterme pro Periode und Account, mindestens 100 (sonst Korrektur des Chi^2-cutoffs); für Ausschüsse: maximal 200, mindestens 35; keine min/max Anzahl Seedterme für Ausschussmitglieder (nur Chi^2-cutoff) 
5. Gefilterte Terme der Ausschussmitglieder von den restlichen Termen des jeweiligen Ausschusses *abziehen*, um individuelle Agenden (z.B. Wahlkampf) herauszufiltern 
6. Ergebnis: Dataframe an Seed-Termen für jede Periode als .csv, mit Variablen feature, chi^2, policy_field, committee/ministry/committee_member, period (gesammelte Datasets für initiale Evaluation)
7. Optional: Quanteda-Plots für Top-Terme jeden Account, um Ergebnisse zu evaluieren

**Zuständige Funktion: get_seed_terms()**

### Erweiterte Termlisten

1. Data Read-in der tokenisierten Daten II: alle EPINetz-Tweets von Datum X - 12 Wochen (Wichtig: Query auf EPINetz Accounts beschränken)
2. Read-in der Seed Terme
3. Bilden des PMI-gewichteten Netzwerks aus den Noun-Words der tokenisierten Tweets
4. RWR für Seed Terme auf PMI Netzwerk
5. Normalisierung der Scores
6. Ergebnis: Erweiterte Termlisten pro Policy Feld als .csv

**Zuständige Funktionen: **

## 2. (live) Klassifikation neuer Dokumente

HD wird in ihrer Pipeline einen Call zu unserer API einbauen, der im live Betrieb Dokumente klassifiziert. 

1. Read-in der Dokumente
2. Tokenization
3. Read-in der erweiterten Termlisten
4. Klassifizierung der Dokumente auf Grundlage der in ihnen vorkommenden Terme und mit ihnen assoziierter Policy Felder
5. Normalisierung der Scores über alle Policy Felder - Wahrscheinlichkeit eines Dokuments, zu einem Policy Feld zu gehören  - als Option
6. Ergebnis: 1 Named Vektor pro Dokument mit 1 Wert pro Policy Feld und Name des Policy Felds

**Zuständige Funktion:**
