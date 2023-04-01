# EPINetz-Policy-Parser

## Workflow

Der Policy Parser besitzt zwei Hauptbestandteile: Regelmäßige, automatisierte Updates der Termlisten (im live-Betrieb via Cronjobs) und die Klassifizierung neuer Dokumente in Echtzeit. Während die Updates der Termlisten vergleichsweise ressourcenintensiv sind, ist das Berechnen der Policy Scores in den Dokumenten wenig aufwändig, da lediglich diese lediglich in ihre Tokens zerlegt,* diese Tokens mit den Termlisten abgeglichen und darauf basierend die Gesamtscore berechnet wird.

* evtl. direkt zerlegte Dokumente aus NLP-Pipeline beziehen

### 1. Regelmäßige Updates der Termlisten

Wöchentliche Erstellung der Termlisten. Seed-Terme der Seed-Accounts aus dem letzten Jahr. Ausgehend von Seed-Termen Random Walks über alle Tweets der vorangegangenen drei Monaten, um erweiterte Termlisten zu erhalten.

#### Seed-Term-Extraktion

!! Offene Frage: ist händische Evaluation in der Extraktion der Seed Terme nötig? !!

1. Read-in der Listen an Ausschüssen, Ministerien und Ausschussmitgliedern und der Twitter User IDs

2. Data Read-in von der EPINetz-Datenbank I: Tweets der Seed-Accounts (Ministerien, Ausschüsse & Ausschussmitglieder) von Datum X - 12 Monate

2. Berechnung des Chi^2-Werts für Terme

3. Filterung der Terme nach Chi^2, Werte variabel (default: 250 für Ausschussmitglieder, 500 für Ministerien, 30 für Ausschüsse)

4. Ergebnis: Liste an Seed-Termen pro Policy Feld als .csv


#### Erweiterte Termlisten

1. Data Read-in von der EPINetz-Datenbank II: alle EPINetz-Tweets von Datum X - 3 Monate (Wichtig: Query auf EPINetz Accounts beschränken)

2. Read-in der Seed Terme

3. 

### 2. (live) Klassifikation neuer Dokumente

