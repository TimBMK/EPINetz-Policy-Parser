# EPINetz-Policy-Parser

# Workflow

Der Policy Parser besitzt zwei Hauptbestandteile: Regelmäßige, automatisierte Updates der Termlisten (im live-Betrieb via Cronjobs) und die Klassifizierung neuer Dokumente in Echtzeit. Während die Updates der Termlisten vergleichsweise ressourcenintensiv sind, ist das Berechnen der Policy Scores in den Dokumenten wenig aufwändig, da lediglich diese lediglich in ihre Tokens zerlegt,* diese Tokens mit den Termlisten abgeglichen und darauf basierend die Gesamtscore berechnet wird.

Da der Zugriff auf die Twitter API für Forschende bis auf weiteres gesperrt ist, findet keine Live-Klassifizierung statt. Stattdessen wurde der Prozess für den Zeitraum von Oktober 2017 (Beginn der 19. Legislaturperiode) bis zum Ende des Datenzugriffs am 22.6.2023 simuliert. Die Twitterdaten basieren auf dem [EPINetz Twitter Politicians Dataset 2021](https://doi.org/10.1007/s11615-022-00405-7) und dessen [Erweiterung bis zum Jahr 2023](https://doi.org/10.7802/2415) (inkl. der Wiederholungswahlen in Berlin). 

Workflows für die regelmäßige Klassifikation von Dokumenten via API finden sich unter **regular_classification**.

Die Dokumentation der initialen Klassifikation aller verfügbaren Twitterdaten seit Oktober 2017 findet sich unter **init_classification**.

Die Daten wurden über die Heidelberger EPINetz Database bezogen.

Ein ausführlicher qualitativer Evaluationsprozess, welcher in den Issues dieses Repositoriums dokumentiert ist, wurde durchgeführt, um den Prozess zu finetunen und optimale Werte für das Modell zu wählen.

## 0. Tokenization 

Regelmäßige Tokenisierung der (neuen) Dokumente via Spacy / SpacyR. Neben einer vollständigen Database wird ein reduzierter Datensatz erstellt, welcher nur den maximal benötigten Zeitraum von einem Jahr beinhaltet. 

**Zuständige Skripte: Tokenizer/regular_tokenization_update.R (live) / Tokenizer/init_tokenization.R (Oktober 2017 - Juni 2023)**

## 1. Regelmäßige Updates der Termlisten

Die Klassifizierung der Dokumente erfolgt auf Basis von Termlisten, welche in zwei Schritten erstellt werden: a) Seed-Term-Extraktion, b) Erweiterung der Seed-Terme mittels Random Walks. Seed-Terme werden mittels der Seed-Accounts aus dem letzten Jahr erstellt. Ausgehend von diesen Seed-Termen werden über alle Tweets der vorangegangenen drei Monaten [Random Walks with Restart (RNR)](https://github.com/alberto-valdeolivas/RandomWalkRestartMH) durchgeführt, um erweiterte Termlisten zu erhalten.

### Seed-Term-Extraktion

1. Read-in der Listen an Ausschüssen, Ministerien und Ausschussmitgliedern und der Twitter User IDs
2. Data Read-in der tokenisierten Daten I: tokenisierte Tweets der Seed-Accounts (Ministerien, Ausschüsse & Ausschussmitglieder) von Datum X - 12 Monate
3. Replies auf Tweets verwenden, URL-Tokens verwenden, @mention-Tokens NICHT verwenden (@mentions werden durch Replies zu prävalent; Replies haben teilweise relevante Informationen)
4. Berechnung des Chi^2-Werts für Terme der zum jeweiligen Zeitpunkt aktiven Committees/Member
5. Filterung der Terme nach Chi^2, Minimalwerte: 250 für Ausschussmitglieder, 150 für Ministerien, 40 für Ausschüsse; für Ministerien: maximal 230 Seedterme pro Periode und Account, mindestens 100 (sonst Korrektur des Chi^2-cutoffs); für Ausschüsse: maximal 200, mindestens 35; keine min/max Anzahl Seedterme für Ausschussmitglieder (nur Chi^2-cutoff) 
6. Gefilterte Terme der Ausschussmitglieder von den restlichen Termen des jeweiligen Ausschusses *abziehen*, um individuelle Agenden (z.B. Wahlkampf) herauszufiltern 
7. Ergebnis: Dataframe an Seed-Termen für jede Periode als .csv, mit Variablen feature, chi^2, policy_field, committee/ministry/committee_member, period (gesammelte Datasets für initiale Evaluation)
8. Optional: Quanteda-Plots für Top-Terme jeden Account, um Ergebnisse zu evaluieren

**Zuständige Funktion: get_seed_terms()**

### Erweiterte Termlisten

1. Data Read-in der tokenisierten Daten II: alle EPINetz-Tweets von Datum X - 12 Wochen (Wichtig: Query auf EPINetz Accounts beschränken)
2. Replies auf Tweets NICHT verwenden (Resultate für RWR nicht gut genug, um computationellen Mehraufwand zu rechtfertigen), URLs und @mentions verwenden (erleichtert Klassifikation sehr kurzer Tweets)
3. unteres 10% Quantil der Tokens nach Häufigkeit wird entfernt (wenig relevant; macht Netzwerkberechnung deutlich effizienter) 
4. Read-in der Seed Terme
5. Bilden des PMI-gewichteten Netzwerks aus den Noun-Words der tokenisierten Tweets, ablegen der einzelnen Netzwerke als .RDS und späterer Readin für die RWR-Berchnung (stabiler und minimiert Ressourcenaufwand)
6. RWR für Seed Terme auf PMI Netzwerk berechnen, Settings: nur positive Scores (d.h. negative Walk Scores, die durch negative PMIs zustande kommen, werden vor der Normalisierung entfernt); Normalisierung über Seeds (d.h. für jeden Seed Walk) und Berechnung der Mittelwerte jedes Terms über alle Walks eines Poltikfelds für den Zeitraum; erneute Normalisierung der Mittelwere pro Politikfeld; 90% Quantile als Cutoff-Wert der normalisierten Means; Seedterms werden unabhängig von ihrer Score behalten (diese wird jedoch nicht verändert)
7. Ergebnis: Erweiterte Termlisten (inkl. Seedterme) für jede Periode als .csv, mit den Variablen policy_field, NodeNames (walk Term), seed_term-Indikator, ScoreMean (Mittelwert der nicht-normalisierten RWR-Scores) und ScoreNormMean (normalisierte Mittelwerte der normaliserten RWR-Scores; *ScoreNormMean wird für die Klassifizierung der Tweets verwendet*

**Zuständige Funktionen: make_multiplex_objects() (Erstellung der Netzwerke), get_rwr_terms()**

## 2. (live) Klassifikation neuer Dokumente

Falls live Klassifikation notwendig wird, wird Heidelberg in ihrer Pipeline einen Call zu unserer API einbauen, der im live Betrieb Dokumente klassifiziert. Im live Betrieb ist die wöchentliche Berechnung und Datum X + 1 Woche möglich, um durch Zugriff auf jeweils letzte Termliste effizient in Echtzeit zu klassifizieren, statt Seed-Terme und RWRs immer neu zu berechnen.

1. Data Read-in der tokenisierten Daten III: alle EPINetz-Tweets von Datum X - 1 Woche (Wichtig: Query auf EPINetz Accounts beschränken)
2. Replies auf Tweets NICHT klassifizieren (idR. Texte zu kurz und uninformativ), @mentions und URLs verwenden (Vorgehen hier identisch mit Erweiterung der Termlisten)
3. Read-in der erweiterten Termlisten
4. Klassifizierung der Dokumente nach Politikfeldern auf Grundlage der in ihnen vorkommenden Terme und ihrer ScoreNormMean Werte. Settings: keine Veränderung der Seedterm-Werte, kein zusätzlicher Cutoff; in mehreren Politikfeldern vorkommende Terme werden NICHT entfernt
5. Normalisierung der Scores über alle Dokumente eines Poltikfelds in einem Zeitraum; untere 10% Quantile der Scores pro Politikfeld werden auf 0 gesetzt (10% schlechteste Ergebnisse werden ignoriert)
6. Ergebnis: Dataframe mit doc_id (Tweet ID), policy_field, score (nicht normalisiert) und score_norm (pro Politikfeld im Zeitraum normalisiert)
7. Optional: Ausgabe der erweiterten Termlisten (walk_terms) und IDs der nicht klassifizierten Dokumente
8. Weitergabe an Heidelberg als Dataframe im wide-Format mit der Spalte tweet_id und 1 Spalte mit normalisierter Score pro Politikfeld

**Zuständige Funktion: classify_documents()**


## Optional: Visualisierung der Politikfelder

Die so berechneten Politikfelder lassen sich als Netzwerke visualisieren, bei denen eine Anzahl an prävalentesten Termen (n) angezeigt wird. 

*Variante 1:* Darstellung jedes einzelnen Politikfelds in einem Zeitraum, wobei die Knotengröße die Politikfeldscore (ScoreNormMean) eines Terms und die Kantengröße zwischen den Termen deren PMI (gemeinsames Auftreten) in für dieses Subset an Termen im Zeitraum pro Politkfeld darstellt. So können relevante Terme und deren gemeinsames Auftreten in Dokumenten erfasst werden.
![policy_field_network_single](https://github.com/EPINetz/EPINetz-Policy-Parser/assets/32549063/f4f96d61-7769-4239-92f3-a0947e88677d)


*Variante 2:* Darstellung der Politikfelder in einem Zeitraum, wobei die Kantengröße zwischen den Knotentypen Politkfeld und Term die Politikfeldscore des letzteren darstellt. So können relevante Terme und deren Überschneidungen in mehreren Politikfeldern erfasst werden.
![policy_fields_network](https://github.com/EPINetz/EPINetz-Policy-Parser/assets/32549063/45ee9715-af2a-489c-9a6f-6276e73aad70)

**Zuständiges Skript: policy_field_visualisation.R**


## Zusätzliche Hilfsfunktionen

**utils_text_processing.R** enthält eine Reihe von Hilfsfunktionen, um Tokens und Dokumente zu verarbeiten

## Credits
Erstellung des Prototypen für den Klassifizierungsprozess: Erik Wolfes-Wenker & Tim König

Erstellung der Funktionen, Workflows und Skripte für Evaluation, regelmäßige und initiale Klassifikation: Tim König

Qualitative Evaluation der Ergebnisse und Finetuning der Parameter: Erik Wolfes-Wenker, Marco Gronewold & Quentin Bukold

Random Walk Algorithmus: [Alberto Valdeolivas](https://github.com/alberto-valdeolivas/RandomWalkRestartMH)
> A Valdeolivas, L Tichit, C Navarro, S Perrin, G Odelin, N Levy, P Cau, E Remy, and A Baudot. 2018. “Random walk with restart on multiplex and heterogeneous biological networks.” Bioinformatics 35 (3)

