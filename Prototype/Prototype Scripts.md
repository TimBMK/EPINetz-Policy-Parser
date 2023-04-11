# EPINetz-Policy-Parser
# Prototype Scripts

Ich habe nun Material für dich zusammengestellt, das sich befindet in: /data/EPINetz/EPINetz-Policy-Parser/RWR. Hier kommen noch ein paar Erläuterungen dazu inkl. Angabe von Stellschrauben, an denen noch gedreht werden kann (insbesondere Veränderung gewisser Cutt-offs):

- Random_Walks_20230331
Dies ist das Skript für die Random Walks. Das Paket, mit dem die RWRs berechnet werden, ist RandomWalkRestartMH (https://github.com/alberto-valdeolivas/RandomWalkRestartMH/; Installationshinweise und Dokumentation).Erläuterungen zu den einzelnen Bestandteilen dieses Codes:

## 1. Read Network
Hier wird der Netzwerk-Snapshot (bisher Quartal) eingelesen und in ein igraph-Objekt umgewandelt, auf welches später das RWR-Paket zugreift bzw. in dem die RWRs stattfinden.

## 2. Seed Set Preparation
Hier werden zuerst die durch Termextraktion generierten policy-spezifischen Ausschuss-Seeds eingelesen (Objekt seeds_committees).
Stellschraube: Variation des Chi2-Wertes (tstat_key <- tstat_key[tstat_key$chi2>30, ]) - wie wird also die Grenze definiert, ab derer Terme in das Set aufgenommen werden?

Dann werden die durch Termextraktion generierten ausschussmitgliederspezifischen Ausschuss-Seeds eingelesen (Objekt members_terms). Diese werden anschließend aus seeds_committees herausgefiltert (damit z.B. Begriffe aus dem Wahlkampfkontext entfernt werden).
Stellschraube: Veränderung des Chi2-Wertes (tstat_key <- tstat_key[tstat_key$chi2>250, ])

Dann werden die durch Termextraktion generierten Minsterium-Seeds eingelesen (Objekt seeds_ministries).
Stellschraube: Variation des Chi2-Wertes (tstat_key <- tstat_key[tstat_key$chi2>500, ])

Schließlich wird eine Master seed list (Objekt seeds_list_vec) erstellt, in der Ausschuss- und Ministerium-Seeds je Politikbereich kombiniert werden (Gesamtliste, die politikbereichsspezifische Einzellisten von Termen enthält). Dieser Liste werden später die Seeds als Ausgangspunkte für die Random Walks entnommen.

## 3. RWR - Single Seeds
Hier werden die RWRs jeweils ausgehend von einem Seed-Term durchgeführt (es gibt also eine RWR-Berechnung pro Seed).Es wird zunächst für das Gesamtnetzwerk ein sog. multiplexes Objekt erstellt. Das Paket wurde für komplexe biologische Netzwerke entwickelt, aber es geht auch mit - wie in unserem Fall - einfachen Netzwerken um, die also gleichfalls eingespeist werden können. Es wird entsprechend das NE-Netzwerk, hier ein Netzwerk für ein gesamtes Quartal, übergeben. Anschließend erfolgt die Umwandlung in das benötigte Matrix-Format.Daraufhin wird die Normalisierungsfunktion für die Score-Normalisierung definiert (Min-Max-Normalisierung).
Stellschraube: Normalisierungsmethode

Für jeden Politikbereich (Rückgriff auf die Master seed list) werden dann ausgehend von den jeweiligen Seeds die RWRs durchgeführt und die so identifizierten Terme samt ihren Affinity-Scores in einer Gesamtliste (pro Politikbereich) gesammelt, allerdings nur Terme mit hohen Affinity-Scores.
Stellschraube: Variation des Score-Cut-offs (Grenze für Aufnahme der identifizierten Terme als Top Terme in die Gesamtliste)

Nach der Berechnung der RWRs findet für jeden Politikbereich noch die Verarbeitung der Gesamtliste statt: Hinzufügen der Seed Nodes selbst, Umgang mit mehrfach identifizierten Termen, erneute Normalisierung (weil nur Top Terme mit hohen Scores aufgenommen wurden, so erhält man wieder Werte zwischen 0 und 1)

Bitte beachte: Im Codebereich zur Berechnung der RWRs für den Politikbereich Arbeit habe ich gerade noch zusätzliche Kommentare eingefügt, die dir hoffentlich beim Nachvollziehen helfen.

- data_network_snapshots_EW
Dies ist dein Skript für die Snapshot-Erstellung mit meinen Anpassungen (hier wird der Output topic_graphs_quarter201901.csv.tar.gz erstellt, der im oben beschriebenen Skript genutzt wird).
Stellschraube: Variation der Zeiträume (Quartale, Monate, Wochen)

- Terms_Illustration_Docs
Dies ist dein Skript für die Klassifikation der Dokumente mit meinen Anpassungen.
