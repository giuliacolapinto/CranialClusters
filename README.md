# Progetto di Analisi Cranometrica

L'obiettivo dello script è confrontare diverse metodologie di clustering e classificazione supervisionata per distinguere i campioni maschili e femminili nel dataset Howell.

## Pre-processing e Pulizia Trattamento

**Sinostosi**: Esclusione di 11 variabili qualitative (es. RFA, RPA, BSA) a causa di valori nulli derivanti dalla fusione ossea nei soggetti anziani.

**Standardizzazione**: Centratura e scalatura dei dati raggruppati per popolazione/tribù per rimuovere bias geografici.

**Selezione Feature**: Utilizzo della PCA per identificare le variabili con i caricamenti (loadings) più alti (es. NAR, GOL, ZYB).

## Modelli Testati

### A. Gaussian Mixture Models (V-Fold Cross Validation)

#### A.1. V-Fold CV con 18 Variabili Originali

In questa fase sono stati condotti test iterativi (100 simulazioni per ogni blocco V) utilizzando il pacchetto `Rmixmod` sulle variabili selezionate (es. `NAR`, `GOL`, `ZYB`, `BNL`).

-   **Configurazioni Testate:** Sono stati valutati blocchi di cross-validation con $V = 5, 8, 10, 12, 15$.

-   **Miglior Modello per Errore (CV):** Il modello **`Gaussian_pk_L_C`** (volume costante, forma uguale, orientamento libero) ha costantemente minimizzato l'errore di cross-validation, con un valore di circa **0.122**.

-   **Miglior Modello per Accuratezza:** Il modello **`Gaussian_pk_Lk_C`** (volume variabile) ha invece massimizzato l'accuratezza predittiva sul test set, raggiungendo picchi del **82.6%**.

-   **Conclusione Parziale:** Nonostante le variazioni di $V$, i risultati sono rimasti stabili, indicando che una struttura a volume variabile descrive meglio la distribuzione dei dati originali.

#### A.2. V-Fold CV con PCA (Dimensioni Ridotte)

L'analisi è stata ripetuta utilizzando solo le prime due componenti principali (PC1 e PC2) per verificare se la riduzione del rumore migliorasse la classificazione.

-   **Configurazioni Testate:** Blocchi di $V = 10, 20, 60, 100$.

-   **Miglior Modello per Errore (CV):** I modelli **EVE** (`Gaussian_pk_L_D_Ak_D`) e **EEV** sono risultati i più bilanciati. Il valore di CV ottenuto è stato di circa **0.145-0.146**, leggermente superiore (quindi peggiore) rispetto alle variabili grezze.

-   **Miglior Modello per Accuratezza:** Anche con la PCA, l'accuratezza massima si è attestata intorno al **81-82%**.

-   **Visualizzazione:** È stata prodotta un'analisi delle missclassification tramite scatter plot, evidenziando come gli errori (punti neri) si concentrino nelle zone di sovrapposizione degli ellissi di confidenza tra i due sessi.

### B. Clustering K-means

Testato come approccio non supervisionato con K=2.

**Risultato:** Non soddisfacente, con un tasso di errore del **21.3%** e un Adjusted Rand Index (ARI) basso (0.32).

### C. Modelli Mclust Ottimizzati

È stata eseguita una ricerca iterativa (200 simulazioni) per trovare il subset ottimale di 10 variabili e il miglior modello probabilistico:

-   **Feature finali:** `MAB`, `FRC`, `AUB`, `GOL`, `NOL`, `AVR`, `JUB`, `ZYB`, `NPH`, `FMB`.

-   **Modello:** **VVE** (Volume variabile, forma uguale, orientamento uguale).

-   **Performance:** Accuratezza sul test set del **83%**.
