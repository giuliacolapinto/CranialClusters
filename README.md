# Cranometric Analysis Project

The goal of the script is to compare different supervised clustering and classification methodologies for distinguishing male and female samples in the Howell dataset.

## Pre-processing and Cleaning Treatment

**Synostosis**: Exclusion of 11 qualitative variables (e.g. RFA, RPA, BSA) due to null values ​​resulting from bone fusion in elderly subjects.

**Standardization**: Centering and scaling of data grouped by population/tribe to remove geographic bias.

**Feature Selection**: Using PCA to identify variables with the highest loadings (e.g., NAR, GOL, ZYB).

## Tested Models

### A. Gaussian Mixture Models (V-Fold Cross Validation)

#### A.1. V-Fold CV with 18 Original Variables

In this phase, iterative tests (100 simulations for each V block) were conducted using the `Rmixmod` package on the selected variables (e.g. `NAR`, `GOL`, `ZYB`, `BNL`).

-   **Configurations Tested:** Cross-validation blocks with $V = 5, 8, 10, 12, 15$ were evaluated.

-   **Best Model by Error (CV):** The **`Gaussian_pk_L_C`** model (constant volume, equal shape, free orientation) consistently minimized the cross-validation error, with a value of approximately **0.122**.

-   **Best Model for Accuracy:** The **`Gaussian_pk_Lk_C`** (variable volume) model instead maximized predictive accuracy on the test set, reaching peaks of **82.6%**.

-   **Partial Conclusion:** Despite the variations in $V$, the results remained stable, indicating that a variable volume structure better describes the distribution of the original data.

#### A.2. V-Fold CV with PCA (Reduced Size)

The analysis was repeated using only the first two principal components (PC1 and PC2) to test whether noise reduction improved classification.

-   **Configurations Tested:** Blocks of $V = 10, 20, 60, 100$.

-   **Best Model for Error (CV):** The **EVE** (`Gaussian_pk_L_D_Ak_D`) and **EEV** models were found to be the most balanced. The CV value obtained was approximately **0.145-0.146**, slightly higher (and therefore worse) than the raw variables.

-   **Best Model for Accuracy:** Even with PCA, the maximum accuracy was around **81-82%**.

-   **Visualization:** An analysis of the misclassifications was produced via scatter plot, highlighting how the errors (black dots) are concentrated in the overlapping areas of the confidence ellipses between the two sexes.

### B. Clustering K-means

Tested as an unsupervised approach with K=2.

**Result:** Unsatisfactory, with an error rate of **21.3%** and a low Adjusted Rand Index (ARI) (0.32).

### C. Optimized Mclust Models

An iterative search (200 simulations) was performed to find the optimal subset of 10 variables and the best probabilistic model:

-   **Final feature:** `MAB`, `FRC`, `AUB`, `GOL`, `NOL`, `AVR`, `JUB`, `ZYB`, `NPH`, `FMB`.

-   **Model:** **VVE** (Variable Volume, Equal Shape, Equal Orientation).

-   **Performance:** Accuracy on the test set of **83%**.

# ITALIAN VERSION:

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
