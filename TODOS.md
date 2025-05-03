
# IMLUI

## Checkn PMLB

Check whether we can use datasets from PMLB. PMLB is "a large benchmark suite for machine learning evaluation and comparison".

## Database editing verbessern

Folgende Verbesserungen sollten implementiert werden:

- [ ] Editing schreibt nicht direkt in die Datenbank, sondern erstmal nur in die Tabelle, d.h. `ses$tbl[[tbl_name]]`. Vorher wird ein Backup der Tabelle angelegt `ses$rv$tbl$bak[[tbl_name]]`.
- [ ] Es gibt Buttons für `reset changes`, `submit changes` und `add row`
- [ ] Bei `reset changes` wird die Tabelle neu anhand des Backups initialisiert  `ses$rv$tbl[[tbl_name]] <- ses$rv$tbl$bak[[tbl_name]]`.
- [ ] Bei `submit changes` wird die DB anhand der Änderungen beschrieben
- [ ] Bei `add row` wird eine neue Zeile angelegt

## Switch from Wiki to Pkgdown

## Restliche Todos ausformulieren

- [ ] Add dataset-feature-mappings to enable use of IPI / aaIPI
- [ ] Make sure every model / dataset has a description
- [ ] FL Data from Master Thesis einbinden
- [ ] FL Modelle von Ellen einbinden
- [ ] tux1404 auf Postgres umstellen
- [ ] Implement `db__check`, which does the following:
  ```
  for each table:
    if not existing: {create}
    for each column of table: {if not existing: create}
    for row of table: {if not existing: create}
  ```
- [ ] Estimatex23data einbinden
- [ ] R Objects aus Szecpanowski Datensatz bauen
- [ ] Make R Objects from Kube14 Data
- [ ] Make R Objects from Frank's / Thorsten's Data
- [ ] Write function to generate the following wiki entries automatically from code:
  - [ ] `Module-Reference.md`, should contain list of all modules sorted alphabetically
  - [ ] `Function_reference.md`, should contain list of all functions sorted once by Module and once alphabetically.
  - [ ] `modules/<mod>.md`, for each module `mod`
  - [ ] `functions/<func>.md`, for each function `func`
- [ ] Make img cache unique for each uid
- [ ] Enable remaining third party authentications
- [ ] Migrate toscplot to Github and setup Github Actions
- [ ] Add method to visualize data preprocessing steps
- [ ] Training / Test Data von Nicole richtig einpflegen
- [ ] FEP: Fix coloring of lines in plot (true classification based on threshold should be used!)
- [ ] FEP: Draw feature names in column center
- [ ] FEP: Add option to show/hide feature names
- [ ] Add feature mapping tab showing which feature of which model signature should be mapped to which dataset feature
- [ ] Add survival curves with variable threshold (e.g. *quartiles*, *median* or *fixed threshold*)
- [ ] Add option to show feature names as numbers within plot and with a legend beside the plot.
- [ ] Add redraw button to sidebar and isolate every sidebar setting
- [ ] Implement interface to upload new data
- [ ] Implement handling of intercepts
- [ ] Add raw LAMIS dataset, log2 transformed dataset, full normalized dataset (i.e. make steps comprehensible)
- [ ] Add titles to plots showing used package, package author, method and paper where method is described
- [ ] Add option to plot test data as boxplots instead of lines
- [ ] Add option to draw "extreme" samples as "zic-zac" line
- [ ] Add default tresholds and papers to each model and dataset
- [ ] Make Datasets specific for given method
- [ ] Add slider for alpha value and point size
- [ ] Only store plots in certian width/height ratios
- [ ] Generate unique filename based on all function arguments
- [ ] Add options to plot feature effects as horizontal rectangles (probably new function)
- [ ] Add plot for shapley values / feature contributions of 0..n samples
- [ ] Add option to sort samples by clustering
- [ ] Add 2D visualization tSNE, UMAP, PCA of datasets. Talk with Marian and Jakob first.
- [ ] Add options for scaling of 2D visualization (e.g. "center/scale to standard units" or "center and scale with inverse feature weight")
- [ ] Highlight "non-flippable" features (e.g. with a `*`). Non-flippable means, the classification would not swap, even if the feature would change by a "huge" amount. "Huge" in this context means: within the feature range of all samples.
- [ ] `lenz.R` fertigstellen und pushen
- [ ] Toscdata Docu updaten und in einzelne Packages zerlegen
