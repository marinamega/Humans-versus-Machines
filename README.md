# Humans versus Machines: Comparing Manual and Automated Annotation of Benthic Communities

This repository contains the data and code used in the study *"Humans versus Machines: Artificial intelligence performs similarly to manual analysis in benthic reef monitoring"*, 
in which we compared the performance of manual (human) and automated (machine learning–based) annotators in the analysis of benthic photoquadrats, with a focus on taxonomic labels and morphofunctional groups.

The analyses quantify annotation performance using Cohen’s Kappa and explore sources of disagreement between annotators.

---

## Repository structure

### 📁 data/
Contains the datasets used in the analyses.

- `EVSET_EXP.csv`  
  Expert annotations from the evaluation set used as the reference baseline.

- `EVSET_AU.csv`  
  Annotations produced by the author (manual annotator).

- `EVSET_COM.csv`  
  Annotations produced by the automated (machine learning–based) annotator.

- `image_quality.xlsx`  
  Quantitative metrics and presence and absence image imperfections used to estimate image quality.

- `all_expert_annotations.xlsx`  
  All expert annotations from both training and evaluation sets, used to calculate benthic organism coverage.

---

### 📁 script/
R script used to perform the analyses.

- `HumansVSMachines_analyses.R`  
    - Data filtering, label harmonization, and dataset preparation;  
    - Calculation of annotator performance;  
    - Assessment of the effect of image quality on annotator performance.

---

## Contact

For questions regarding the data or analyses, please contact:  
**Marina Méga** (marinaamegaa@gmail.com)
