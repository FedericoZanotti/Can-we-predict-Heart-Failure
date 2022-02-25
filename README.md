# Can we predict Heart Failure
This project is the result of the course Statistical Learning in which I have analyzed a dataset composed by medical records of 299 heart failure patients. R language is used.
This repositories contains different files. First of all Data folder contains 2 files:
- dati_modello_progetto.xlsx: this file is a summary of the results
- heart_failure_clinical_records_dataset.csv: the dataset used
Then the other files are the source code of the project:
- model_project.r contains the statistical analysis of the dataset
- GLM.r contains the analysis for the Generalized Linear Model
- LDA.r contains the analysis for the Linear Discriminant Analysis
- QDA.r contains the analysis for the Quadratic Discriminant Analysis
- utility.r contains some utility custom functions

The report with the description of the work is called Zanotti_Federico_report.pdf and presents this structure:
1. Introduction to the problem
2. Cleaning and Filtering data
3. Statisical analysis of the dataset
4. Description of the models employed
5. Final Results

In this work I used GLM, LDA, QDA models in order to predict an heart failure. The metrics used are Precision, Recall and F1-score, due to the size of the dataset.
The final results are listed in dati_modello_progetto.xlsx.
Here I report the conclusions:
In my work, an important and interesting evidence to point out is the fact that age, ejection fraction and
serum creatinine (some medical parameters present in the dataset) are the most relevant features, and we only need these to detect if a patient could have an Heart
Failure. In fact GLM with these features has an high recall (83%), meaning that the model well identifies
when a subject has an Heart Failure. This result is encouraging for the hospitals, because even if some
clinical features of a patient are missing or incomplete, doctors are able to predict patient survival by
analyzing the ejection fraction and serum creatinine values according also to the age. 

