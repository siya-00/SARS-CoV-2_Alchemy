## **SARS-CoV-2 Alchemy**: Unraveling the Dynamics of Age, Vaccination, and Geography in the Evolution of SARS-CoV-2 in India

#### Inputs Files:
- india_metadata.tsv: Contains SARS-CoV-2 metadata with information such as accession number, collection date, location (Indian states/union territories), age, and gender.
- group1.tsv, group2.tsv, group3.tsv: Contains Variant Call information with sample-specific information about mutations in SARS-CoV-2 genomes.

Note : The files india_metadata.tsv, group1.tsv, group2.tsv, and group3.tsv are available for download [here](https://drive.google.com/drive/folders/1NCqSf9C5ka964DEtmaYRemwaUONn4iUu?usp=sharing). Entire raw datasets(FASTA file and metadata file) can also be downloaed via GISAID.

### Key Features:

- Cleans and processes metadata (e.g., age, gender, location) for SARS-CoV-2 samples.
- Standardizes location, age, and gender values for consistency.
- Joins metadata with mutation data(VCF).
- Identifies mutations present in at least 5 samples.
- Segregates data by age groups (0-17, 18-64, 65-100 years).
- Performs random sub-sampling to balance sample sizes across age groups.

