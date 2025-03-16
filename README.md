# UKB_ML_Benchmarking

This is work of the Shah Group in which eight distinct survival task implementations, ranging from linear to deep learning (DL) models, within the large-scale prospective cohort study UK Biobank (UKB). We compared discrimination and computational requirements across heterogenous predictor matrices and endpoints. Finally, we assessed how well different architectures scale with sample sizes ranging from n = 5,000 to n = 250,000 individuals. Our results show that discriminative performance across a multitude of metrices is dependent on endpoint frequency and predictor matrix properties, with very robust performance of (penalised) COX Proportional Hazards (COX-PH) models.

# Preprint availability

We published our preprint at https://arxiv.org/abs/2503.08870. Feel free to reach out to us!

# Code order

## Data preprocessing

- Touchscreen preprocessing v2.R
- Benchmarking_Preprocessing_Imputation.ipynb
- Benchmarking_Postprocessing_v1.ipynb

## Model training

- Benchmarking_Model_Fitting_Evaluation_scalingsamplesize.ipynb

## Plotting

- Benchmarking_plotting.ipynb
- Benchmarking_plotting_scaling.ipynb

## Statistical analysis

- Benchmarking_plotting_statistics.ipynb