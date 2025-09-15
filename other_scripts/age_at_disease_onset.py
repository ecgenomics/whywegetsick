#!/usr/bin/python3
# -*- coding: utf-8 -*-

# Libraries
import pandas as pd
import sys
import os


# Function
def onset_age(df, prevalence):
    """
    Compute age at disease onset from prevalence data.

    :param df: manually-generated disease dataframe from Risteys with regular format: sep = ';'
    df columns = ['INITIAL_AGE', 'FINAL_AGE', 'COUNTS']
    Example:
    0;19;907
    20;29;2936
    30;39;3573
    40;49;4006
    50;59;3572
    60;69;2328
    70;79;1081
    80;89;282
    90;99;10
    :param prevalence: percentage of disease prevalence, either 5%, 50% or 95%
    :return: age at disease onset (ado) for the specified prevalence
    """
    total_counts = df.COUNTS.sum()
    df['NORMALISED_COUNTS'] = df.COUNTS / total_counts
    df['CUMULATIVE_COUNTS'] = df.NORMALISED_COUNTS.cumsum()
    df['COUNTS_PERCENTAGE'] = round(df.CUMULATIVE_COUNTS * 100, 2)
    initial_age = 0
    final_age = 0
    cumulative_initial = 0
    cumulative_final = 0
    ado = 0
    for row in df.iterrows():
        index = row[0]
        row_values = row[1]
        if row_values.COUNTS_PERCENTAGE < prevalence:
            initial_age = row_values.FINAL_AGE + 1
            cumulative_initial = row_values.CUMULATIVE_COUNTS
        if row_values.COUNTS_PERCENTAGE > prevalence:
            if index == 0:
                initial_age = 0
                final_age = row_values.FINAL_AGE + 1
                cumulative_initial = 0
                cumulative_final = row_values.CUMULATIVE_COUNTS
                ado = (prevalence / 100 - cumulative_initial) * (
                        (final_age - initial_age) / (cumulative_final - cumulative_initial)) + initial_age
                break
            else:
                final_age = row_values.FINAL_AGE + 1
                cumulative_final = row_values.CUMULATIVE_COUNTS
                ado = (prevalence / 100 - cumulative_initial) * (
                        (final_age - initial_age) / (cumulative_final - cumulative_initial)) + initial_age
                break
    return round(ado, 2)


# Input File
input_file = sys.argv[1]
disease_df = pd.read_csv(input_file, sep=';')
filename = os.path.basename(input_file)
disease_code = filename.split('.')[0]
output_filename = './tmp/' + disease_code + '_ADO.tsv'

# Call Function
ado_5 = onset_age(disease_df, 5)
ado_50 = onset_age(disease_df, 50)
ado_95 = onset_age(disease_df, 95)

# Output File
df_output = pd.DataFrame(columns=['DISEASE_CODE', 'ADO_5', 'ADO_50', 'ADO_95'])
df_output.at[0, 'DISEASE_CODE'] = disease_code
df_output.at[0, 'ADO_5'] = ado_5
df_output.at[0, 'ADO_50'] = ado_50
df_output.at[0, 'ADO_95'] = ado_95
df_output.to_csv(otput_filename, sep='\t', index=False)
