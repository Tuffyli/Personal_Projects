# ---------------------------------------------------------------------------- #
# CNJ Graph Creation
# Data Base and Plot
# Last edited by: Tuffy Licciardi Issa
# Date: 18/10/2025
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os
import matplotlib.pyplot as plt
from matplotlib.ticker import FormatStrFormatter



# ---------------------------------------------------------------------------- #
# Database -----
# ---------------------------------------------------------------------------- #

filename = 'C:\\Users\\tuffy\\Documents\\IC\\Python\\JN_31-Ago-2023.csv'

colunas = ['ano', 'sigla', 'cn1']
jus = ['TJ', 'TRF', 'TRT', 'TRE']
tra = ['TRT', 'TRT2']

cnj = pd.read_csv(filename, 
                  usecols = colunas,
                  sep = ';',
                  encoding='latin1',
                  na_values = 'nd',
                  on_bad_lines = "skip")

print(cnj.head())

##1st Stage
# Calculating the labor justice lawsuits percentage in Brazil's judicial system
cnj_filtrado = cnj[cnj['sigla'].isin(jus)]

#Reseting the df index
cnj_filtrado = cnj_filtrado.reset_index(drop= True)
cnj_filtrado = cnj_filtrado.set_index('ano')
print(cnj_filtrado)

#df pivoting
cnj_pivot = cnj_filtrado.pivot_table(index = 'ano',
                                     columns = 'sigla',
                                     values = 'cn1')
print(cnj_pivot)

#Removing NA values
cnj_pivot = cnj_pivot.fillna(0)

cnj_pivot['total_por_ano'] = cnj_pivot.sum(axis = 1) 
print(cnj_pivot)

#Extracting the TRT size per year
cnj_pivot['TRT/total'] = (cnj_pivot['TRT']/cnj_pivot['total_por_ano'])
cnj_pivot['TRT/total'] = cnj_pivot['TRT/total'].round(2) #Rounding values

#DF for plotting
trt = cnj_pivot['TRT/total']
print(trt)

# --------------------------------------------------------------------------- #
##2nd Stage
# Calculating the TRT2 share inside the TRT cases
cnj2_filtrado = cnj[cnj['sigla'].isin(tra)]

#Index ajustment
cnj2_filtrado = cnj2_filtrado.reset_index(drop=True).set_index('ano')
print(cnj2_filtrado)

#df pivoting
cnj2_pivot = cnj2_filtrado.pivot_table(index = 'ano',
                                       columns = 'sigla', 
                                       values = 'cn1')
print(cnj2_pivot)

#Extracting the percentage
cnj2_pivot['TRT2/TRT'] = (cnj2_pivot['TRT2']/cnj2_pivot['TRT'])
trt2 = cnj2_pivot['TRT2/TRT'].round(2)
print(trt2)

# --------------------------------------------------------------------------- #
##ETAPA 3
# Plotting the final graph


# Create the subplots
fig, ax = plt.subplots(2, 1, sharey=True, figsize=(10, 10))
fig.subplots_adjust(hspace=0.3)

# Plot the first graph
ax[0].plot(trt.index, trt, color='blue')
ax[0].set_title('Figura 1.1 \n Proporção de novos casos da Justiça Trabalhista (TRT/Total); 2009-2022')
ax[0].set_ylim(bottom=0.10, top=0.30)
ax[0].set_xticks(trt.index)
ax[0].set_xticklabels(trt.index, rotation=0)
ax[0].set_xlabel('Ano')
ax[0].spines['top'].set_visible(False)
ax[0].spines['right'].set_visible(False)
ax[0].yaxis.set_major_formatter(FormatStrFormatter('%.2f'))

# Plot the second graph
ax[1].plot(trt2.index, trt2, color='red')
ax[1].set_title('Figura 2.2 \n Proporção de novos casos do TRT2 (TRT2/TRT); 2009-2022')
ax[1].set_ylim(bottom=0.10, top=0.30)
ax[1].set_xticks(trt2.index)
ax[1].set_xticklabels(trt2.index, rotation=0)
ax[1].set_xlabel('Ano')
ax[1].spines['top'].set_visible(False)
ax[1].spines['right'].set_visible(False)
ax[1].yaxis.set_major_formatter(FormatStrFormatter('%.2f'))

# Add overall title and footnotes
fig.suptitle('Figura 1', fontsize=22)
plt.figtext(0.2, 0.03, 'Fonte: CNJ(2023). Elaboração do autor.', ha='center', fontsize=8)
plt.figtext(0.314, 0.05, '*Dados dos novos casos do TRE não disponíveis para o cálculo do número total.', ha='center', fontsize=8)

# Save the combined figure
fig.savefig('Figure_combined.png')

# Save the first graph separately
fig_11 = plt.figure(figsize=(10, 5))
ax11 = fig_11.add_subplot(111)
ax11.plot(trt.index, trt, color='blue')
#ax11.set_title('Figura 1.1 \n Proporção de novos casos da Justiça Trabalhista (TRT/Total); 2009-2022')
ax11.set_ylim(bottom=0.10, top=0.30)
ax11.set_xticks(trt.index)
ax11.set_xticklabels(trt.index, rotation=0)
ax11.set_xlabel('Ano')
ax11.spines['top'].set_visible(False)
ax11.spines['right'].set_visible(False)
ax11.yaxis.set_major_formatter(FormatStrFormatter('%.2f'))
fig_11.savefig('Graph_11.png')

# Save the second graph separately
fig_12 = plt.figure(figsize=(10, 5))
ax12 = fig_12.add_subplot(111)
ax12.plot(trt2.index, trt2, color='red')
#ax12.set_title('Figura 1.2 \n Proporção de novos casos do TRT2 (TRT2/TRT); 2009-2022')
ax12.set_ylim(bottom=0.10, top=0.30)
ax12.set_xticks(trt2.index)
ax12.set_xticklabels(trt2.index, rotation=0)
ax12.set_xlabel('Ano')
ax12.spines['top'].set_visible(False)
ax12.spines['right'].set_visible(False)
ax12.yaxis.set_major_formatter(FormatStrFormatter('%.2f'))
fig_12.savefig('Graph_12.png')

# Show the combined plot
plt.show()


# -------------- #
# Combined Graph as a single image
fig, ax = plt.subplots(figsize=(10, 6))


# Ploting TRT/Total as a single traced line
ax.plot(trt.index, trt, color='black', linestyle='--', label='TRT/Total')

# Ploting TRT2/TRT as a continuous traced line
ax.plot(trt2.index, trt2, color='black', linestyle='-', label='TRT2/TRT')


# Final Ajustments
ax.set_ylim(bottom=0.10, top=0.30)
ax.set_xticks(trt.index)
ax.set_xticklabels(trt.index, rotation=0)
ax.set_xlabel('Year')
ax.yaxis.set_major_formatter(FormatStrFormatter('%.2f'))

# Removing extra spines
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

# Legends
ax.legend()

# Saving Image
fig.savefig('Grafico_Unico.png', dpi=600, bbox_inches='tight')

# Graph Show
plt.show()