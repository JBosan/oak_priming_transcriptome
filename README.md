# oak_priming_transcriptome
Associated code and data for oak priming of defence transcriptome analysis.

The aim of the analysis was to characterise chemical-induced (BABA, SA, JA) priming during infection of the most susceptible UK oak species, _Quercus robur_, with oak powdery mildew (_Erysiphe alphitoides_).

#### DSI_colonies
[Data](https://github.com/JBosan/oak_priming_transcriptome/tree/main/DSI_colonies/data), [code](https://github.com/JBosan/oak_priming_transcriptome/blob/main/DSI_colonies/DSI_and_colonies.R), and [graphs](https://github.com/JBosan/oak_priming_transcriptome/tree/main/DSI_colonies/final_graphs) for assessment of disease severity index (DSI) and colony counts.

#### PR1_callose_RGR
Contains following:

* PR1 expression, callose deposition, and relative growth rate (RGR) [data](https://github.com/JBosan/oak_priming_transcriptome/tree/main/PR1_callose_RGR/data).
* Code for analysis of [PR1 expression](https://github.com/JBosan/oak_priming_transcriptome/tree/main/PR1_callose_RGR).
* Code for analysis of [callose deposition](https://github.com/JBosan/oak_priming_transcriptome/blob/main/PR1_callose_RGR/callose_boxplots.R).
* Code for analysis of [relative growth rate (RGR)](https://github.com/JBosan/oak_priming_transcriptome/blob/main/PR1_callose_RGR/RGR_boxplots.R) and [leaf number](https://github.com/JBosan/oak_priming_transcriptome/blob/main/PR1_callose_RGR/leaf%20number%20boxplots.R).
* [Graphs](https://github.com/JBosan/oak_priming_transcriptome/tree/main/PR1_callose_RGR/final_graphs) for PR1 expression, callose deposition, and relative growth rate (RGR).

#### [Data_quality_scatterplots](https://github.com/JBosan/oak_priming_transcriptome/tree/main/Data_quality_scatterplots)
Scatter plots used to assess data quality across all samples before DESeq analysis

#### DESeq analysis
Analysis of differential gene expression using [DESeq 2](https://github.com/thelovelab/DESeq2) for [0](https://github.com/JBosan/oak_priming_transcriptome/blob/main/Deseq2%20analysis%200dpi.Rmd), [1](https://github.com/JBosan/oak_priming_transcriptome/blob/main/Deseq2%20analysis%201dpi.Rmd) and [2](https://github.com/JBosan/oak_priming_transcriptome/blob/main/Deseq2%20analysis%202dpi.Rmd) dpi

#### [FC_volcano_plots.Rmd](https://github.com/JBosan/oak_priming_transcriptome/blob/main/FC_volcano_plots.Rmd)
Code for volcano plots used to determine optimal fold-change threshold for selecting up- and downregulated genes

#### DEGs
[Summary](https://github.com/JBosan/oak_priming_transcriptome/tree/main/DEGs/data) of differentially expressed genes discovered during DESeq analysis with [code](https://github.com/JBosan/oak_priming_transcriptome/blob/main/DEGs/DEGs_bar_graph.R) used to generate [summary graphs](https://github.com/JBosan/oak_priming_transcriptome/tree/main/DEGs/graphs)

#### primed_gene_venns
[Data](https://github.com/JBosan/oak_priming_transcriptome/blob/main/primed_gene_venns/priming_selection_venns.RData) and [code](https://github.com/JBosan/oak_priming_transcriptome/blob/main/primed_gene_venns/priming_selection_venns.Rmd) used to generate venn diagrams comparing genes primed by each elicitor.

#### [trinotate.pptx](https://github.com/JBosan/oak_priming_transcriptome/blob/main/trinotate.pptx)
Overview and explanation of code used for GO annotation of [oak reference genome](https://www.oakgenome.fr/index.html) using [trinotate v4.0.2](https://github.com/Trinotate/Trinotate/wiki)

#### GO_enrichment
[Data](https://github.com/JBosan/oak_priming_transcriptome/tree/main/GO_enrichment), [code](https://github.com/JBosan/oak_priming_transcriptome/blob/main/GO_enrichment/GO_enrichment_analysis.Rmd), and output [graphs](https://github.com/JBosan/oak_priming_transcriptome/tree/main/GO_enrichment/GO_graphs) and [summary tables](https://github.com/JBosan/oak_priming_transcriptome/tree/main/GO_enrichment/GO_summary_tables) for GO enrichment analysis

#### [KEGG_analysis](https://github.com/JBosan/oak_priming_transcriptome/tree/main/KEGG_analysis)
Data, code, and output of KEGG analysis used to integrate transcriptomics with metabolome data from [Sanchez-Lucas et al., 2025](https://onlinelibrary.wiley.com/doi/10.1111/pce.15419).




