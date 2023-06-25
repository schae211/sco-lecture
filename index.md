## Applied mathematics in Biology: Single-cell omics

*Lecture Notes*

Heidelberg University, summer term 2023

Simon Anders

## Contents

Note: The notebooks linked to in the following do not always present the material
in the order I've presented it in the lecture. Rather, I've rearrange the material
to put related content together.

Note 2: The source code for all the notebooks can be found in [this GitHub repo](https://github.com/simon-anders/sco-lecture).

### Seurat

In the first lecture, we discussed the technology behind single-cell RNA-Seq 
and worked through the [Seurat "Guided clustering tutorial"](https://satijalab.org/seurat/articles/pbmc3k_tutorial.html)
and discussed. I do not have further written lecture notes, unfortunately. However,
there is the video recording.

### 10X raw data

In [this R notebook](read_10x.html), we explore the data produced by the CellRanger
tool and construct the count data.

### Dimension reduction

In [this R notebook](dimred.html), we proceed from the count matrix through 
normalization, PCA and nearest-neighbour search to obtaining a UMAP
representation, all without using Seurat or other single-cell-specific
tools. Details on PCA are, however, postponed to later.

### Clustering and cell type assignment

In [this R notebook](clustering.html), we build the nearest-neighbor graph,
perform Leiden clustering, and attempt to assign cell types to clusters.

### Modularity score ###

This [Python notebook](Modularity.html) explain what quantity the Leiden clustering
algorithm is actually optimizing.

### Python

In this Python notebook, we perform the same steps in Python. Work in progress

### Pseudotime

In this [R notebook](pseudotime.html), we start exploring trajectory inference
and smooth gene expression along trajectories.

### Kernel smoothing and local regression

To understand the "locfit" smoothing in the previous section, we learn [here](smoothing.html)
about smoothing methods.

### Principal component analysis

[Here](pca.html), we learn how PCA really works, and [here](pca2.html), we see its
effect on cell-to-cell distances.

### Pseudotime II: Diffusion distances

[Here](princurve.html), we learn how principal curves are used to get a pseudotime,
and [here](diffdist.html) how diffusion maps are used for this purpose.

