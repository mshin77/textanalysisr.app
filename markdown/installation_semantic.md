## Semantic Analysis Setup

Install Python dependencies for BERTopic and transformer-based semantic analysis.

## Requirements

- Python >= 3.7
- pip or conda package manager

## Installation via Python

```bash
# Using pip
pip install bertopic sentence-transformers umap-learn hdbscan

# Using conda
conda install -c conda-forge bertopic sentence-transformers umap-learn hdbscan
```

## Installation via R

```r
library(reticulate)
py_install(c("bertopic", "sentence-transformers", "umap-learn", "hdbscan"))
```

## Features Enabled

- Neural topic modeling (BERTopic)
- Transformer-based embeddings
- Semantic clustering (UMAP + HDBSCAN)
- Document similarity analysis
- Advanced topic visualization
