<h2 align="center">
SAMPLYZER
</h2>
<p align ="center">
<strong>R package and Web App for efficient sample quality control of human genomic data</strong>
</p>

<p align ="center">
  <a title="R Version"> <img src='https://img.shields.io/badge/R-3.3.2-brightgreen'> </a>
</p>

---

## Requirements

* `R >= 3.3.2`
* `class (>= 7.3-14)`
* `ggplot2 (>= 2.2.1)`
* `WriteXLS (>= 4.0.0)`
* `grid (>= 3.3.2)`
* `gridExtra (>= 2.2.1)`

## Installation

This tool is still under development, and haven't yet released to CRAN. To install from github, you will need to have `devtools` installed.

```r
library(devtools)
install_github("x-lab/samplyzer")
```

## Usage

### 1. Command-Line (CMD)

Running SAMPLYZER from the command line requires specifying some parameters. Here are some common parameters:

**Required arguments**

```sh
-b, --bamQcMetr: Path to BAM QC metrics file.
-v, --vcfQcMetr: Path to VCF QC metrics file.
-a, --annotations: Path to sample annotations file.
-d, --primaryID: Name of primary IDs in input TSVs to specify each sample.
--stratify: List of attributes for stratification, multiple attributes separated by commas.
```

**Optional arguments**

```sh
-o, --outliers: Sample ID to be flagged as outliers (default: 'Sample-001').
--cutoffTsv: Path to a TSV file containing hard cutoffs for QC metrics (default: NULL).
-g, --samplepc: A TSV file with genotype PCs for each sample (default: NULL).
-r, --refpc: A TSV file with genotype PCs from a reference set of samples (default: NULL).
-z, --zscore: Z-score cutoff used to flag outlier samples (default: 4).
-p, --prefix: Prefix of output files (default: 'examples').
-x, --pcX: PCA analysis QC metrics (default: 'Mean_Coverage').
-y, --pcY: PCA analysis QC metrics (default: 'nHets').
--geom: QC metrics geom (default: 'scatter').
```

For detailed CMD examples, please refer to the tutorial [here](vignettes/run.sh).

### 2. R Markdown (RMD)

Using SAMPLYZER in R Markdown can be achieved through parameterization. Here are some common parameters:

* `bamQcMetr`: Path to BAM QC metrics file.
* `vcfQcMetr`: Path to VCF QC metrics file.
* `annotations`: Path to sample annotations file.
* `primaryID`: Name of primary IDs in input TSVs to specify each sample.
* `stratify`: List of attributes for stratification, multiple attributes separated by commas.
* `outliers`: Sample ID to be flagged as outliers (default: 'Sample-001').
* `cutoffTsv`: Path to a TSV file containing hard cutoffs for QC metrics (default: NULL).
* `samplepc`: A TSV file with genotype PCs for each sample (default: NULL).
* `refpc`: A TSV file with genotype PCs from a reference set of samples (default: NULL).
* `zscore`: Z-score cutoff used to flag outlier samples (default: 4).
* `prefix`: Prefix of output files (default: 'examples').
* `pcX`: PCA analysis QC metrics (default: 'Mean_Coverage').
* `pcY`: PCA analysis QC metrics (default: 'nHets').
* `geom`: QC metrics geom (default: 'scatter').

For detailed RMD examples, please refer to the run script [here](vignettes/tutorial.Rmd).

### 3. Web Interface

SAMPLYZER offers a user-friendly web interface for interactive data exploration: [Sample Explorer Website](https://xlab.shinyapps.io/samplyzer/).

![Alt text](https://img1.imgtp.com/2023/11/15/U5l1sV53.png)https://img1.imgtp.com/2023/11/15/U5l1sV53.png)

Test data files are available in the [`vignettes/data`](vignettes/run.sh).

## License

This software is free and is under the MIT license.
