<h2 align="center">
  SAMPLYZER
</h2>
<p align="center">
  <strong>An R package and web application for efficient sample quality control of human whole genome and whole exome sequencing data</strong>
</p>


## Main Project Features

Samplyzer is an innovative project in the realm of bioinformatics and Next Generation Sequencing (NGS) data analysis. It boasts several key features that contribute to its effectiveness and usability:

- **Comprehensive Quality Control (QC) Exploration:** Samplyzer serves as an R package and Web Application, facilitating the efficient exploration of sample-level QC statistics. This functionality is particularly crucial in bioinformatics analysis, especially when dealing with diverse and numerous samples generated through NGS.
- **Iterative Quality Control at Every Step:** In the realm of NGS data analysis, maintaining quality control at each step is paramount. Samplyzer acknowledges this requirement and ensures that researchers can meticulously inspect and, if necessary, adjust or eliminate any suboptimal data. This iterative process is vital for obtaining reliable and accurate results.
- **Interactive Exploration with Shiny Application:** The project incorporates a Shiny application, providing researchers with a user-friendly interface for interactive exploration of QC metrics. Users can conveniently upload bam QC metrics and vcf QC metrics, gaining insights into data quality through dynamic and interactive plots.
- **Command Line Tool Integration:** Samplyzer offers a command line tool alongside the Shiny application, allowing researchers to seamlessly incorporate it into their workflows. This integration enhances flexibility and accommodates diverse user preferences in data analysis.
- **Specialized Functions for Data Analysis:** The R package comes equipped with specialized functions designed to streamline various aspects of data analysis. These functions include ancestry inference, QC metric stratification, sampling filtering, and robust visualization capabilities. Researchers can leverage these functions to gain a deeper understanding of their data and draw meaningful conclusions.
- **Compatibility with GATK Pipeline:** Samplyzer is specifically tailored to work seamlessly with the Genome Analysis Toolkit (GATK) pipeline. This ensures that researchers utilizing GATK can effortlessly integrate Samplyzer into their analytical processes, enhancing the overall efficiency and effectiveness of their workflows.
- **Dedicated Development Team:** The project is actively developed by a committed team of researchers. This team places a strong emphasis on good documentation and software design, ensuring that Samplyzer remains a reliable and well-maintained tool for the bioinformatics community.

## Requirements

* R >= 3.3.2
* class (>= 7.3-14),
* ggplot2 (>= 2.2.1),
* WriteXLS (>= 4.0.0),
* grid (>= 3.3.2),
* gridExtra (>= 2.2.1)

## Installation

This tool is still under development, and haven't yet released to CRAN. To install from github, you will need to have `devtools` installed.

```r
library(devtools)
install_github("xiaolicbs/samplyzer")
```

## Usage

### 1. Command-Line (CMD)

Running SAMPLYZER from the command line requires specifying some parameters. Here are some common parameters:

**required arguments**

```sh
-b, --bamQcMetr: Path to BAM QC metrics file.
-v, --vcfQcMetr: Path to VCF QC metrics file.
-a, --annotations: Path to sample annotations file.
-d, --primaryID: Name of primary IDs in input TSVs to specify each sample.
--stratify: List of attributes for stratification, multiple attributes separated by commas.
```

**optional arguments**

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

- `bamQcMetr`: Path to BAM QC metrics file.
- `vcfQcMetr`: Path to VCF QC metrics file.
- `annotations`: Path to sample annotations file.
- `primaryID`: Name of primary IDs in input TSVs to specify each sample.
- `stratify`: List of attributes for stratification, multiple attributes separated by commas.
- `outliers`: Sample ID to be flagged as outliers (default: 'Sample-001').
- `cutoffTsv`: Path to a TSV file containing hard cutoffs for QC metrics (default: NULL).
- `samplepc`: A TSV file with genotype PCs for each sample (default: NULL).
- `refpc`: A TSV file with genotype PCs from a reference set of samples (default: NULL).
- `zscore`: Z-score cutoff used to flag outlier samples (default: 4).
- `prefix`: Prefix of output files (default: 'examples').
- `pcX`: PCA analysis QC metrics (default: 'Mean_Coverage').
- `pcY`: PCA analysis QC metrics (default: 'nHets').
- `geom`: QC metrics geom (default: 'scatter').

For detailed RMD examples, please refer to the run script [here](vignettes/tutorial.Rmd).

### 3. Web Interface

SAMPLYZER offers a user-friendly web interface for interactive data exploration: [Sample Explorer Website](http://121.40.162.92:3838/samplyzer/).

Test data files are available in the [`vignettes/data`](vignettes/run.sh).

## License

This software is free and is under the MIT license.