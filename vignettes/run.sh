#!/usr/bin/env bash

# run with both BAM QC and VCF QC metrics
../exec/sampleQcTool.R --bamQcMetr './data/bamQcMetr.tsv' --vcfQcMetr './data/vcfQcMetr.tsv' \
   --annotations './data/sampleAnnotations.tsv' --primaryID SampleID --stratify 'SeqTech,LibTech' \
   --refpc '../vignettes/data/refPCs.tsv' --samplepc '../vignettes/data/samplePCs.tsv' -o 'Sample-001'
   --prefix examples

# run with only BAM QC metrics
../exec/sampleQcTool.R --bamQcMetr './data/bamQcMetr.tsv' --vcfQcMetr './data/vcfQcMetr.tsv' \
   --annotations './data/sampleAnnotations.tsv' --primaryID SampleID --stratify 'SeqTech,LibTech' \
   --prefix bamQc  -o 'Sample-001'
