#!/usr/bin/env bash

# run with both BAM QC and VCF QC metrics
../src/sampleQcTool.R --bamQcMetr bamQcMetr.tsv --vcfQcMetr vcfQcMetr.tsv \
   --annotations sampleAnnotations.tsv --primaryID SampleID --stratify ANCESTRY,SeqTech \
   --refpc refPCs.tsv --samplepc samplePCs.tsv \
   --outputDir ./ --prefix examples

# run with only BAM QC metrics
../src/sampleQcTool.R --bamQcMetr bamQcMetr.tsv \
   --annotations sampleAnnotations.tsv --primaryID SampleID --stratify ANCESTRY,SeqTech \
   --outputDir ./ --prefix bamQc

