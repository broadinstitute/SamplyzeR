#!/usr/bin/env bash

../src/sampleQcTool.R --bamQcMetr bamQcMetr.tsv --vcfQcMetr vcfQcMetr.tsv \
   --annotations sampleAnnotations.tsv --primaryID SampleID --stratify ANCESTRY,SeqTech \
   --refpc refPCs.tsv --samplepc samplePCs.tsv \
   --outputDir ./ --prefix examples
