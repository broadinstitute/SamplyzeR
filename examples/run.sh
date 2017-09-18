#!/usr/bin/env bash

../src/sampleQcTool.R --bamQcMetr bamQcMetr.tsv --vcfQcMetr vcfQcMetr.tsv \
   --attributes sampleAttributes.tsv --primaryID ExternalID --stratify ANCESTRY,SeqTech \
   --outputDir ./ --prefix example
