Replication instructions for "Inferential Approaches for Network Analysis: AMEN for Latent Factor Models".
===

`master.sh` - shell script that will run each of the R scripts necessary for replicating the results presented in the paper and the appendix. We recommend navigating to the directory in which this file is located in the terminal and simply running `sh master.sh`. But ... 
* before running this script note that two of the scripts, `runMods/runAmen.R` (see line 21) and `outPerf/runMods_wMiss.R` (see line 6), involve parallel computation. Go into each of those scripts and adjust the number of cores to utilize based on your constraints/blessings. Please note that the output generated during the replication will take up ~24 gb of space.
* In total, it will take ~13 hours to complete the replication if utilizing the default number of cores. 
    - ~1 hour is taken to run each of the models in the runMods/ directory. 
        + This can be sped up if you ran the scripts in parallel by modifying lines 20-24 of `master.sh`.
    - ~12 hours are taken to run the out-of-sample performance analysis, which is a 45-fold cross-validation.
        + We recommend, if you are able, that you throw this up to an EC2 instance or a cluster at your university/labgroup and increase the core allocation in line 6 of `runMods_wMiss.R`.
* Contact [Shahryar Minhas](http://s7minhas.com/) if you run into issues or questions.

Input/Output Storage
---
* data/data/ - contains all input files necessary to conduct replication.
* data/rdaResults/ - contains model results that are generated during the course of the replication via `master.sh`.
* data/summResults/ - contains floats (.tex, .pdf, .png) generated during the course of the replication via `master.sh`.

R scripts
---
* helpers/ - contains scripts for setting up workspace and other helpful functions used in visualizing or organizing results.
* other/ - contains `dvNet.R` which is used to generate Figure 2.
* outPerf/ - contains scripts used to run (`runMods_wMiss.R`) and visualize out-of-sample performance analysis.
* runMods/ - contains scripts to run various models discussed in paper and appendix.
* runResults/ - contains scripts to create table of parameter estimates (`coefCompare.R`), latent factor visualization (`latSpaceViz.R`), and conduct goodness of fit tests based on ability of model to capture network attributes (`netPerf.R` & `netPerfSecondary.R`).