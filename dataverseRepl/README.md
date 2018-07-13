## Replication instructions for the paper

Replicating the figures and tables in the main text will take 5-10 minutes on a standard laptop if the provided MCMC results are used. If the provided results are deleted, the running time for the main text figures and tables is detailed below. We recommend replicating the figures and tables in the appendix with use of an EC2 instance (setup details provided in the next section). 

#### Setup information

All of the analysis reported in the manuscript and the appendix is run on a m4.10xlarge EC2 instance. Once the instance is active, ssh into it and run the following lines to set up the R environment and RStudio server: 

```
sudo adduser minhas
sudo add-apt-repository 'deb [arch=amd64,i386] https://cran.rstudio.com/bin/linux/ubuntu xenial/'
sudo apt-get update
sudo apt-get install r-base
sudo apt-get install gdebi-core
sudo apt-get install libapparmor1
wget https://download2.rstudio.org/rstudio-server-1.1.442-amd64.deb
sudo gdebi rstudio-server-1.1.442-amd64.deb
sudo rstudio-server verify-installation
sudo apt-get install pkg-config
sudo apt-get install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev libnlopt-dev
sudo apt-get install htop
sudo apt-get install tree
```

Session information for the version of R we ran on this instance is shown below (further information on packages used in the analysis is included at the end of the README): 

```
> sessionInfo()
R version 3.4.4 (2018-03-15)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 16.04.3 LTS

Matrix products: default
BLAS: /usr/lib/libblas/libblas.so.3.6.0
LAPACK: /usr/lib/lapack/liblapack.so.3.6.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

loaded via a namespace (and not attached):
[1] compiler_3.4.4
>
```

#### Running the scripts

Scripts that should be run in the following order (before running any of the scripts please modify the path to match your local environment, the path is specified in the first few lines of each of the scripts): 

- 0_replicationSetup.R: Creates data object for analysis and installs necessary packages.
    + This should take about five-ten minutes to run .
- 1_runModels.R: This script will generate Table 3 and Figure 3 in the manuscript. Results are saved in the `floats` directory as `Table3.tex` and `Figure3_bw.pdf`.
    + Original results from the authors are already included in the `results` directory, if the relevant files are deleted, then this script will take approximately one hour to run. 
- 2_outSampAnalysis.R: This script will generate Figure 2 in the manuscript. Results are saved in the `floats` directory as `Figure2a_bw.pdf` and `Figure2b_bw.pdf`.
    + Original results from the authors are already included in the `results` directory, if the relevant files are deleted, then this script will take approximately five hours to run. 

We have also included all the analysis necessary to generate the results in the appendix. Each of the seven appendix related scripts are labeled as `appendix_...`. All the results from the appendix scripts will be stored in the `appendix_results_floats` directory.

#### R package build notes

Last, please note the version of each of the libraries that our project relies on (each library was built using R 3.4.4). 

|                   |                    |                |                 |
|:------------------|:-------------------|:---------------|:----------------|
|network: 1.13.0    |sna: 2.4            |ergm: 3.8.0     |latentnet: 2.7.1 |
|btergm: 1.9.0      |amen: 1.4           |reshape2: 1.4.3 |plyr: 1.8.4      |
|ggplot2: 3.0.0     |latex2exp: 0.4.0    |xtable: 1.8-2   |ROCR: 1.0-7      |
|caTools: 1.17.1    |RColorBrewer: 1.1-2 |png: 0.1-7      |grid: 3.5.0      |
|doParallel: 1.0.11 |foreach: 1.4.4      |                |                 |

If you find any errors or have any further questions, please address them to me via email at minhassh@msu.edu.