Replication file for "Inferential Approaches for Network Analysis: AMEN for Latent Factor Models
". Code to replicate analysis presented in paper is shown in the following files:

* `master.sh` - this is a shell script that will run each of the R scripts necessary for replicating the results presented in the paper and the appendix. Before running this script go to `helpers/paths.R` and modify to your environment accordingly.
* data/data/ - contains all input files necessary to run this analysis.
* data/rdaResults/ - will store model results that are generated during the course of the replication via `master.sh`.
* data/summResults/ - contains graphics generated during the course of the replication via `master.sh`.