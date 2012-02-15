#!/bin/bash
## Instructions for farm-style cluster

## Use the current working directory
#$ -cwd
## use bash commands
#$ -S /bin/bash
## combine error and output files
#$ -j y
## Parallel for openmp:
##$ -pe threaded 16
## Launch parallel mpi threads
#$ -pe mpi 100
## Output file name
#$ -o full_prosecutor.out
## Name for queue job
#$ -N full_prosecutor

module load gcc openmpi R Rmpi
R -f prosecutorsFallacy_modelfits.R 


