## Run for no longer than 6 hrs (real time)
#PBS -l walltime=06:00:00
## Queue: debug is for quick checks
#PBS -q regular
## ask for 10 nodes with 8 threads each
#PBS -l nodes=10:ppn=8
# the name appearing in the queue log
#PBS -N prosecutorFallacy
## join output and error files
#PBS -j oe
## run in the current directory
cd $PBS_O_WORKDIR
## we'll need the R module and we're ready to go:
module load R/2.14.1
R -f prosecutorsFallacy_modelfits.R
