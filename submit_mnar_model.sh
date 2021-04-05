#!/bin/bash
#SBATCH --account=ACCOUNT_NAME
#SBATCH -p common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=5G
#SBATCH --mail-type=end
#SBATCH --mail-user=NETID@DUKE.EDU

module load JAGS/4.2.0-rhel8
module load R/4.0.3-rhel8
Rscript mnar-models.R 0 5 0 15 0 15