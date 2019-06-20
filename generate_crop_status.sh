#!/bin/bash
#SBATCH --job-name=slurm_test_crop_status        # Job name
#SBATCH --mail-type=ALL                          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=bparmentier@sesync.org       # Where to send mail
#SBATCH	--array=1,15,18,42                              # job array, four jobs corresponding to four tiles in Alaska
#SBATCH --ntasks=4                               # each array task gets a single CPU core
#SBATCH --time=00:59:45                          # Time limit hrs:min:sec
#SBATCH --output=console_%A_%a.out               # Standard output from console
#SBATCH --error=log_%A_%a.err                    # Error log 
#SBATCH --partition=sesynctest                   # queue name, this is for debugging and testing
  
pwd; hostname; date
 
echo "Running crop status generation on a single CPU core"
 
Rscript --vanilla /nfs/bparmentier-data/Data/projects/agbirds-data/scripts/generate_crop_status_layers_06202019.R