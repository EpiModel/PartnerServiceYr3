#!/bin/bash

# Mandatory Options
#SBATCH --export=ALL
#SBATCH --open-mode=append

# Generated by the `slurmworkflow` R package
#SBATCH --partition=epimodel
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=uonwubi@emory.edu
#SBATCH --cpus-per-task=10
#SBATCH --time=24:00:00
#SBATCH --mem=0

# see https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html
set -e

source "$SWF__STEP_SCRIPT"
