# Usage: Characterise a sample BAM
## Author: Brittany Howell (bh10@sanger.ac.uk)
## Date: 20th June 2019


# Get average depth
samtools depth -r 1:1000-2000 sample.bam