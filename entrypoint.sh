#!/bin/bash

# Start the cron service
cron -f &

# Run R script in the background
/app/starting_script.sh > /var/log/starting_script.log 2>&1 &

# Start the Plumber API
exec Rscript -e "plumber::plumb('/app/plumber_diversity_script.R')$run(host='0.0.0.0', port=8870)"