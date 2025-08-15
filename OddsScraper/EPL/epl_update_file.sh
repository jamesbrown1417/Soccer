#!/bin/bash

# Give access to normal path variables
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd /Users/jamesbrown/Projects/Soccer || exit

# Remove .json and .txt files in specific directories
rm OddsScraper/EPL/Neds/*.json
rm OddsScraper/EPL/Bet365/HTML/*.txt

# Execute Python and R scripts
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/EPL/Bet365/01-get_bet365_html.py
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/EPL/Bet365/02-get_bet365_player.py

# Execute R scripts for bet365 player data
Rscript OddsScraper/EPL/Bet365/get-bet365-player-assists.R
Rscript OddsScraper/EPL/Bet365/get-bet365-player-shots.R
Rscript OddsScraper/EPL/Bet365/get-bet365-player-shots-on-target.R
Rscript OddsScraper/EPL/Bet365/get-bet365-player-tackles.R

# Execute Neds scraping scripts
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/EPL/Neds/get_neds_urls_EPL.py
Rscript OddsScraper/EPL/Neds/get_neds_match_urls_EPL.R
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/EPL/Neds/get_match_json_EPL.py

# Get TAB JSON
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/EPL/TAB/get-tab-response.py

# Execute R script for getting arbs
Rscript OddsScraper/EPL/master_processing_script_EPL.R

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage"

# Push the commit to the 'main' branch on 'origin'
git push origin main

# Render the report
echo "1" | quarto publish quarto-pub Reports/arbs_EPL.qmd