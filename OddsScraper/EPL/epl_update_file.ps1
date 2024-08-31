# Set the current directory to your project folder
Set-Location -Path "C:\Users\james\OneDrive\Desktop\Projects\Soccer"

# Remove .json and .txt files in specific directories
Remove-Item -Path "C:\Users\james\OneDrive\Desktop\Projects\Soccer\OddsScraper\EPL\Neds\*.json"

# Execute Python and R scripts
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Soccer/OddsScraper/EPL/Bet365/01-get_bet365_html.py"
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Soccer/OddsScraper/EPL/Bet365/02-get_bet365_player.py"
& "Rscript" "OddsScraper\EPL\Bet365\-get-bet365-player-assists.R"
& "Rscript" "OddsScraper\EPL\Bet365\-get-bet365-player-shots.R"
& "Rscript" "OddsScraper\EPL\Bet365\-get-bet365-player-shots-on-target.R"
& "Rscript" "OddsScraper\EPL\Bet365\-get-bet365-player-tackles.R"

& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Soccer/OddsScraper/EPL/Neds/get_neds_urls_EPL.py"
& "Rscript" "OddsScraper\EPL\Neds\get_neds_match_urls_EPL.R"
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Soccer/OddsScraper/EPL/Neds/get_match_json_EPL.py"

# Execute R script for getting arbs
& "Rscript" "OddsScraper\EPL\master_processing_script_EPL.R"

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
$commitMessage = "automated commit and timestamp " + (Get-Date -Format "yyyy-MM-dd HH:mm:ss")
git commit -m $commitMessage

# Push the commit to the 'main' branch on 'origin'
git push origin main