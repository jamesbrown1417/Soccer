# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Get H2H HTML===============================================================
import asyncio

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        await driver.get('https://www.bet365.com.au/#/AC/B1/C1/D1002/E91422157/G40/')
        await driver.sleep(0.1)
        
        # wait 100s for elem to exist
        elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'src-MarketGroup')]", timeout=100)
        body_html = await elem.get_attribute('outerHTML')
        
        # Write html to file - overwrite existing file
        with open(f"Odds-Scraping/The Hundred/Bet365/HTML/h2h_html.txt", 'w') as f:
            f.write(body_html)
        
        # Get all occurences of src-ParticipantFixtureDetailsHigher_Team, we want to click on each one
        team_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'rcl-ParticipantFixtureDetails_TeamNames')]")
        
        # Print team elements inner text
        for team_element in team_elements:
            print(await team_element.get_attribute('innerText'))
        
        # URL List
        goals_url_list = []
        player_url_list = []
        
        for index in range(len(team_elements)):
            # Get the team elements again as the page has been refreshed
            team_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'rcl-ParticipantFixtureDetails_TeamNames')]")
            
            # Scroll into view, in the middle of screen
            await driver.execute_script("arguments[0].scrollIntoView(true);", team_elements[index])
            await driver.execute_script("window.scrollBy(0, -150)")
            
            # Wait 5 seconds
            await driver.sleep(0.1)
            
            # Click on the current element
            await team_elements[index].click()
            
            # Get Current URL
            cur_url = await driver.current_url

            # Append 'I1/' to URL
            modified_goals_url = cur_url + 'I6/'
            
            # Append 'I2/' to URL
            modified_player_url = cur_url + 'I8/'
            
            goals_url_list.append(modified_goals_url)
            player_url_list.append(modified_player_url)
            
            print(modified_goals_url)
            
            # Go back to the previous page
            await driver.back()
            
        # Write URL as a csv
        goals_url_list = '\n'.join(goals_url_list)
        with open(f"Odds-Scraping/The Hundred/Bet365/goals_urls.csv", 'w') as f:
           f.write(goals_url_list)
           
        player_url_list = '\n'.join(player_url_list)
        with open(f"Odds-Scraping/The Hundred/Bet365/player_urls.csv", 'w') as f:
           f.write(player_url_list)

asyncio.run(main())