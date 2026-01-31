# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime
import os
from dotenv import load_dotenv
import asyncio

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Load environment variables: try default .env, then fallback to 'env'
load_dotenv()
# Fallback to a non-dotted 'env' file present in the repo
if os.getenv('BET365USER') is None or os.getenv('BET365PW') is None:
    load_dotenv('/Users/jamesbrown/Projects/Soccer/env')

# Read credentials after loading
username = os.getenv('BET365USER')
password = os.getenv('BET365PW')

# Validate credentials early with a clear error
if not username or not password:
    raise RuntimeError(
        "Missing Bet365 credentials. Set BET365USER and BET365PW in .env or env, or export them in the environment."
    )

# Get H2H HTML and Player Props==============================================
async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        await driver.get('https://www.bet365.com.au/#/AC/B1/C1/D1002/E91422157/G40/')
        await driver.sleep(2)

        # Always perform login each run
        print("Attempting login...")
        login_element = await driver.find_element(By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedOutWide_Login')] | //button[contains(@class, 'hrm-e3') and contains(text(), 'Log In')]", timeout=10)
        await driver.sleep(2)
        await login_element.click()
        await driver.sleep(1)

        username_field = await driver.find_element(By.XPATH, "//input[@placeholder='Username or email address']", timeout=10)
        await username_field.clear()
        await driver.sleep(0.3)
        await username_field.send_keys(username)
        print("Entered username")

        password_field = await driver.find_element(By.XPATH, "//input[@placeholder='Password']", timeout=10)
        await password_field.clear()
        await driver.sleep(0.3)
        await password_field.send_keys(password)
        print("Entered password")

        login_button = await driver.find_element(By.XPATH, "//span[starts-with(@class, 'slm')]", timeout=5)
        await login_button.click()
        print("Clicked login button")

        print("Waiting 2 seconds...")
        await driver.sleep(2)

        await driver.minimize_window()

        # wait 100s for elem to exist
        elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'gl-MarketGroup_Wrapper')]", timeout=100)
        print("Market container found after login")
        body_html = await elem.get_attribute('outerHTML')
        
        # Write html to file - overwrite existing file
        with open(f"OddsScraper/EPL/Bet365/HTML/h2h_html.txt", 'w') as f:
            f.write(body_html)
        
        # Get all occurences of src-ParticipantFixtureDetailsHigher_Team, we want to click on each one
        team_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'rcl-ParticipantFixtureDetails_TeamNames')]")
        
        # Print team elements inner text
        for team_element in team_elements:
            print(await team_element.get_attribute('innerText'))
        
        # URL List
        specials_url_list = []
        player_url_list = []
        
        # If len(team_elements) is greater than 10, just get the first 10
        if len(team_elements) > 10:
            team_elements = team_elements[:10]
        
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

            # Append 'I8/' to URL after removing "I0/"
            modified_player_url = cur_url.replace("I0/", "I8/")
            
            # Append 'I9/' to URL after removing "I0/"
            modified_special_url = cur_url.replace("I0/", "I9/")
            
            specials_url_list.append(modified_special_url)
            player_url_list.append(modified_player_url)
            
            # Go back to the previous page
            await driver.back()
            
        # Write URL as a csv
        specials_url_list_str = '\n'.join(specials_url_list)
        with open(f"OddsScraper/EPL/Bet365/specials_urls.csv", 'w') as f:
           f.write(specials_url_list_str)

        player_url_list_str = '\n'.join(player_url_list)
        with open(f"OddsScraper/EPL/Bet365/player_urls.csv", 'w') as f:
           f.write(player_url_list_str)

        # Scrape Player Props from each URL=======================================
        print("\n" + "="*60)
        print("Starting player props scraping...")
        print("="*60 + "\n")

        for index, url in enumerate(player_url_list, start=1):
            try:
                await driver.get(url)

                # Wait for cm-MarketGroupWithIconsButton_Text to exist
                await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ')]", timeout=15)

                # Print URL
                print(f"Getting URL {url} which is match {index}")

                # If there is a button that says Multi Scorers, click it
                try:
                    multi_scorers_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ') and text()='Multi Scorers']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", multi_scorers_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await multi_scorers_button.click()
                    print('Clicked Multi Scorers')
                    await driver.sleep(1)
                except:
                    print('No Multi Scorers button was found')
                    pass

                # If there is a button that says Tackles, click it
                try:
                    tackles_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ') and text()='Player Tackles']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", tackles_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await tackles_button.click()
                    print('Clicked Tackles')
                    await driver.sleep(1)
                except:
                    print('No Tackles')
                    pass

                # If there is a button that says Player Shots Over/Under, click it
                try:
                    shots_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ') and text()='Player Shots Over/Under']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", shots_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await shots_button.click()
                    print('Clicked Player Shots Over/Under')
                    await driver.sleep(1)
                except:
                    print('No Player Shots Over/Under button')
                    pass

                # If there is a button that says Player Shots On Target Over/Under, click it
                try:
                    shots_on_target_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ') and text()='Player Shots On Target Over/Under']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", shots_on_target_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await shots_on_target_button.click()
                    print('Clicked Player Shots On Target Over/Under')
                    await driver.sleep(1)
                except:
                    print('No Player Shots On Target Over/Under button')
                    pass

                # Get all elements with class 'msl-ShowMore_Link ' that has text 'Show more'
                button_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'msl-ShowMore_Link ') and contains(text(), 'Show more')]")

                print(f"Found {len(button_elements)} 'Show more' buttons")

                # Scroll into view of each button, click it and wait 1 second
                for button_element in button_elements:
                   await driver.execute_script("arguments[0].scrollIntoView(true);", button_element)
                   await driver.execute_script("window.scrollBy(0, -150)")
                   await button_element.click()
                   await driver.sleep(1)

                # Write out html to file------------------------------------------------
                # wait for elem to exist
                elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
                body_html_players = await elem.get_attribute('outerHTML')
                with open(f"OddsScraper/EPL/Bet365/HTML/body_html_players_match_{index}.txt", 'w') as f:
                    f.write(body_html_players)
                print(f"Saved HTML for match {index}")

            except Exception as e:
                print(f"An error occurred with URL {url}: {e}. Moving to the next URL.")
                continue  # Proceed to the next iteration of the loop

asyncio.run(main())
