fix_team_names <- function(team_name_vector) {
    map_chr(
        team_name_vector,
        ~ case_when(
            str_detect(., "^Arsenal|Arsenl") ~ "Arsenal",
            str_detect(., "^Aston Villa|AVilla") ~ "Aston Villa",
            str_detect(., "^(Bournemouth|Bourne)|(AFC Bournemouth)") ~ "Bournemouth",
            str_detect(., "(^Brentford|Brntfd)|(^Bees)") ~ "Brentford",
            str_detect(., "^Brighton|BrtgHA") ~ "Brighton & Hove Albion",
            str_detect(., "^Burnley") ~ "Burnley",
            str_detect(., "^Chelsea|Chlsea") ~ "Chelsea",
            str_detect(., "^Crystal Palace|CrystP") ~ "Crystal Palace",
            str_detect(., "(^Everton|Evertn)|(^Toffees)") ~ "Everton",
            str_detect(., "^Fulham") ~ "Fulham",
            str_detect(., "^Ipswich|Ipswch") ~ "Ipswich Town",
            str_detect(., "^Leeds|Leeds United") ~ "Leeds United",
            str_detect(., "^Leicester|Leicst") ~ "Leicester City",
            str_detect(., "(^Liverpool|Livrpl)|(^Reds)") ~ "Liverpool",
            str_detect(., "(^Luton Town)|(^Hatters)") ~ "Luton Town",
            str_detect(., "(^Manchester City|MnCity)|(^Man City)") ~ "Manchester City",
            str_detect(., "(^Manchester United)|(^Man United)|(^Man Utd)") ~ "Manchester United",
            str_detect(., "^Newcastle|Newcle") ~ "Newcastle United",
            str_detect(., "^Notting|NForst|Nottm") ~ "Nottingham Forest",
            str_detect(., "^Sheffield United") ~ "Sheffield United",
            str_detect(., "^Southampton|Sthamp") ~ "Southampton",
            str_detect(., "(^Tottenham|Totthm)|(^Spurs)") ~ "Tottenham Hotspur",
            str_detect(., "^West Ham|WstHam") ~ "West Ham United",
            str_detect(., "(^Wolverhampton|Wolves)|(^Wolves)") ~ "Wolverhampton Wanderers",
            TRUE ~ . # Default case to return the original team name
        )
    )
}