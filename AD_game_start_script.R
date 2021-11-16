#load game functions
source('c:/Users/charl/OneDrive/Desktop/local_repo/AD_Game/AD_game_functions.R')

#create coaches df
coaches_out_df = create_coaches_df(static_dir_path, seed, number_of_teams)

#create team df
team_df = create_team_df(static_dir_path, seed, number_of_teams)

#hire coaches to teams
hire_output = hire_initial_coaches(team_df, coaches_out_df)

#get output dfs
coaches_df = hire_output$coaches_df
team_df = hire_output$team_df
free_agent_df = hire_output$free_agent_df
