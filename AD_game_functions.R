#read in packages
library(tidyr)

#path to static file and set seed
static_dir_path = 'c:/Users/charl/OneDrive/Desktop/AD_Game/static_files/'
save_dir_path = 'c:/Users/charl/OneDrive/Desktop/AD_Game/save_files/'
seed = set.seed(123)
number_of_teams = 10
sports = c('Football', 'Basketball', 'Baseball')

create_coaches_df = function(static_dir_path, seed, number_of_teams){
  #fills out coaches df
  #all calls are in base R, no packages to load
  
  #read in name df
  names_path = paste0(static_dir_path, 'Coach_Names.csv')
  names_df = read.csv(names_path)
  colnames(names_df)[1] = 'First'
  
  #read in empty coach df
  coach_path = paste0(static_dir_path, 'Empty_Coach_DF.csv')
  coach_df = read.csv(coach_path)
  colnames(coach_df)[1] = 'First'
  
  #number of coaches is 3x number of teams
  number_of_coaches = 3 * number_of_teams
  
  #loop through sports and create dfs
  out_coaches = list()
  for(i in 1:length(sports)){
    #get names and stats 
    first_name = sample(names_df$First, number_of_coaches, replace = TRUE)
    last_name = sample(names_df$Last, number_of_coaches, replace = TRUE)
    sport = sports[i]
    
    #sample attributes
    experience = sample(seq(0,30,1), number_of_coaches, replace = TRUE)
    offense = sample(seq(0,10,1), number_of_coaches, replace = TRUE)
    defense = sample(seq(0,10,1), number_of_coaches, replace = TRUE)
    recruit = sample(seq(0,10,1), number_of_coaches, replace = TRUE)
    charisma = sample(seq(0,10,1), number_of_coaches, replace = TRUE)
    
    #put information into coaches df
    coach_df = data.frame('First' = first_name, 'Last' = last_name,
                          'Sport' = sport, 'Experience' = experience,
                          'Offense' = offense, 'Defense' = defense,
                          'Recruit' = recruit, 'Charisma' = charisma)
    
    #calculate overall skill for coach
    coach_df$Overall = rowMeans(coach_df[,c(5:8)])
    
    #get tendency
    coach_df$Tendency = ifelse(coach_df$Offense > coach_df$Defense, 'Offensive',
                               ifelse(coach_df$Offense == coach_df$Defense, 
                                      'Well-Rounded', 'Defensive'))
    #rename for baseball
    coach_df$Tendency = ifelse(sports[i] == 'Baseball' & coach_df$Tendency == 'Offensive',
                               'Hitting', ifelse(sports[i] == 'Baseball' & 
                                                   coach_df$Tendency == 'Defensive',
                                                 'Pitching', coach_df$Tendency))
    #output df
    out_coaches[[i]] = coach_df
  }
  #bind dfs together
  coaches_out_df = do.call('rbind', out_coaches)
  
  #output coaches_out_df
  return(coaches_out_df)
}
