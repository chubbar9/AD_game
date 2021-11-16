#read in packages
library(tidyr)

#path to static files, set seed, set number of teams
static_dir_path = 'c:/Users/charl/OneDrive/Desktop/local_repo/AD_Game/static_files/'
save_dir_path = 'c:/Users/charl/OneDrive/Desktop/local_repo/AD_Game/save_files/'
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
  #coach_path = paste0(static_dir_path, 'Empty_Coach_DF.csv')
  #coach_df = read.csv(coach_path)
  #colnames(coach_df)[1] = 'First'
  
  #number of coaches is 15x number of teams
  number_of_coaches = 15 * number_of_teams
  
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

create_team_df = function(static_dir_path, seed, number_of_teams){
  #read in name df
  names_path = paste0(static_dir_path, 'Coach_Names.csv')
  names_df = read.csv(names_path)
  colnames(names_df)[1] = 'First'
  
  #create school location and mascot
  school_location = sample(names_df$Locations, number_of_teams, replace = FALSE)
  school_mascot = sample(names_df$Mascots, number_of_teams, replace = TRUE)
  
  #create budgets for each sport
  football_budget = sample(seq(10,25,1), number_of_teams, replace = TRUE)
  basketball_budget = sample(seq(10,25,1), number_of_teams, replace = TRUE)
  baseball_budget = sample(seq(10,25,1), number_of_teams, replace = TRUE)
  
  #put dataframe together
  team_df = data.frame('Location' = school_location, 'Mascot' = school_mascot,
                       'Football_Budget' = football_budget, 'Basketball_Budget' = basketball_budget,
                       'Baseball_Budget' = baseball_budget)
  
  #total budget
  team_df$Total_Budget = rowSums(team_df[,c(3:5)])
  
  #output dataframe
  return(team_df)
}

hire_initial_coaches = function(team_df, coaches_out_df){
  #loop through sports and then teams and hire coaches
  new_team_temp = list()
  new_hired_coaches = list()
  for(i in 1:length(sports)){
    #subset by sport and sort by best overall
    temp_coach_df = subset(coaches_out_df, coaches_out_df$Sport == sports[i])
    temp_coach_df = temp_coach_df[order(-temp_coach_df$Overall),]
    
    #sort teams by sport budget
    if(sports[i] == 'Basketball'){
      sport_budget = 'Basketball_Budget'
    }else if(sports[i] == 'Football'){
      sport_budget = 'Football_Budget'
    }else{
      sport_budget = 'Baseball_Budget'
    }
    
    #create smaller df with team info and budget
    temp_team_df = team_df[,c('Location', 'Mascot', sport_budget)]
    temp_team_df = team_df[order(-temp_team_df[,sport_budget]),]
    
    #cat coach names for easier wrangling below
    temp_coach_df$Name = paste0(temp_coach_df$First, '_', temp_coach_df$Last)
    
    #loop through teams and hire coaches
    teams = unique(temp_team_df$Location)
    hired_coaches = list()
    new_budget = list()
    for(j in 1:length(teams)){
      #get team budget
      team_budget = subset(temp_team_df, Location == teams[j])[,sport_budget][1]
      
      #create head coach, offensive and defensive coordinator dfs
      hc_df = temp_coach_df[c(1:number_of_teams),]
      off_df = subset(temp_coach_df, temp_coach_df$Tendency %in% c('Offensive', 'Hitting') &
                        !(temp_coach_df$Name %in% hc_df$Name))
      def_df = subset(temp_coach_df, temp_coach_df$Tendency %in% c('Defensive', 'Pitching') &
                        !(temp_coach_df$Name %in% hc_df$Name))
      
      #hire best available head coach
      hired_hc = hc_df[j,]
      hc_salary = hired_hc$Overall[1]
      remaining_salary = team_budget - hc_salary
      
      #update tendency to head coach
      hired_hc$Tendency = 'Head_Coach'
      
      #hire random offensive coordinator
      random_oc_num = sample.int(dim(off_df)[1], 1)
      hired_oc = off_df[random_oc_num,]
      oc_salary = hired_oc$Overall[1]
      remaining_salary = remaining_salary - oc_salary
      
      #hire random defensive coordinator
      random_dc_num = sample.int(dim(def_df)[1], 1)
      hired_dc = def_df[random_dc_num,]
      dc_salary = hired_dc$Overall[1]
      remaining_salary = remaining_salary - dc_salary
      
      #create hired coaches df
      hired_coaches_df = rbind(hired_hc, hired_oc, hired_dc)
      hired_coaches_df$Location = teams[j]
      
      #remove hired coaches from the pool
      temp_coach_df = subset(temp_coach_df, !(temp_coach_df$Name %in% hired_coaches_df$Name))
      
      #create budget add-on df
      budget_add = data.frame('Location' = teams[j], 'budget_add' = abs(remaining_salary))
      
      #output hired coaches and add-ons to budget
      hired_coaches[[j]] = hired_coaches_df
      new_budget[[j]] = budget_add
    }
    #rbind hired coaches df
    hired_coaches_long = do.call('rbind', hired_coaches)
    
    #add and update budget
    new_budget_out = do.call('rbind', new_budget)
    temp_team_df = merge(temp_team_df, new_budget_out, 'Location')
    temp_team_df[,sport_budget] = temp_team_df[,sport_budget] + temp_team_df$budget_add
    temp_team_df = temp_team_df[,-7]
    temp_team_df = temp_team_df[,c('Location', 'Mascot', sport_budget)]
    colnames(temp_team_df)[3] = 'Amount'
    temp_team_df$Budget = sport_budget
    
    #output hired coaches df and team temp
    new_team_temp[[i]] = temp_team_df
    new_hired_coaches[[i]] = hired_coaches_long
  }
  #rbind dataframes together
  coaches_df = do.call('rbind', new_hired_coaches)
  team_df = do.call('rbind', new_team_temp)
  
  #team df long to wide format
  team_df = reshape(team_df, idvar = c('Location','Mascot'), timevar = "Budget", direction = "wide")
  colnames(team_df)[3:5] = c('Football_Budget', 'Basketball_Budget', 'Baseball_Budget') 
  
  #set win expectation for each sport
  #win expectation will be percentile % * (1-num of teams)
  percentile = ecdf(team_df$Football_Budget)
  team_df$Football_Expectation = abs(percentile(team_df$Football_Budget) * (1-number_of_teams))
  #basketball
  percentile = ecdf(team_df$Basketball_Budget)
  team_df$Basketball_Expectation = abs(percentile(team_df$Basketball_Budget) * (1-number_of_teams))
  #baseball
  percentile = ecdf(team_df$Baseball_Budget)
  team_df$Baseball_Expectation = abs(percentile(team_df$Baseball_Budget) * (1-number_of_teams))
  
  #create free agent coaches by removing hired coaches
  coaches_out_df$Name = paste0(coaches_out_df$First, '_', coaches_out_df$Last)
  free_agent_df = subset(coaches_out_df, !(coaches_out_df$Name %in% coaches_df$Name))
  
  #output all of the dataframes
  return(list(coaches_df = coaches_df, team_df = team_df, free_agent_df = free_agent_df))
}
