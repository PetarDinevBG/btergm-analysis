library(xergm)
library(dplyr)
library(Matrix)
library(data.table)


create_round_edges <- function(data, current_round){
  edge_mat <- matrix(0,nrow=20,ncol=20)
  data <- adjust_missing_players(data)
  for(j in 1:nrow(data)){
    player_id <- data[[j, "player_id"]]
    other_id <- data[[j, "other_id"]]
    edge_mat[player_id, other_id] <- 1
  }
  return(edge_mat)
}

add_endowment <- function(network, data){
  endowment <- unique(data[(data$round==1), ])
  endowment <- endowment[ , c("player_id", "endowment")][order(endowment$player_id), ]
  print(endowment[,2])
  network <- set.vertex.attribute(network, "endowment", endowment[,2])
  return(network)
}

add_wealth <- function(network, data, round){
  wealth_data <- as.data.table(data)
  wealth_data <- wealth_data[ , wealth := shift(final_wealth, fill=0), by=c('treatment', 'group_id', 'player_id')]
  wealth_data <- transform(wealth_data, wealth=sqrt(wealth))
  wealth_data <- adjust_missing_players(wealth_data)
  setDF(wealth_data) # Convert to data frame
  round_wealth <- get_round_wealth(wealth_data, round)
  network <- set.vertex.attribute(network, "wealth", round_wealth)
  return(network)
}

adjust_missing_players <- function(data){
  players_in_game <- data[(data$round==1), ]$player_id
  num_players <- length(players_in_game)
  if (num_players < max(players_in_game)) {
    x <- setdiff(seq(1, max(players_in_game)), players_in_game) 
    data$player_id[data$player_id > x] <- data$player_id[data$player_id > x] - 1
    if("other_id" %in% colnames(data))
    {
      data$other_id[data$other_id > x] <- data$other_id[data$other_id > x] - 1
    }
  }
  return(data)
}

get_round_wealth <- function(data, current_round){
  round_data <- data[(data$round==current_round), ]
  return(round_data[,"wealth"])
  
}

get_xergm_data <- function(data1, data2){
  network <- list()
  for(i in 1:20){
    round_edges <- create_round_edges(data1[(data1$round == i), ])
    network[[i]] <- network(round_edges)
    network[[i]] <- add_endowment(network[[i]], data2)
    network[[i]] <- add_wealth(network[[i]], data2, i)
    odegsqrt <- sqrt(degree(network[[i]], cmode = "outdegree"))
    network[[i]] <- set.vertex.attribute(network[[i]],
                                         "odegsqrt", odegsqrt)
  }
  return(network)
}

interact_data <- read.csv("cleaned_data/interactions.csv", header=T)
wealth_data <- read.csv("cleaned_data/finwealth_from_interactions.csv", header=T)

all_networks <- NULL

for (t in unique(interact_data$treatment)) {
  group_networks <- list()
  for (g in unique(interact_data[(interact_data$treatment==t), ]$group_id)) {
    filtered_data1 <- interact_data[(interact_data$treatment==t) & (interact_data$group_id==g), ]
    filtered_data2 <- wealth_data[(wealth_data$treatment==t) & (wealth_data$group_id==g), ]
    xergm_network <- get_xergm_data(filtered_data1, filtered_data2)
    group_networks[[paste(t, '_', g, sep="")]] <- xergm_network
  }
  all_networks[[paste(t, sep="")]] <- group_networks
}

