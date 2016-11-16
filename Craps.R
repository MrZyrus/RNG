.throwDice = function() {
  return(sample(1:6, 1) + sample(1:6, 1))
}

.game = function() {
  firstThrow = .throwDice()
  duration = 1
  if (firstThrow == 7 | firstThrow == 11) return(c(2, duration))
  if (firstThrow == 2 | firstThrow == 3 | firstThrow == 12) return(c(0, duration))
  nextThrows = .throwDice()
  duration = 2
  while(nextThrows != 7) {
    if (nextThrows == firstThrow) return(c(2, duration))
    nextThrows = .throwDice()
    duration = duration + 1
  }
  return(c(0, duration))
}

.bets = function(playerType) {
  capital = 1000
  if (playerType == 1) bet = 100
  if (playerType == 2 | playerType == 3) bet = runif(1, 1, capital)
  if (playerType == 4) bet = runif(1, capital/2, capital)
  if (playerType == 5) bet = runif(1, 1, capital/2)
  capital = capital - bet
  game = .game()
  games = 1
  duration = game[2]
  while (game[1] == 2) {
    capital = capital + bet*2
    if (playerType == 3) bet = runif(1, 1, min(capital, 1000))
    if (playerType == 4) bet = min(bet * 2, 1000, capital)
    if (playerType == 5 & game[2] == 1) bet = min(bet * 2, 1000, capital)
    if (playerType == 5 & game[2] > 2) bet = min(bet - 1, 1, capital)
    capital = capital - bet
    game = .game()
    games = games + 1
    duration = duration + game[2]
  }
  return(c(capital, games, duration))
}
  
.night = function() {
  totalTime = 0
  totalPlayers = 0
  for (i in 1:10) {
    time = 0
    while (time < 28800) {
      game = .bets(sample(1:5, 1))
      totalPlayers = totalPlayers + 1
      for (i in 1:game[3]) {
        time = time + runif(1, 15, 45)
      }
    }
    totalTime = max(totalTime, time)
  }
  return(totalPlayers)
}