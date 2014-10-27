board  = c( 7, 7, 7, 7, 7, 7, 7, 0,
	     	7, 7, 7, 7, 7, 7, 7, 0)			# Initialize board.
par(mfrow=c(2,2))
plot(board, ylim = c(-1, 60), pch = 15)

pepe = T
pilar = T

housep = sample(1:7, 1)
houser = sample(9:15, 1)
#print(c(housep, houser))
shellsp = board[housep]	
shellsr = board[houser]
dropsp = rep(F, 16)
dropsr = rep(F, 16)
board[housep] = 0
board[houser] = 0
print(board)

while (pepe & pilar) {
while (shellsp > 0) {						
while ((housep+1) < 16 & shellsp > 0) {		
	dropsp[(housep+1)] = T						
	housep = housep + 1
	shellsp = shellsp - 1
	}
stopp = housep
board = board + dropsp
dropsp = rep(F, 16)
housep = 0
}
print(board)
#lines(board, col='cyan')

while (shellsr > 0) {
while ((houser + 1) <= 16 & shellsr > 0) {
	if ((houser + 1) == 8) houser = houser + 1 else houser = houser
	dropsr[(houser+1)] = T						
	houser = houser + 1
	shellsr = shellsr - 1
	}	
stopr = houser
board = board + dropsr
dropsr = rep(F, 16)
houser = 0
}
print(board)
#print(sum(board))
#lines(board, col='pink')

if (stopp == 8) {
	if (sum(board[1:7]) > 0) {
	pepe = T
	housep = sample((which(board[1:7] > 0)), 1)
	shellsp = board[housep]
	dropsp = rep(F, 16)
	board[housep] = 0
	} else pepe = F
	} else {
	if (board[stopp] > 1) {
		pepe = T
		housep = stopp
		shellsp = board[stopp]
		dropsp = rep(F, 16)
		board[stopp] = 0
		} else {
			pepe = F
			if ((stopp < 8) & (board[(8-stopp)*2 + stopp] > 0)) {
				board[8] = board[8] + board[(8-stopp)*2 + stopp] + 1
				board[(8-stopp)*2 + stopp] = 0
				board[stopp] = 0 
				}
			}
		}
if (stopr == 16) {
	if (sum(board[9:15]) > 0) {
	pilar = T
	houser = sample((which(board[9:15]>0)), 1) + 8
	shellsr = board[houser]
	dropsr = rep(F, 16)
	board[houser] = 0
	} else pilar = F
	} else {
	if (board[stopr] > 1) {
		pilar = T
		houser = stopr
		shellsr = board[stopr]
		dropsr = rep(F, 16)
		board[stopr] = 0
		} else {
			pilar = F
			if ((stopr > 8) & (board[stopr - (stopr-8)*2] > 0)) { 
				board[16] = board[16] + board[stopr - (stopr-8)*2] + 1
				board[stopr - (stopr-8)*2] = 0
				board[stopr] = 0
				}
			}
		}

print(paste(stopp, shellsp, housep, pepe))
print(paste(stopr, shellsr, houser, pilar))
#print(board)
#print(sum(board) + shellsr + shellsp)
lines(board, col = 'orange')

if (sum(c(pepe, pilar)) == 0) {
pepe = T
pilar = T

housep = sample((which(board[1:7] > 0)), 1)
houser = sample((which(board[9:15] > 0)), 1) + 8
shellsp = board[housep]	
shellsr = board[houser]
dropsp = rep(F, 16)
dropsr = rep(F, 16)
board[housep] = 0
board[houser] = 0
}
}
 
while (pepe | pilar) {

# pilar's turn
if (pilar) {
while (shellsr > 0) {
while ((houser + 1) <= 16 & shellsr > 0) {
	if ((houser + 1) == 8) houser = houser + 1 else houser = houser
	dropsr[(houser+1)] = T						
	houser = houser + 1
	shellsr = shellsr - 1
	}	
stopr = houser
board = board + dropsr
dropsr = rep(F, 16)
houser = 0
}
print(board)
if (stopr == 16) {
	if (sum(board[9:15]) > 0) {
		pilar = T
		houser = sample((which(board[9:15]>0)), 1) + 8
		shellsr = board[houser]
		dropsr = rep(F, 16)
		board[houser] = 0
	} else {
		if (sum(board[1:7]) > 0) {
			pilar = F
			pepe = T
			housep = sample((which(board[1:7] > 0)), 1)
			shellsp = board[housep]
			dropsp = rep(F, 16)
			board[housep] = 0
		} else {
			pilar = F
			pepe = F
		}
	}
} else {
	if (board[stopr] > 1) {
		pilar = T
		houser = stopr
		shellsr = board[stopr]
		dropsr = rep(F, 16)
		board[stopr] = 0
	} else {
		if (stopr < 8) {
			pilar = F
			pepe = T
			housep = sample((which(board[1:7] > 0)), 1)
			shellsp = board[housep]
			dropsp = rep(F, 16)
			board[housep] = 0
		} else {
			if ((sum(board[1:7])- board[stopr - (stopr-8)*2]) > 0) {
				if (board[stopr - (stopr-8)*2] == 0) {
					pilar = F
					pepe = T
					housep = sample((which(board[1:7] > 0)), 1)
					shellsp = board[housep]
					dropsp = rep(F, 16)
					board[housep] = 0
				} else {
					pilar = F
					board[16] = board[16] + board[stopr - (stopr-8)*2] + 1
					board[stopr - (stopr-8)*2] = 0
					board[stopr] = 0
					pepe = T
					housep = sample((which(board[1:7] > 0)), 1)
					shellsp = board[housep]
						dropsp = rep(F, 16)
					board[housep] = 0
				}
			} else {
				pilar = T
				houser = stopr
				shellsr = board[stopr]
				dropsr = rep(F, 16)
				board[stopr] = 0
			}
		}
	}
}
} else {

# pepe's turn. also works.
while (shellsp > 0) {						
while ((housep+1) < 16 & shellsp > 0) {		
	dropsp[(housep+1)] = T						
	housep = housep + 1
	shellsp = shellsp - 1
	}
stopp = housep
board = board + dropsp
dropsp = rep(F, 16)
housep = 0
}
print(board)
if (stopp == 8) {
	if (sum(board[1:7]) > 0) {
		pepe = T
		housep = sample((which(board[1:7] > 0)), 1)
		shellsp = board[housep]
		dropsp = rep(F, 16)
		board[housep] = 0
	} else {
		if (sum(board[9:15]) > 0) {
			pepe = F
			pilar = T
			houser = sample((which(board[9:15]>0)), 1) + 8
			shellsr = board[houser]
			dropsr = rep(F, 16)
			board[houser] = 0
		} else {
			pilar = F
			pepe = F
		}
	}
} else {
	if (board[stopp] > 1) {
		pepe = T
		housep = stopp
		shellsp = board[stopp]
		dropsp = rep(F, 16)
		board[stopp] = 0
	} else {
		if (stopp > 8) {
			pepe = F
			pilar = T
			houser = sample((which(board[9:15]>0)), 1) + 8
			shellsr = board[houser]
			dropsr = rep(F, 16)
			board[houser] = 0
		} else {
			if ((sum(board[9:15])- board[(8-stopp)*2 + stopp]) > 0) { 
				if (board[(8-stopp)*2 + stopp] == 0) {
					pepe = F
					pilar = T
					houser = sample((which(board[9:15]>0)), 1) + 8
					shellsr = board[houser]
					dropsr = rep(F, 16)
					board[houser] = 0
				} else {
					pepe = F
					board[8] = board[8] + board[(8-stopp)*2 + stopp] + 1
					board[(8-stopp)*2 + stopp] = 0
					board[stopp] = 0 
					pilar = T
					houser = sample((which(board[9:15]>0)), 1) + 8
					shellsr = board[houser]
					dropsr = rep(F, 16)
					board[houser] = 0
				}
			} else {
				pepe = T
				housep = sample((which(board[1:7] > 0)), 1)
				shellsp = board[housep]
				dropsp = rep(F, 16)
				board[housep] = 0
			}
		}
	}
}
}
}