board  = c(7, 7, 7, 7, 7, 7, 7, 0, 7, 7, 7, 7, 7, 7, 7, 0)
print(board)

house = sample(1:7, 1)
print(house)
shells = board[house]
drops = rep(F, 16)
board[house] = 0

while (shells > 0) {
	
while ((house+1) < 16 & shells > 0) {
	drops[(house+1)]=T
	house = house + 1
	shells = shells - 1
	}
board = board + drops
drops = rep(F, 16)
house = 0
}

print(board)