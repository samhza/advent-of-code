f = open("in6")
rows = []
x = 0
y = 0
row = -1
for line in f:
	rows.append(line.strip())
	row = row+1
	try:
		x = line.index("^")
		y = row
	except ValueError:
		pass
print("------")
def p(x,y):
	return rows[y][x]

# 0 UP 1 RIGHT 2 DOWN 3 LEFT
dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]
def loops(visited, dir, x, y, bx, by):
	while not ((dir,x,y) in visited):
		visited.add((dir,x,y))
		(dx, dy) = dirs[dir]
		nx = x+dx
		ny = y+dy
		if(nx < 0 or ny < 0 or nx >= len(rows[0]) or ny >= len(rows)):
			return False
		if p(nx,ny) == '#' or (nx,ny)==(bx,by):
			dir += 1
			dir = dir % 4
		else:
			x = nx
			y = ny
	return True

visited = set()
loops(visited, 0, x, y, -1, -1)
uniq = set((x,y) for (d,x,y) in visited)
print("part 1:", len(uniq))
n = 0
for (bx,by) in uniq:
	if loops(set(), 0, x, y, bx, by):
		n += 1
print("part 2:", n)
