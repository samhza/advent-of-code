import itertools

f = open("in")
x = 0
y = 0
rows = []
ant = dict()
for l in f:
    width = len(l.strip())
    rows.append(list(l.strip()))
    x = 0
    for c in l.strip():
        if c != '.':
            ant.setdefault(c, []).append((x,y))
        x+=1
    y += 1
height = y-1

def valid(x,y):
    return x >= 0 and x < width and y >= 0 and y <= height

part1, part2 = set(), set()
for (a,pos) in ant.items():
    for ((x1, y1), (x2, y2)) in itertools.combinations(pos, 2):
        dx = x1 - x2
        dy = y1 - y2
        if valid(x2 - dx, y2 - dy):
            part1.add((x2 - dx, y2 - dy))
        if valid(x1 + dx, y1 + dy):
            part1.add((x1 + dx, y1 + dy))
        while valid(x1, y1):
            part2.add((x1, y1))
            x1 = x1 + dx
            y1 = y1 + dy
        while valid(x2, y2):
            part2.add((x2, y2))
            x2 = x2 - dx
            y2 = y2 - dy

print("part 1:", len(part1))
print("part 2:", len(part2))
