import itertools

x = open("in1")
a = []
b = []
for line in x:
	s = line.split('   ')
	a.append(int(s[0]))
	b.append(int(s[1]))
print(sum((abs(x - y) for (x,y) in zip(sorted(a), sorted(b)))))

