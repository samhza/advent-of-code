import itertools

x = open("in1")
a = []
b = dict()
for line in x:
	s = line.split('   ')
	a.append(int(s[0]))
	bb = b.get(int(s[1]), 0)
	b[int(s[1])] = bb+1
print(sum((x*b.get(x, 0) for x in a)))

