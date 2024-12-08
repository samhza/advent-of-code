import math

def works(target, current, rem):
    if len(rem) == 0:
        return target == current
    if current > target:
        return False
    a, b = current, rem[0]
    v = (works(target, a + b, rem[1:]) or
         works(target, a * b, rem[1:]))
    if v:
        return v
    if works(target, (a * (pow(10, math.ceil(1 if b == 1 else math.log10(b))))) + b, rem[1:]):
        return 2
    
n = 0
n2 = 0
for line in open("in"):
    splat = line.split(': ')
    target, numbers = int(splat[0]), [int(x) for x in splat[1].split(' ')]
    v = works(target, numbers[0], numbers[1:])
    if v == 1:
        n += target
        n2 += target
    elif v == 2:
        n2 += target

print(n, n2)
