import sys

x = 10

p1_a = x
p1_b = x
p1_c = None

p2_a = x
p2_b = x
p2_c = x
p2_d = None

def a():
    global x, p1_a
    p1_a = x

def b():
    global x, p1_b
    p1_b = x

def c():
    global p1_a,p1_b,p1_c
    p1_c = p1_a * p1_b

def d():
    global x, p1_c
    x = p1_c

def e():
    global x, p2_a
    p2_a = x

def f():
    global x, p2_b
    p2_b = x

def g():
    global x, p2_c
    p2_c = x

def h():
    global p2_a, p2_b, p2_c, p2_d
    p2_d = p2_a * p2_b * p2_c

def i():
    global x, p2_d
    x = p2_d

s = sys.argv[1]

for j in list(s):
    eval(j+'()')

print(x)