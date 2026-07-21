import turtle as t

red = '#EF2B2D'
green = '#009E49'
yellow = '#FCD116'

t.bgcolor('skyblue')
t.up()
t.back(150)
t.left(90)
t.forward(100)
t.right(90)

t.color(red)
t.begin_fill()
for _ in range(2):
    t.forward(300)
    t.right(90)
    t.forward(100)
    t.right(90)
t.end_fill()

t.right(90)
t.forward(100)
t.left(90)
t.color(green)
t.begin_fill()
for _ in range(2):
    t.forward(300)
    t.right(90)
    t.forward(100)
    t.right(90)
t.end_fill()

t.color(yellow)
t.forward(150)
t.left(90)
t.forward(33)
t.right(90)
t.right(72)
t.begin_fill()
size = 24
for _ in range(5):
    t.forward(size)
    t.left(72)
    t.forward(size)
    t.right(144)
t.end_fill()
