import turtle as t

yellow = '#FCD116'
t.bgcolor('skyblue')
t.fillcolor(yellow)
t.begin_fill()
for _ in range(5):
    t.forward(50)
    t.left(72)
    t.forward(50)
    t.right(144)
t.end_fill()
