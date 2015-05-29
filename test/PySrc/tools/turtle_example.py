def draw_spiral(t):
    t.fillcolor('blue')
    t.begin_fill()
    for i in range(20):
        d = 50 + i*i*1.5
        t.pencolor(0, 0.05*i, 0)
        t.width(i)
        t.forward(d)
        t.right(144)
    t.end_fill()

if __name__ == '__live_coding__':
    global __live_turtle__
    draw_spiral(__live_turtle__)
    
elif __name__ == '__main__':
    from Tkinter import mainloop
    from turtle import Turtle
    
    draw_spiral(Turtle())
    mainloop()
