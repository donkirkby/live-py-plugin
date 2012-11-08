def draw_spiral(t):
    for i in range(20):
        d = 50 + i*10
        t.forward(d)
        t.right(144)

if __name__ == '__live_coding__':
    global __live_turtle__
    draw_spiral(__live_turtle__)
    
elif __name__ == '__main__':
    from Tkinter import mainloop
    from turtle import Turtle
    
    draw_spiral(Turtle())
    mainloop()
