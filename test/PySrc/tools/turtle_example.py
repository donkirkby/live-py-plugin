#echo on
#echo width 60
#echo turtle
def draw(t):
    t.screen.cv.create_line(0, 0, 100, 200)
    t.screen.cv.create_text(0, 100, text='xxx\nyyy', font=('Arial', 8, 'normal'))
    t.write('xxx\nyyy', align='center', font=('Arial', 16, 'normal'))
#    for i in range(20):
#        d = 50 + i*10
#        t.forward(d)
#        t.write('xxx\nyyy', align='center')
#        t.right(144)

if __name__ == '__live_coding__':
    global __live_turtle__
    draw(__live_turtle__)
    
elif __name__ == '__main__':
    from Tkinter import mainloop
    from turtle import Turtle
    
    draw(Turtle())
    mainloop()
