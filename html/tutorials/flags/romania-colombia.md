Beginning Python programmers can use this tutorial to learn basic concepts in
Python using the visual tools in the turtle module.

The challenge in this tutorial is to learn how this turtle script draws the
Romanian flag, and then change it to draw the Colombian flag. You can experiment
on your own, or read the rest of the tutorial to learn the concepts you need.

    ### Canvas ###
    import turtle as t
    
    t.up()
    t.left(90)
    t.forward(100)
    t.right(90)
    t.back(150)
    
    t.color('dodgerblue4')
    t.begin_fill()
    for i in range(2):
        t.forward(100)
        t.right(90)
        t.forward(200)
        t.right(90)
    t.end_fill()
    t.forward(100)
    
    t.color('gold')
    t.begin_fill()
    for i in range(2):
        t.forward(100)
        t.right(90)
        t.forward(200)
        t.right(90)
    t.end_fill()
    t.forward(100)
    
    t.color('firebrick3')
    t.begin_fill()
    for i in range(2):
        t.forward(100)
        t.right(90)
        t.forward(200)
        t.right(90)
    t.end_fill()
    
    t.mainloop()
    
    ### Goal ###
    import turtle as t
    
    t.up()
    t.back(150)
    t.left(90)
    
    t.color('gold')
    t.begin_fill()
    for i in range(2):
        t.forward(100)
        t.right(90)
        t.forward(300)
        t.right(90)
    t.end_fill()
    t.back(50)
    
    t.color('dodgerblue4')
    t.begin_fill()
    for i in range(2):
        t.forward(50)
        t.right(90)
        t.forward(300)
        t.right(90)
    t.end_fill()
    t.back(50)
    
    t.color('firebrick3')
    t.begin_fill()
    for i in range(2):
        t.forward(50)
        t.right(90)
        t.forward(300)
        t.right(90)
    t.end_fill()
    
    t.mainloop()
