This is more challenging than the [Romania to Colombia] exercise: turn the
Japanese flag into the flag of Burkina Faso. The tricky part is the star in the
centre. This time, instead of using colour names, we use hexadecimal codes with
two digits for red, two digits for green, and two digits for blue. Here are the
details you need to draw the flag of Burkina Faso:

* Red is `#EF2B2D`
* Green is `#009E49`
* Yellow is `#FCD116`
* The star is inscribed inside a circle of radius 33.

Again, you can experiment on your own, or read the rest of the tutorial to learn
the concepts you need.

[Romania to Colombia]:  ?tutorial=flags/romania-colombia

    ### Canvas ###
    import turtle as t
    
    t.bgcolor('skyblue')
    t.up()
    t.left(90)
    t.forward(100)
    t.right(90)
    t.back(150)
    
    t.color('#e0e0e0')  # White
    t.begin_fill()
    for _ in range(2):
        t.forward(300)
        t.right(90)
        t.forward(200)
        t.right(90)
    t.end_fill()
    
    t.color('#b0000f')  # Red
    t.forward(150)
    t.right(90)
    t.forward(40)
    t.left(90)
    t.begin_fill()
    t.circle(-60)
    t.end_fill()
    
    t.mainloop()
    ### Goal ###
    import turtle as t
    
    t.bgcolor('skyblue')
    t.up()
    t.back(150)
    
    t.color('#EF2B2D')  # Red
    t.begin_fill()
    for _ in range(2):
        t.forward(300)
        t.left(90)
        t.forward(100)
        t.left(90)
    t.end_fill()
    
    t.color('#009E49')  # Green
    t.begin_fill()
    for _ in range(2):
        t.forward(300)
        t.right(90)
        t.forward(100)
        t.right(90)
    t.end_fill()
    
    t.color('#FCD116')
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
    
    t.mainloop()
