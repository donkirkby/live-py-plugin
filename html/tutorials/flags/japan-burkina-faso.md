# Flag Tutorial: Romania to Colombia
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
    
    white = '#e0e0e0'
    red = '#b0000f'
    
    t.bgcolor('skyblue')
    t.up()
    t.left(90)
    t.forward(100)
    t.right(90)
    t.back(150)
    
    t.write('Japan', font=('Arial', 30))
    t.color(white)
    t.begin_fill()
    for _ in range(2):
        t.forward(300)
        t.right(90)
        t.forward(200)
        t.right(90)
    t.end_fill()
    
    t.color(red)
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
    
    red = '#EF2B2D'
    green = '#009E49'
    yellow = '#FCD116'
    
    t.bgcolor('skyblue')
    t.up()
    t.back(150)
    t.left(90)
    t.forward(100)
    t.right(90)
    
    t.write('Burkina Faso', font=('Arial', 30))
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
    
    t.mainloop()

## Polygons
Before you learn how to draw a star, try drawing a pentagon. Turn this green
square into a yellow pentagon, by changing the colour, the number of sides, and
the angle. Remember, the yellow in the Burkina Faso flag is `#FCD116`. If you
have trouble calculating the angle, take a guess, then adjust it until your
picture looks like the goal.

If you're wondering about the `_` in `for _ in range(4):`, it's a variable name
just like `i` or `green`, but it's often used in Python to mean a variable that
we're going to ignore. Here, we do the same thing for each side of the square or
pentagon, so we don't need to know which side we're drawing.

    ### Canvas ###
    import turtle as t
    
    green = '#009E49'
    
    t.bgcolor('skyblue')
    
    t.color(green)
    t.begin_fill()
    for _ in range(4):
        t.forward(100)
        t.left(90)
    t.end_fill()
    
    t.mainloop()
    ### Goal ###
    import turtle as t
    
    yellow = '#FCD116'
    
    t.bgcolor('skyblue')
    
    t.color(yellow)
    t.begin_fill()
    for _ in range(5):
        t.forward(100)
        t.left(72)
    t.end_fill()
    
    t.mainloop()

If you had trouble calculating the angle for the pentagon, you can use the
technique in this diagram. Each turn of the turtle is marked by angles 1 to 5.
You can see that the turtle turns a full 360° through five turns, so each turn
is 360÷5 = 72. Don't worry about the details of this code, it's just drawing the
diagram to explain how to calculate the turn angle.

    ### Canvas ###
    import turtle as t
    
    t.shape('turtle')
    for i in range(5):
        t.forward(100)
        t.stamp()
        t.forward(30)
        t.left(90)
        t.circle(30, 72)
        t.circle(30, -72)
        t.write(i+1, font=('Courier', 15))
        t.right(90)
        t.back(30)
        t.left(72)
    
    t.mainloop()


## A Star
What's the difference between a pentagon and a star? Only the angle. Try using
your finger or a pencil to trace all the turn angles for the turtle at each
point of the star, and count how many complete circles it turns.

    ### Canvas ###
    import turtle as t
    
    yellow = '#FCD116'
    
    t.bgcolor('skyblue')
    
    t.color(yellow)
    t.begin_fill()
    for _ in range(5):
        t.forward(100)
        t.left(72)
    t.end_fill()
    
    t.mainloop()
    ### Goal ###
    import turtle as t
    
    yellow = '#FCD116'
    
    t.bgcolor('skyblue')
    
    t.color(yellow)
    t.begin_fill()
    for _ in range(5):
        t.forward(100)
        t.left(144)
    t.end_fill()
    
    t.mainloop()

If it wasn't clear with your finger, look at the marked angles in this diagram.
The turtle starts out facing right, turns through corners 1 and 2, then faces
right again in the middle of corner 3, and turns a complete second circle
through corners 4 and 5. It turns two complete circles in five turns, so each
turn is 360*2÷5 = 144.

    ### Canvas ###
    import turtle as t
    
    t.shape('turtle')
    for i in range(5):
        t.forward(100)
        t.stamp()
        t.forward(30)
        t.left(90)
        t.circle(30, 144)
        t.circle(30, -144)
        t.write(i+1, font=('Courier', 15))
        t.right(90)
        t.back(30)
        t.left(144)
    
    t.mainloop()


## Filling the Centre
Notice that the star's centre isn't filled in. That's because the filling
technique counts how many edges you have to cross to get to a pixel. If the
number is odd, it fills in the pixel. Getting to the centre of the star always
crosses two edges, so it isn't filled in.

To fill in the centre, we can't draw all those lines through the middle, so we
need to draw around the outside of the star. Can you work out what angle you
need for the inside corner? Either calculate it with geometry, or keep adjusting
it until you get a star.

    ### Canvas ###
    import turtle as t
    
    yellow = '#FCD116'
    
    t.bgcolor('skyblue')
    
    t.fillcolor(yellow)
    t.begin_fill()
    for _ in range(5):
        t.forward(50)
        t.left(90)
        t.forward(50)
        t.right(144)
    t.end_fill()
    
    t.mainloop()
    ### Goal ###
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
    
    t.mainloop()

Here's how I calculated the turn angle for the inside corner. The yellow
triangle is one of the points of the star, and we already know that angle A is
144°. The turtle's turn angle at the inside corner is marked as C. You can see
that A and B add up to 180°, so B must be 36°. B, C, and D are the three angles
of the yellow triangle, so they must add up to 180°. Since B is 36°, C+D must be
180° - 36° = 144°. The yellow triangle is an isosceles triangle, so C and D are
the same, and C is 144° ÷ 2 = 72°.

    ### Canvas ###
    import turtle as t
    
    yellow = '#FCD116'
    
    t.color(yellow)
    t.begin_fill()
    t.left(72)
    t.forward(100)
    t.right(144)
    t.forward(100)
    t.end_fill()
    t.goto(0, 0)
    t.left(72)
    
    t.color('black')
    t.back(25)
    t.forward(100)
    t.back(50)
    t.left(90)
    t.circle(25, 72)
    t.circle(25, -72)
    t.right(90)
    t.back(25)
    t.write('C', font=('Courier', 15), align='right')
    t.left(72)
    t.forward(75)
    t.write('B', font=('Courier', 15), align='right')
    t.right(90)
    t.circle(25, 36)
    t.circle(25, -36)
    t.left(90)
    t.forward(55)
    t.right(90)
    t.circle(-30, 90)
    t.write('A', font=('Courier', 15), align='right')
    t.circle(-30, 54)
    t.circle(-30, -144)
    t.left(90)
    t.back(30)
    t.right(144)
    
    t.forward(100)
    t.left(72)
    t.write('D', font=('Courier', 15))
    t.back(25)
    t.left(90)
    t.circle(-25, 72)
    t.circle(-25, -72)
    t.right(90)
    t.forward(50)
    
    t.mainloop()

## Inside a Circle
Next, you need to make your star fit inside a circle of radius 33. Can you
adjust the Japanese flag's circle to be the right size?

    ### Canvas ###
    import turtle as t
    
    white = '#e0e0e0'
    red = '#b0000f'
    
    t.bgcolor('skyblue')
    t.up()
    t.left(90)
    t.forward(100)
    t.right(90)
    t.back(150)
    
    t.color(white)
    t.begin_fill()
    for _ in range(2):
        t.forward(300)
        t.right(90)
        t.forward(200)
        t.right(90)
    t.end_fill()
    
    t.color(red)
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
    
    white = '#e0e0e0'
    red = '#b0000f'
    
    t.bgcolor('skyblue')
    t.up()
    t.left(90)
    t.forward(100)
    t.right(90)
    t.back(150)
    
    t.color(white)
    t.begin_fill()
    for _ in range(2):
        t.forward(300)
        t.right(90)
        t.forward(200)
        t.right(90)
    t.end_fill()
    
    t.color(red)
    t.forward(150)
    t.right(90)
    t.forward(67)
    t.left(90)
    t.begin_fill()
    t.circle(-33)
    t.end_fill()
    
    t.mainloop()

Now, change the angle and size of the star to fit inside the circle. You could
use trigonometry to calculate the size exactly, but it's probably much easier
to adjust it until it looks good.

    ### Canvas ###
    import turtle as t
    
    white = '#e0e0e0'
    red = '#b0000f'
    yellow = '#FCD116'
    
    t.bgcolor('skyblue')
    t.up()
    t.left(90)
    t.forward(100)
    t.right(90)
    t.back(150)
    
    t.color(white)
    t.begin_fill()
    for _ in range(2):
        t.forward(300)
        t.right(90)
        t.forward(200)
        t.right(90)
    t.end_fill()
    
    t.color(red)
    t.forward(150)
    t.right(90)
    t.forward(67)
    t.left(90)
    t.begin_fill()
    t.circle(-33)
    t.end_fill()
    
    t.color(yellow)
    t.begin_fill()
    star_size = 50
    for _ in range(5):
        t.forward(star_size)
        t.left(72)
        t.forward(star_size)
        t.right(144)
    t.end_fill()
    
    t.mainloop()
    ### Goal ###
    import turtle as t
    
    white = '#e0e0e0'
    red = '#b0000f'
    yellow = '#FCD116'
    
    t.bgcolor('skyblue')
    t.up()
    t.left(90)
    t.forward(100)
    t.right(90)
    t.back(150)
    
    t.color(white)
    t.begin_fill()
    for _ in range(2):
        t.forward(300)
        t.right(90)
        t.forward(200)
        t.right(90)
    t.end_fill()
    
    t.color(red)
    t.forward(150)
    t.right(90)
    t.forward(67)
    t.left(90)
    t.begin_fill()
    t.circle(-33)
    t.end_fill()
    
    t.color(yellow)
    t.begin_fill()
    t.right(72)
    star_size = 24
    for _ in range(5):
        t.forward(star_size)
        t.left(72)
        t.forward(star_size)
        t.right(144)
    t.end_fill()
    
    t.mainloop()

Now that you have all the skills you need, can you solve the challenge at the
start of the tutorial?
