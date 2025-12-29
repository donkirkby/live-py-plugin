---
title: "Flag Tutorial: Romania to Colombia"
layout: react
is_react: True
hero_image: ../../images/flags/romania-colombia.png
image: /images/flags/romania-colombia.png
---
Beginning Python programmers can use this tutorial to learn basic concepts in
Python using the visual tools in the turtle module. All the code runs in your
browser, so you don't have to install anything.

The challenge in this tutorial is to learn how this turtle script draws the
Romanian flag on the right, 300 pixels wide and 200 high with colours in
vertical bars. Then change it to draw the Colombian flag shown in the goal
canvas below, with the same colours in a different order and in horizontal bars.
You can experiment on your own, or read the rest of the tutorial to learn the
concepts you need. The progress bar shows how close you are to the goal, and the
canvas differences, below right, highlight the differences in red (too light) or
blue (too dark).

If you really mess up the script, you can click the reset button to go back to
the start. If you want more challenge, try the next tutorial with the flags of
[Japan and Burkina Faso].

## Table of Contents
* [The Challenge]
* [Moving the Turtle]
* [Debugging with Stamps]
* [Filling Shapes]
* [Repeating with Loops]
* [Lifting the Pen]
* [Changing Colour]

[Japan and Burkina Faso]: japan-burkina-faso.md
[The Challenge]: #the-challenge
[Moving the Turtle]: #moving-the-turtle
[Debugging with Stamps]: #debugging-with-stamps
[Filling Shapes]: #filling-shapes
[Repeating with Loops]: #repeating-with-loops
[Lifting the Pen]: #lifting-the-pen
[Changing Colour]: #changing-colour

## The Challenge

    ### Canvas ###
    import turtle as t
    
    t.penup()
    t.left(90)
    t.forward(100)
    t.right(90)
    t.back(150)
    t.write('Romania', font=('Arial', 30))
    
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
    
    t.done()
    ### Goal ###
    import turtle as t
    
    t.penup()
    t.back(150)
    t.left(90)
    t.forward(100)
    t.write('Colombia', font=('Arial', 30))
    t.back(100)
    
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

## Moving the Turtle

If you haven't used the turtle before, imagine that it's a little turtle in the
middle of a piece of paper with a pen in its mouth. You can tell it to walk
forward and backward or turn left and right. When it walks, it drags the pen
along the paper and draws a line. It always starts in the middle of the page,
facing right.

In this example, it walks forward 100 pixels with `forward()`, turns 90° to the
right using `right()`, then walks forward 50 pixels . Use `left()` if you want
to turn left and `backward()` if you want to go backward. The `done()` call at
the end doesn't do anything here in the browser, but it keeps the turtle window
open, if you copy this script and run it in regular Python. Can you change the
script so that your canvas matches the goal canvas below?

    ### Canvas ###
    import turtle as t
    
    t.forward(100)
    t.right(90)
    t.forward(50)
    
    t.done()
    ### Goal ###
    import turtle as t
    
    t.forward(50)
    t.right(45)
    t.forward(100)

## Debugging with Stamps

Two moves plus a turn are pretty simple to follow, but the scripts will get
longer and more complicated. When you're trying to figure out how a script
works, the `stamp()` method can show you where the turtle is at any point. In
this script, the stamp shows you where the turtle starts. Try cutting that line
and then pasting it after each command in the script. Think about where you
expect the stamp to appear before you paste it in. You can also move the current
line up and down with *Alt+Up* and
*Alt+Down* on most systems or
*Option+Up* and *Option+Down* on a Mac. You can find more shortcuts in
the [Ace editor documentation].

Bonus: If you want to change the shape of the cursor to a turtle,
add `t.shape("turtle")` on the second line in this script.

[Ace editor documentation]: https://github.com/ajaxorg/ace/wiki/Default-Keyboard-Shortcuts

    ### Canvas ###
    import turtle as t
    
    t.stamp()
    t.forward(100)
    t.right(30)
    t.forward(84)
    t.right(60)
    t.forward(66)
    t.right(90)
    t.forward(50)
    t.right(120)
    t.forward(33)
    t.right(150)
    t.forward(16)
    
    t.done()

## Filling Shapes

To draw the flags, you need to fill in blocks of colour, calling `begin_fill()`,
moving the turtle around a shape, and then calling `end_fill()`. Turn this
triangle into a square.

    ### Canvas ###
    import turtle as t
    
    t.begin_fill()
    t.forward(100)
    t.right(120)
    t.forward(100)
    t.end_fill()
    
    t.done()
    ### Goal ###
    import turtle as t
    
    t.begin_fill()
    for _ in range(4):
        t.forward(100)
        t.right(90)
    t.end_fill()

If you get stuck with the left side of the square highlighted in red, make sure
you've moved the turtle around all four sides of the square. It makes a small
difference.

## Repeating with Loops

Computers are good at doing the same thing over and over. Drawing a square, for
example, can be done by repeating two commands four times: move forward and turn
90°. One way to repeat commands in Python is to put them in a `for` loop. The
`range()` function produces a range of numbers, and `for i in range(4):` will
repeat the commands below it 4 times, putting a different number in `i` each
time. In this example, we don't care about the number in `i`, we just want to
repeat the movements 4 times. Which lines are inside the loop? Python uses
indentation (a tab or four spaces) to group lines into a loop, so the loop ends
when the indentation comes back to the left.

Try changing this square into a rectangle. As a hint, rectangles are less
repetitive than squares, so your loop will only repeat two times, but it will
have two different forward moves in it.

    ### Canvas ###
    import turtle as t
    
    for i in range(4):
        t.forward(50)
        t.left(90)
    
    t.done()
    ### Goal ###
    import turtle as t
    
    for i in range(2):
        t.forward(100)
        t.left(90)
        t.forward(50)
        t.left(90)

## Lifting the Pen

The turtle doesn't have to draw a line everywhere it moves. Try moving the
`penup()` and `pendown()` calls around in this script to see what changes. Try
adding a `t.stamp()` call.

    ### Canvas ###
    import turtle as t
    
    t.begin_fill()
    for i in range(2):
        t.forward(100)
        t.left(90)
        t.forward(50)
        t.left(90)
    t.end_fill()
    
    t.right(90)
    t.forward(50)
    t.left(90)
    t.penup()
    t.forward(50)
    t.pendown()
    for i in range(4):
        t.forward(50)
        t.right(90)
    
    t.done()

## Changing Colour

The last thing you need to solve this challenge is changing the colour of the
pen with the `color()` command. Can you figure out how to add the gold rectangle
below the blue square?

    ### Canvas ###
    import turtle as t
    
    t.color('blue')
    t.begin_fill()
    for i in range(4):
        t.forward(50)
        t.left(90)
    t.end_fill()
    
    t.done()
    ### Goal ###
    import turtle as t
    
    t.color('blue')
    t.begin_fill()
    for i in range(4):
        t.forward(50)
        t.left(90)
    t.end_fill()

    t.penup()
    t.right(90)
    t.forward(50)
    t.left(90)
    t.back(50)
    t.pendown()
    t.color('gold')
    t.begin_fill()
    for i in range(2):
        t.forward(100)
        t.right(90)
        t.forward(50)
        t.right(90)
    t.end_fill()

Now that you have all the skills you need, can you solve [the challenge] at the
start of the tutorial? Here's a summary of the skills you learned:

| Command              | Explanation                                                  |
| -------------------- | ------------------------------------------------------------ |
| `forward(distance)`  | Move forward by the specified `distance`, in the direction the turtle is headed. |
| `backward(distance)` | Move the turtle backward by `distance`, opposite to the direction the turtle is headed. |
| `right(a) `          | Turn to the right with the specified angle `a`.              |
| `left(a)`            | Turn to the left with the specified angle `a`.               |
| `done()`             | Keep the turtle window open in regular Python.               |
| `stamp()`            | Show where the turtle is.                                    |
| `begin_fill()`       | Start colouring a shape.                                     |
| `end_fill()`         | Stop colouring a shape.                                      |
| `penup()`            | The pen is lifted from the canvas and doesn't draw when moving. |
| `pendown()`          | The pen is pressed against the canvas and draws when moving. |
| `color("c")`         | Choose the colour named `c`.                                 |

[the challenge]: #the-challenge
