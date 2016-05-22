---
title: Live Coding in Python
subtitle: An Eclipse Plug In and an Emacs Mode
---
I've built a tool that lets you run your Python code as you type it. For
example, this code draws a 50x50 pixel square.

![code that draws a 50x50 square][square50]

When I change the forward distance to 75, the square immediately changes. I
don't even have to save the file.

![code that draws a 50x50 square][square75]

In this tutorial, I'll demonstrate two things. Live turtle graphics that make a
fun learning tool, and a live coding display that can be used with regular code
to show you what's happening inside it. To try it yourself, visit
[donkirkby.github.com][livepy]. To see it in action, watch
[my demo video][video], or read on.

![running the turtle code in a window][turtle_window]

Python already comes with a turtle module, so what's the difference? To use the
regular turtle, I need to add a call to `main_loop()`, and then I need to save
and run. Every time I make a change to the code, I need to save and run to see
the result. Of course, I don't do that every time. Instead, I predict the result
by running through the code in my head. One of this project's main goals for
live coding is to let programmers' brains focus on writing code instead of
running code. If you can see the code's results laid out in front of you, you
don't have to hold it all in your head.

Still, I sometimes like to run the regular turtle graphics code to see the
animation of how the turtle moves along its path. The same turtle code will run
in live coding mode as in the regular turtle window.

Another benefit to live coding like this is that I can be creative in a
different way, by reacting to the results of my changes. How about an example?
When I added the feature for filling polygons, I played with triangles, squares,
and pentagons. Then I tried a star, and the middle wasn't filled. After the
surprise wore off, I realized that the centre is actually "outside" the polygon
when you draw a star this way.

![drawing a star][star]

That gave me the idea to see how it would deal
with a spiral, so I made the turtle go around the star five times, and made the
sides longer and longer. That was cool, a striped star! Then I made it go
around 50 times, and it filled the screen.

At this point, I wondered what would happen if I changed the angle, and the
results blew my mind!

![drawing a pinwheel][pinwheel]

I didn't set out to draw a pinwheel pattern and work out how to achieve that, I
just stumbled across it while exploring how filled polygons work. When you
combine live coding's rapid response with an intuitive interface like turtle
graphics, it's easier to learn and create with. I think that was Bret Victor's
point in his Inventing on Principle video that inspired me to build this tool.

---

That was the fun learning tool, now what can you do with real code? I did
create a turtle class that writes to PDF, so that will let you use turtle
graphics in a few more situations, but the main feature is a different view that
helps you visualize what's happening inside your code so you don't have to keep
running it in your head. I'll start with a trivial chunk of code where I assign
a variable, and then modify it.

    s = 'Hello'
    s += ', World!'

That's easy to step through in your head and see that `s` is now
`'Hello, World!'` Remember, though, that I want to let your brain focus on
writing code instead of stepping through it.

I open the live coding display on the right, and it shows me what's in the
variable after each change.

<table><tr><td>

    s = 'Hello'                             
    s += ', World!'

</td><td>

    s = 'Hello'
    s = 'Hello, World!'

</td></tr></table>

Let's do something more interesting and write a library function that does
binary search for a value in a sorted array. The live coding will show us what's
happening in our code so we don't have to hold it all in our heads.


    def search(n, a):                       
        return -1
                                        
It's a bad search function that never finds anything, but let's see how it works
when we call it.

<table><tr><td>

    def search(n, a):                       
        return -1
    
    i = search(2, [1, 2, 4])
    
</td><td>
    
    n = 2 a = [1, 2, 4]                     
    return -1
    
    i = -1 

</td></tr></table>

You can see the input parameters at the start of the function, and the return
value at the end.

We'll start looking for the value in the array, and the first place to look is
the middle item.

<table><tr><td>

    def search(n, a):                       
        low = 0
        high = len(a) - 1
        mid = low + high / 2
        if n == a[mid]:
            return mid
        return -1

    i = search(2, [1, 2, 4])
    
</td><td>

    n = 2 a = [1, 2, 4]                     
    low = 0
    high = 2
    mid = 1
    
    return 1
    
    
    i = 1
    
</td></tr></table>

That was lucky! It was in the first place we looked, and you can see the
calculations as it goes. You see an abstract formula in the code, like
`high = len(a) - 1`, and you see the concrete result in the live coding
display, like `high = 2`. However, a search function usually won't find the
item we're searching for on the first try. Let's ask for an item earlier in the
list and use a while loop to find it.

<table><tr><td>

    def search(n, a):                       
        low = 0
        high = len(a) - 1
        while True:
            mid = low + high / 2
            v = a[mid]
            if n == v:
                return mid
            if n < v:
                high = mid - 1
        return -1
    
    i = search(1, [1, 2, 4])

</td><td>

    n = 1 a = [1, 2, 4]                     
    low = 0
    high = 2
             |
    mid = 1  | mid = 0
    v = 2    | v = 1
             |
             | return 0
             |
    high = 0 |
    
    
    i = 0

</td></tr></table>

The loop runs twice, and each run adds a column to the display showing the
calculations. That's a good example of how this tool differs from a debugger.
With a debugger, you're always looking at a single moment in time. Here, you
can see the whole history of the search laid out on the screen, and you move
back and forth through time just by moving your eye. It's a lot like the
difference that makes static visualizations of sorting algorithms easier to
follow than animated sorting algorithms.

Now let's look for an item later in the list.

<table><tr><td>

    def search(n, a):                       
        low = 0
        high = len(a) - 1
        while True:
            mid = low + high / 2
            v = a[mid]
            if n == v:
                return mid
            if n < v:
                high = mid - 1
            else:
                low = mid + 1
        return -1
    
    i = search(4, [1, 2, 4])
    
</td><td>

    n = 4 a = [1, 2, 4]                     
    low = 0
    high = 2
            |
    mid = 1 | mid = 3
    v = 2   | IndexError: list index out of
            |
            | 
            |
            |
            |
    low = 2 |
    
    
    IndexError: list index out of range
    
</td></tr></table>

Oops, I get an IndexError. Without the live coding display, I would just get a
traceback that shows where the error happened, but not how it happened. Now, I
can walk back from the error to see where things went wrong. `mid` is the index
value, and it's calculated at the top of the loop. The two values that go into
it are both 2, so they should average to 2. Oh, I need parentheses to calculate
the average.

<table><tr><td>

    def search(n, a):                       
        low = 0
        high = len(a) - 1
        while True:
            mid = (low + high) / 2
            v = a[mid]
            if n == v:
                return mid
            if n < v:
                high = mid - 1
            else:
                low = mid + 1
        return -1
    
    i = search(4, [1, 2, 4])
    
</td><td>

    n = 4 a = [1, 2, 4]                     
    low = 0
    high = 2
            |
    mid = 1 | mid = 2
    v = 2   | v = 4
            |
            | return 2
            |
            |
            |
    low = 2 |
    
    
    i = 2
    
</td></tr></table>

What happens if we try to find a value that's not in the list?

<table><tr><td>

    def search(n, a):                       
        low = 0
        high = len(a) - 1
        while True:
            mid = (low + high) / 2
            v = a[mid]
            if n == v:
                return mid
            if n < v:
                high = mid - 1
            else:
                low = mid + 1
        return -1
    
    i = search(3, [1, 2, 4])
    
</td><td>

    n = 3 a = [1, 2, 4]                     
    low = 0
    high = 2
            |          |         |         |
    mid = 1 | mid = 2  | mid = 1 | mid = 1 |
    v = 2   | v = 4    | v = 4   | v = 4   |
            |          |         |         |
            |          |         |         |
            |          |         |         |
            | high = 1 |         |         |
            |          |         |         |
    low = 2 |          | low = 2 | low = 2 |
    
    
    RuntimeError: live coding message limit
    
</td></tr></table>

I guess that while True wasn't such a good idea, we're stuck in an infinite
loop. If you want to see some of the later loop runs, you can scroll over to
the right.

From the third run on, the values in the loop don't change, so we probably want
to exit from the second or third run. If you look at the end of the second run,
you can see that high is lower than low. That means that we've searched all the
way from both ends to meet in the middle, and it's time to give up.

<table><tr><td>

    def search(n, a):                       
        low = 0
        high = len(a) - 1
        while low <= high:
            mid = (low + high) / 2
            v = a[mid]
            if n == v:
                return mid
            if n < v:
                high = mid - 1
            else:
                low = mid + 1
        return -1
    
    i = search(3, [1, 2, 4])
    
</td><td>

    n = 3 a = [1, 2, 4]                     
    low = 0
    high = 2
            |
    mid = 1 | mid = 2
    v = 2   | v = 4
            |
            |
            |
            | high = 1
            |
    low = 2 |
    return -1
    
    i = -1
    
</td></tr></table>

At this point, I think I'm done. I can add a few entries and search for them to
make sure everything is working. Also, if this were a real library module, I
wouldn't want to execute a call at the end of the file, so I only do it when
I'm in live coding mode.

    if __name__ == '__live_coding__':
        i = search(3, [1, 2, 4])

Remember, you can try this tool yourself by visiting
[donkirkby.github.com][livepy]. Help me test it, and report your bugs. I'd also
love to hear about any other projects working on the same kind of tools.

[square50]: http://donkirkby.github.com/live-py-plugin/images/demo_square50.png
[square75]: http://donkirkby.github.com/live-py-plugin/images/demo_square75.png
[livepy]: http://donkirkby.github.com/live-py-plugin/
[video]: http://www.youtube.com/watch?v=LV3aFRHlAEQ
[turtle_window]: http://donkirkby.github.com/live-py-plugin/images/demo_turtle_window.png
[star]: http://donkirkby.github.com/live-py-plugin/images/demo_star.png
[pinwheel]: http://donkirkby.github.com/live-py-plugin/images/demo_pinwheel.png
