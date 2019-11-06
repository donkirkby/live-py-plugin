---
title: Getting Started with Live Coding in Sublime Text
subtitle: Instantly Visualize Your Code
---
Live Coding in Python lets you run your Python code as you type it. For
example, this code prints a greeting to my friend, Alice.

![Hello Alice]

When I change the name to Bob, the display on the right immediately changes. I
don't even have to save the file.

![Hello Bob]

In this tutorial, I'll demonstrate a live coding display that can be used to
show you what's happening inside your code, as well as previews of Matplotlib
plots and Pyglet user interfaces. To try it yourself, follow the Sublime Text
[installation instructions], then type some code, as in the example above.
Finally, from the Live Coding menu, choose Start. You should see the display on
the right. You can also watch [my demo video][video].

## Live Coding Display ##
I'll start with a trivial chunk of code where I assign
a variable, and then modify it.

    s = 'Hello'
    s += ', World!'

That's easy to step through in your head and see that `s` is now
`'Hello, World!'` Remember, though, that I want to let your brain focus on
writing code instead of stepping through it.

From Sublime Text's Live Coding menu, I choose Start, and it opens the live
coding display like the one on the right (below). The display shows me what's in
the variable after each change.

    # Original source code                   | # Displays variables and loops
    s = 'Hello'                              | s = 'Hello' 
    s += ', World!'                          | s = 'Hello, World!' 

Let's do something more interesting and write a library function that does
binary search for a value in a sorted array. The live coding will show us what's
happening in our code so we don't have to hold it all in our heads.

    def search(n, a):                       
        return -1
                                        
It's a bad search function that never finds anything, but let's see how it works
when we call it.

    def search(n, a):                        | n = 2 | a = [1, 2, 4]
        return -1                            | return -1 
                                             | 
    i = search(2, [1, 2, 4])                 | i = -1 

You can see the input parameters at the start of the function, and the return
value at the end.

We'll start looking for the value in the array, and the first place to look is
the middle item.

    def search(n, a):                        | n = 2 | a = [1, 2, 4] 
        low = 0                              | low = 0 
        high = len(a) - 1                    | high = 2 
        mid = low + high // 2                | mid = 1 
        if n == a[mid]:                      | 
            return mid                       | return 1 
        return -1                            | 
                                             | 
    i = search(2, [1, 2, 4])                 | i = 1 

That was lucky! It was in the first place we looked, and you can see the
calculations as it goes. You see an abstract formula in the code, like
`high = len(a) - 1`, and you see the concrete result in the live coding
display, like `high = 2`. However, a search function usually won't find the
item we're searching for on the first try. Let's ask for an item earlier in the
list and use a while loop to find it.

    def search(n, a):                        | n = 1 | a = [1, 2, 4] 
        low = 0                              | low = 0 
        high = len(a) - 1                    | high = 2 
        while True:                          |          | 
            mid = low + high // 2            | mid = 1  | mid = 0 
            v = a[mid]                       | v = 2    | v = 1 
            if n == v:                       |          | 
                return mid                   |          | return 0 
            if n < v:                        |          | 
                high = mid - 1               | high = 0 | 
        return -1                            | 
                                             | 
    i = search(1, [1, 2, 4])                 | i = 0 

The loop runs twice, and each run adds a column to the display showing the
calculations. That's a good example of how this tool differs from a debugger.
With a debugger, you're always looking at a single moment in time. Here, you
can see the whole history of the search laid out on the screen, and you move
back and forth through time just by moving your eye. It's a lot like the
difference that makes static visualizations of sorting algorithms easier to
follow than animated sorting algorithms.

Now let's look for an item later in the list.

    def search(n, a):                        | n = 4 | a = [1, 2, 4] 
        low = 0                              | low = 0 
        high = len(a) - 1                    | high = 2 
        while True:                          |         | 
            mid = low + high // 2            | mid = 1 | mid = 3 
            v = a[mid]                       | v = 2   | IndexError: list index out of range 
            if n == v:                       |         | 
                return mid                   |         | 
            if n < v:                        |         | 
                high = mid - 1               |         | 
            else:                            |         | 
                low = mid + 1                | low = 2 | 
        return -1                            | 
                                             | 
    i = search(4, [1, 2, 4])                 | IndexError: list index out of range 

Oops, I get an IndexError. Without the live coding display, I would just get a
traceback that shows where the error happened, but not how it happened. Now, I
can walk back from the error to see where things went wrong. `mid` is the index
value, and it's calculated at the top of the loop. The two values that go into
it are both 2, so they should average to 2. Oh, I need parentheses to calculate
the average.

    def search(n, a):                        | n = 4 | a = [1, 2, 4] 
        low = 0                              | low = 0 
        high = len(a) - 1                    | high = 2 
        while True:                          |         | 
            mid = (low + high) // 2          | mid = 1 | mid = 2 
            v = a[mid]                       | v = 2   | v = 4 
            if n == v:                       |         | 
                return mid                   |         | return 2 
            if n < v:                        |         | 
                high = mid - 1               |         | 
            else:                            |         | 
                low = mid + 1                | low = 2 | 
        return -1                            | 
                                             | 
    i = search(4, [1, 2, 4])                 | i = 2 

What happens if we try to find a value that's not in the list?

    def search(n, a):                        | n = 3 | a = [1, 2, 4] 
        low = 0                              | low = 0 
        high = len(a) - 1                    | high = 2 
        while True:                          |         |          |         |         | 
            mid = (low + high) // 2          | mid = 1 | mid = 2  | mid = 1 | mid = 1 | 
            v = a[mid]                       | v = 2   | v = 4    | v = 2   | v = 2   | 
            if n == v:                       |         |          |         |         | 
                return mid                   |         |          |         |         | 
            if n < v:                        |         |          |         |         | 
                high = mid - 1               |         | high = 1 |         |         | 
            else:                            |         |          |         |         | 
                low = mid + 1                | low = 2 |          | low = 2 | low = 2 | 
        return -1                            | 
                                             | 
    i = search(3, [1, 2, 4])                 | RuntimeError: live coding message limit exceeded 

I guess that while True wasn't such a good idea, we're stuck in an infinite
loop. If you want to see some of the later loop runs, you can scroll over to
the right.

From the third run on, the values in the loop don't change, so we probably want
to exit from the second or third run. If you look at the end of the second run,
you can see that high is lower than low. That means that we've searched all the
way from both ends to meet in the middle, and it's time to give up.

    def search(n, a):                        | n = 3 | a = [1, 2, 4] 
        low = 0                              | low = 0 
        high = len(a) - 1                    | high = 2 
        while low <= high:                   |         | 
            mid = (low + high) // 2          | mid = 1 | mid = 2 
            v = a[mid]                       | v = 2   | v = 4 
            if n == v:                       |         | 
                return mid                   |         | 
            if n < v:                        |         | 
                high = mid - 1               |         | high = 1 
            else:                            |         | 
                low = mid + 1                | low = 2 | 
        return -1                            | return -1 
                                             | 
    i = search(3, [1, 2, 4])                 | i = -1 

At this point, I think I'm done. I can add a few entries and search for them to
make sure everything is working. Also, if this were a real library module, I
wouldn't want to execute a call at the end of the file, so I only do it when
I'm in live coding mode.

    if __name__ == '__live_coding__':
        i = search(3, [1, 2, 4])


## Matplotlib Preview
The Matplotlib graphing library has a lot of features, and it can be easier to
fiddle with the settings if you have a preview that updates as you change them.
I can see a preview by typing the following code, and then choosing Start Live
Turtle from the Run menu.

    import matplotlib.pyplot as plt
    import numpy as np
    
    x = np.linspace(-np.pi, np.pi)
    c, s = np.cos(x), np.sin(x)
    
    plt.plot(x, c)
    plt.plot(x, s)
    
    plt.show()

If I decide that I'd rather put the tick marks at &pi; and -&pi;, I can add a
call to `plt.xticks()`, and the preview immediately updates.

    import matplotlib.pyplot as plt
    import numpy as np
    
    x = np.linspace(-np.pi, np.pi)
    c, s = np.cos(x), np.sin(x)
    
    plt.plot(x, c)
    plt.plot(x, s)
    plt.xticks(np.linspace(-np.pi, np.pi, 3))
    
    plt.show()

There are lots of Matplotlib tutorials around, and this preview can be useful
for following along with a tutorial.

## Pyglet Preview
Pyglet is a library for building user interfaces, and I can preview the user
interface in the Live Turtle view.

![Pyglet preview]

See the [Pyglet documentation] for details on how to build an interface.

## Learn More
Remember, you can find installation instructions and descriptions of all the
other Live Coding in Python plugins and tools by visiting
[donkirkby.github.com][livepy]. Help me test it, and report your bugs. I'd also
love to hear about any other projects working on the same kind of tools.

[Hello Alice]: https://donkirkby.github.io/live-py-plugin/images/sublime_alice.png
[Hello Bob]: https://donkirkby.github.io/live-py-plugin/images/sublime_bob.png
[installation instructions]: https://donkirkby.github.io/live-py-plugin/#installing-the-sublime-text-plugin
[livepy]: https://donkirkby.github.io/live-py-plugin/
[video]: https://www.youtube.com/watch?v=Vdr2l3yNFH4
[Pyglet preview]: https://donkirkby.github.io/live-py-plugin/images/sublime_pyglet.png
[Pyglet documentation]: https://pyglet.readthedocs.io/en/stable/
