---
title: Getting Started with Live Coding
subtitle: Plug ins for PyCharm, Emacs, Eclipse, or even a browser
---
I've built a tool that lets you run your Python code as you type it. For
example, this code draws a 50x50 pixel square.

![code that draws a 50x50 square][square50]

When I change the forward distance to 75, the square immediately changes. I
don't even have to save the file.

![code that draws a 50x50 square][square75]

In this tutorial, I'll demonstrate three things: live turtle graphics that make a
fun learning tool, a live coding display that can be used with regular code
to show you what's happening inside it, and live unit tests. To try it
yourself, visit [donkirkby.github.com][livepy]. To see it in action, watch
[my demo video][video], or read on.

## Live Turtle Graphics ##
![running the turtle code in a window][turtle_window]

Python already comes with a turtle module, so what's the difference? To use the
regular turtle, I need to add a call to `mainloop()`, and then I need to save
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

## Live Coding Display ##
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

    def search(n, a):                        | n = 2 a = [1, 2, 4] 
        return -1                            | return -1 
                                             | 
    i = search(2, [1, 2, 4])                 | i = -1 

You can see the input parameters at the start of the function, and the return
value at the end.

We'll start looking for the value in the array, and the first place to look is
the middle item.

    def search(n, a):                        | n = 2 a = [1, 2, 4] 
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

    def search(n, a):                        | n = 1 a = [1, 2, 4] 
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

    def search(n, a):                        | n = 4 a = [1, 2, 4] 
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

    def search(n, a):                        | n = 4 a = [1, 2, 4] 
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

    def search(n, a):                        | n = 3 a = [1, 2, 4] 
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

    def search(n, a):                        | n = 3 a = [1, 2, 4] 
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

The live coding display is also available for Emacs, thanks to several
Emacs users who contributed most of it.

![Emacs screen shot][emacs]

I have also started working on a PyCharm plugin. Contributions are
welcome if you want to help out.

![PyCharm screen shot][pycharm]

Don't feel like installing anything before you can try it out? Try the
[browser version].


## Live Unit Tests ##
In that example, I kept changing the parameters to search for
different items in the list. Wouldn't each set of search parameters
make a nice unit test? I think unit tests help you [write better
code][tdd], so you can use the live coding display as you add each
unit test and make it pass.

In this section, I'll write a function that counts the number of
unique words in a list. However, words with the same letters are
counted as the same word. For example, the words "apple", "lemon", and
"melon" would only count as two words, because "lemon" and "melon"
have the same letters in different order.

To start, I write a simple unit test that doesn't have any duplicates.

    from unittest import TestCase
    from anagrams import count_anagrams

    class AnagramsTest(TestCase):
        def test_words(self):
            words = ['apple', 'melon']

            n = count_anagrams(words)

            self.assertEqual(2, n)

Of course, that fails when I run it as a unit test, because I haven't
written the `count_anagrams()` method. I start by creating
`anagrams.py` with a stupid version that always returns zero.

    def count_anagrams(words):
        return 0

The test now fails with a reasonable complaint.

    FAIL: test_words (test_anagrams.AnagramsTest)
    ----------------------------------------------------------------------
    Traceback (most recent call last):
      File "/home/don/workspace/scratch/test_anagrams.py", line 12, in test_words
        self.assertEqual(2, n)
    AssertionError: 2 != 0

I want to see what's happening as I make the unit test pass, so I open
`anagrams.py`, click on the drop down menu next to the live coding
button, and choose the `test_anagrams` configuration.

                               | ---------------------- |
                               | unittest: (failures=1) |
                               | ---------------------- |
    def count_anagrams(words): | words = ['apple', 'melon']
        return 0               | return 0

Now I can see the input parameters and the return value, as well as
the fact that the test failed. Next, I make that test pass with the
simplest code that could possibly work.

    def count_anagrams(words): | words = ['apple', 'melon']
        return len(words)      | return 2

Once the test passes, I can add another test method with another
scenario. This one includes two copies of 'melon', so the number of unique
words is still two.

    def test_duplicate_words(self):
        words = ['apple', 'melon', 'melon']

        n = count_anagrams(words)

        self.assertEqual(2, n)

I could make the test pass now, but it's a little confusing when both
tests are being displayed.

                               | ---------------------- | 
                               | unittest: (failures=1) | 
                               | ---------------------- | 
    def count_anagrams(words): | words = ['apple', 'melon', 'melon'] | words = ['apple', 'melon'] 
        return len(words)      | return 3                            | return 2 

Instead, I'll convince Eclipse to just run the new test method. That
becomes even more useful as we add more and more test methods. Open
the test file, and press <kbd>Ctrl</kbd>+<kbd>F9</kbd>. That opens a
dialog that lets you choose which test methods to run. Double click on
`test_duplicate_words`, and it will run. Open the `anagrams.py` file, and
click on the drop down menu next to the live coding button and choose
the `test_anagrams (1)` configuration. Every time you choose a new
test method to run, it will add a configuration to the list. When you
finish a coding session, just remove all the extra configurations. Now
you can see the failing test on its own.

                               | ---------------------- |
                               | unittest: (failures=1) |
                               | ---------------------- |
    def count_anagrams(words): | words = ['apple', 'melon', 'melon']
        return len(words)      | return 3

To remove duplicates, just put all the words into a set before counting.

    def count_anagrams(words): | words = ['apple', 'melon', 'melon'] 
        anagrams = set()       | anagrams = set() 
        for word in words:     | word = 'apple'       | word = 'melon'                | word = 'melon' 
            anagrams.add(word) | anagrams = {'apple'} | anagrams = {'apple', 'melon'} | 
        return len(anagrams)   | return 2 

When you get to the second copy of 'melon', the set doesn't change.

Now we get to the interesting part: detecting anagrams. One way is to
sort the letters in each word.

    def test_anagrams(self):
        words = ['apple', 'melon', 'lemon']

        n = count_anagrams(words)

        self.assertEqual(2, n)


    def count_anagrams(words): | words = ['apple', 'melon', 'lemon']
        anagrams = set()       | anagrams = set()
        for word in words:     | word = 'apple'       | word = 'melon'                | word = 'lemon'
            word = ''.join(    | word = 'aelpp'       | word = 'elmno'                | word = 'elmno'
                sorted(word))  |                      |                               |
            anagrams.add(word) | anagrams = {'aelpp'} | anagrams = {'elmno', 'aelpp'} |
        return len(anagrams)   | return 2

You can see that the second and third iteration of the loop convert
'melon' and 'lemon' to 'elmno', and the set of `anagrams` doesn't
change in the third iteration.

The next feature I want to add is to treat upper case and lower case
the same, so I add a new test case.

    def test_upper(self):
        words = ['Melon', 'Lemon']

        n = count_anagrams(words)

        self.assertEqual(1, n)

I choose the new test with <kbd>Ctrl</kbd>+<kbd>F9</kbd>, then open
`anagrams.py` and choose `test_anagrams.py (2)` from the live coding
drop down menu.

                               | ---------------------- |
                               | unittest: (failures=1) |
                               | ---------------------- |
    def count_anagrams(words): | words = ['Melon', 'Lemon']
        anagrams = set()       | anagrams = set()
        for word in words:     | word = 'Melon'       | word = 'Lemon'
            word = ''.join(    | word = 'Melno'       | word = 'Lemno'
                sorted(word))  |                      |
            anagrams.add(word) | anagrams = {'Melno'} | anagrams = {'Lemno', 'Melno'}
        return len(anagrams)   | return 2

You can see that 'Melon' and 'Lemon' get sorted into 'Melno' and
'Lemno', because upper-case letters sort before lower-case letters. We
can fix that by switching all the words to lower case.

                                | ---------------------- |
                                | unittest: (failures=1) |
                                | ---------------------- |
    def count_anagrams(words):  | words = ['Melon', 'Lemon']
        anagrams = set()        | anagrams = set()
        for word in words:      | word = 'Melon'       | word = 'Lemon'
            word = ''.join(     | word = 'Melno'       | word = 'Lemno'
                sorted(word))   |                      |
            word = word.lower() | word = 'melno'       | word = 'lemno'
            anagrams.add(word)  | anagrams = {'melno'} | anagrams = {'melno', 'lemno'}
        return len(anagrams)    | return 2

Oops, 'Melon' and 'Lemon' now get sorted into 'melno' and 'lemno'. We
fixed the case, but not the sort order. Switching to lower case
*before* sorting the letters will fix it.

    def count_anagrams(words):  | words = ['Melon', 'Lemon']
        anagrams = set()        | anagrams = set()
        for word in words:      | word = 'Melon'       | word = 'Lemon'
            word = word.lower() | word = 'melon'       | word = 'lemon'
            word = ''.join(     | word = 'elmno'       | word = 'elmno'
                sorted(word))   |                      |
            anagrams.add(word)  | anagrams = {'elmno'} |
        return len(anagrams)    | return 1

Finally, I want to handle foreign words correctly. For example, the
German word for street can be written either as 'Straße' or
'Strasse'. Python knows how to convert from one to the other, so I'll
add another test case.

    def test_case_folding(self):
        words = ['Straße', 'Strasse']

        n = count_anagrams(words)

        self.assertEqual(1, n)

If I choose the new test case from the live coding drop down, the
words are counted separately.

                                | ---------------------- |
                                | unittest: (failures=1) |
                                | ---------------------- |
    def count_anagrams(words):  | words = ['Straße', 'Strasse']
        anagrams = set()        | anagrams = set()
        for word in words:      | word = 'Straße'       | word = 'Strasse'
            word = word.lower() | word = 'straße'       | word = 'strasse'
            word = ''.join(     | word = 'aerstß'       | word = 'aerssst'
                sorted(word))   |                       |
            anagrams.add(word)  | anagrams = {'aerstß'} | anagrams = {'aerssst', 'aerstß'}
        return len(anagrams)    | return 2

To fix it, I just switch `lower()` to `casefold()`.

    def count_anagrams(words):     | words = ['Straße', 'Strasse']
        anagrams = set()           | anagrams = set()
        for word in words:         | word = 'Straße'        | word = 'Strasse'
            word = word.casefold() | word = 'strasse'       | word = 'strasse'
            word = ''.join(        | word = 'aerssst'       | word = 'aerssst'
                sorted(word))      |                        |
            anagrams.add(word)     | anagrams = {'aerssst'} |
        return len(anagrams)       | return 1

You can see that `casefold()` converts 'ß' to 'ss', while still
converting 'S' to 's', and the test passes.

Now that I've made each test pass, I run the full test suite again to
make sure I didn't break any of the other tests.

    Finding files... done.
    Importing test modules ... done.

    ----------------------------------------------------------------------
    Ran 4 tests in 0.001s

    OK

It looks good, so I remove the extra run configurations and publish my
new software.

Live unit tests are also available in the Emacs version. In this
example, I chose the unit test with the command

	C-c M-d -m unittest test_anagrams.AnagramsTest.test_case_folding

![Emacs testing screen shot][emacs-test]

Remember, you can try this tool yourself by visiting
[donkirkby.github.com][livepy]. Help me test it, and report your bugs. I'd also
love to hear about any other projects working on the same kind of tools.

[square50]: https://donkirkby.github.io/live-py-plugin/images/demo_square50.png
[square75]: https://donkirkby.github.io/live-py-plugin/images/demo_square75.png
[livepy]: https://donkirkby.github.io/live-py-plugin/
[video]: https://www.youtube.com/watch?v=Vdr2l3yNFH4
[turtle_window]: https://donkirkby.github.io/live-py-plugin/images/demo_turtle_window.png
[star]: https://donkirkby.github.io/live-py-plugin/images/demo_star.png
[pinwheel]: https://donkirkby.github.io/live-py-plugin/images/demo_pinwheel.png
[tdd]: https://donkirkby.github.io/testing/
[emacs]: https://donkirkby.github.io/live-py-plugin/images/emacs.png
[pycharm]: https://donkirkby.github.io/live-py-plugin/images/pycharm.png
[emacs-test]: https://donkirkby.github.io/live-py-plugin/images/emacs-test.png
[browser version]: https://donkirkby.github.io/live-py-plugin/demo/
