Space Tracer
============
Trade Time for Space When You Debug
-----------------------------------
The Space Tracer displays what happens to your code so you can read through it
like a book, instead of stepping through it like most debuggers. For example,
here's a simple script that assigns a variable, then loops a few times making
changes to that variable, before printing out the final message.

    message = 'Hello, World'
    for i in range(3):
        message += '!'
    print(message)
   
A regular debugger would let you set break points, look at variable values, and
step through the code. Space Tracer would show you the code with all of the
variable assignments, and the loop iterations in columns. Here, you see the
script on the left, and the variable values and loops on the right, so you can
read through what happened, like reading a book.

    message = 'Hello, World' | message = 'Hello, World' 
    for i in range(3):       | i = 0                     | i = 1                      | i = 2 
        message += '!'       | message = 'Hello, World!' | message = 'Hello, World!!' | message = 'Hello, World!!!' 
    print(message)           | print('Hello, World!!!') 

You can run Space Tracer as a command-line tool, or you can use it in the
[Live Coding in Python] plugins for PyCharm, Emacs, Eclipse, and Sublime Text.

Installing Space Tracer
-----------------------
Use `pip` to install it:

    $ pip install git+https://github.com/donkirkby/live-py-plugin@decorator

Then copy the script above into a file called `hello.py`, and run it with the `space_tracer` command:

    $ space_tracer hello.py
    message = 'Hello, World' | message = 'Hello, World' 
    for i in range(3):       | i = 0                     | i = 1                      | i = 2 
        message += '!'       | message = 'Hello, World!' | message = 'Hello, World!!' | message = 'Hello, World!!!' 
    print(message)           | print('Hello, World!!!') 
    $

[Live Coding in Python]: https://donkirkby.github.io/live-py-plugin/