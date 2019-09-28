---
title: How it Works
subtitle: Instrumenting Python Code
---
I want to record each change to the program's state, so I take the original
source code and add logging calls around each assignment, loop iteration,
and other interesting features. Each change I make to the code needs to leave
its behaviour the same while recording enough information to build a display.
The recording is done by making calls to a new global variable:
`__live_coding_context__`.

As an example, consider the following code with its display:

    a = 'Hello'        | a = 'Hello' 
    b = a + ', World!' | b = 'Hello, World!' 

That code could be instrumented like this:

    a = __live_coding_context__.assign('a', 'Hello', 1)
    b = __live_coding_context__.assign('b', a + ', World!', 2)

The `assign()` method takes in the calculated value, records the assignment
for its report, and then returns the value for the actual assignment.
The numbers are the line numbers from the original code, and they tell the
context where to report the assignments so they show up on the right lines.

To display loop execution, I add the pipe symbol at the end of every line in the
loop, each time the loop starts. The context has a `start_block()` method to
do this. Here's an example with its display:

    x = 0               | x = 0 
    for n in [1, 3, 5]: | n = 1 | n = 3 | n = 5 
        x = x + n       | x = 1 | x = 4 | x = 9 
 
That could be instrumented like this, including `start_block()` and
instrumentation of the loop variable:

    x = __live_coding_context__.assign('x', 0, 1)
    for n in [1, 3, 5]:
        __live_coding_context__.start_block(2, 3)
        __live_coding_context__.assign('n', n, 2)
        x = __live_coding_context__.assign('x', x + n, 3)

Once I know what changes I want to make to the code, how do I add things to
the user's source code? Python has an amazing module called `ast` for handling
abstract syntax trees. You can parse Python source code from a string into a
syntax tree, and then make changes to it. Finally, you can compile and run it.

Here's some example source code:

    s = 'Hello'
    s += ', World!'

Here are the `ast` objects that it gets parsed into:

    Module(body=[Assign(targets=[Name(id='s', ctx=Store())],
                        value=Str(s='Hello')),
                 AugAssign(target=Name(id='s', ctx=Store()),
                           op=Add(),
                           value=Str(s=', World!'))])

Those objects also have attributes holding the line numbers they came from, so
I can display the assignments next to the matching lines of source code. To make
changes to an abstract syntax tree, you define a class that visits each node
of the tree, and returns the modified node. Here's a simple example, the method
that modifies a for loop:

    def visit_For(self, node):
        new_node = self.generic_visit(node)

        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)
        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        new_body = [self._create_context_call('start_block', args)]
        new_body.extend(self._trace_assignment_list(new_node.target))
        new_body.extend(new_node.body)
        new_node.body = new_body
        return new_node

The `generic_visit()` visits all the child nodes before modifying the for
loop itself. Next, I walk through the child nodes looking for all the line
numbers that are included in the loop, so I can draw the pipe symbols between
each iteration of the loop. Then I insert calls to `start_block()` and
`assign()` before the original statements in the for loop. Finally, I return
the modified for loop.

One of the most challenging parts was to register the instrumented code as a
module so a developer can see calls into the instrumented code from another
module. This is useful for running unit tests, for example. David Beazley wrote
[an exhaustive review][beaz] of Python's module system, and that showed how to
manually override the import process.

Here's a minimal example of how to replace the standard import:

    from ast import parse
    import types
    import sys
    
    source = """\
    def foo():
        print('Hello, World!')
    """
    module_name = 'live_lib'
    
    tree = parse(source)
    # TODO: Instrument tree.
    code = compile(tree, '<dummy file>', 'exec')
    
    mod = types.ModuleType(module_name)
    sys.modules[module_name] = mod
    exec(code, mod.__dict__)
    
    import live_lib
    
    live_lib.foo()

The source string holds the original source code, and I parse that into a
syntax tree. After instrumenting the tree (not shown), I compile it and execute
it using the module's dictionary for its global variables. After that, importing
the `live_lib` module will import the instrumented code, even if there is no
`live_lib.py` file. Any other modules that import `live_lib` will get the
instrumented code.

I also learned enough about Python's module system to replace the regular
turtle with my mock turtle. Now I don't need any special variables or method
calls to visualize turtle graphics. The exact same code can run in live coding
mode or in regular mode. The technique of replacing some framework code with
a new version is called monkey patching; here's the
`MockTurtle.monkey_patch()` method:

    @classmethod
    def monkey_patch(cls, canvas=None):
        turtle_module = sys.modules['turtle']
        turtle_module.Turtle = MockTurtle
        turtle_module.mainloop = lambda: None
        MockTurtle._screen = MockTurtle._Screen(canvas)
        MockTurtle._pen = MockTurtle()

This replaces the regular `turtle.Turtle` class with `MockTurtle`, as well
as registering an instance of MockTurtle that will record all its commands.
The `mainloop()` method usually keeps the turtle window open, but the
replacement does nothing, because there is no window to keep open.

Both of those features work better if you can wait until the user's code
imports a module before replacing it with a traced or monkey patched version.
For example, if I always imported matplotlib and monkey patched it, it would
slow down scripts that don't use matplotlib. To detect when a module is being
imported, I added two new [import hooks] to the meta path.

The main classes are:

* `Tracer` visits the abstract syntax tree for a module, and adds all the
    tracing calls.
* `CodeTracer` is a main control class that coordinates `Tracer` and the import
    hooks. It also loads the main module and prints the results.
* `TracedModuleImporter` is an import hook that decides which module should
    have the tracing calls added when it is imported. It's an importer, which
    means that it is both a finder and a loader.
* `PatchedModuleFinder` is an import hook that decides which modules should be
    monkey patched when they are imported. This delegates the basic loading to
    other entries in the meta path, and then patches the resulting module.
* `PatchedModuleLoader` is the loader for the `PatchedModuleFinder`.
* `FileSwallower` reports calls to `print()`, `stderr.write()` and other output.
* `TracedStringIO` reports `write()` calls on local variables that hold
    `StringIO` objects.
* `Canvas` records calls to drawing methods, usually from the `turtle` module.
* `MockTurtle` replaces the regular `Turtle`, and records all its commands.
* `MockPygletWindow` replaces the `pyglet`'s regular `Window` class, and
    displays its interface on the preview canvas.
* `ReportBuilder` records all of the assignments and loop events, then displays
    them in a text report.
* `DeletionTarget` wraps an object before it has an item or attribute deleted
    from it. If the object's `repr()` changes, then it records an assignment
    event.
* `TracedFinder` and `TreeVisitor` parse the source code for a new module during
    the import process, and check which methods or classes have been selected
    for tracing.

[beaz]: http://www.dabeaz.com/modulepackage/
[import hooks]: https://docs.python.org/3/reference/import.html#the-meta-path
