These files get packed up and then displayed as tutorials in the browser demo.
Any code block is turned into a live coding editor.

### Extra Features
A plain code block will just be displayed with a live coding display next to it.
However, there are extra features you can include.

* **Goal code** - If you include a `### Goal ###` section in the code block,
    it will be executed and compared to the user's code.

      print('This code is visible.')
      ### Goal ###
      print('This code is invisible and the output is compared.')

* **Static code samples** - If you don't want a code sample to be a live sample,
    you can mark it with the `### Static ###` header.
* **REPL code samples** - If a code sample contains ">>>", it will be treated as
    a static code sample.
* **Live code samples** - If you need to override the REPL detector, mark your
    code sample with the `### Live ###` header.
* **Footnotes** - If you link to `#footnoteX` where `X` is any number, then the
    `<a>` tag will be named `footnoteXref`. Conversely, links to `#footnoteXref`
    will be named `footnoteX`. This lets you link to a footnote and back to the
    reference in the text. You're responsible for keeping the numbers in synch.
    For example, you could put this in the body of the page:
    `[[1]](#footnote1)`. Then you could put its mate at the bottom:
    `[[1]](#footnote1ref)`.

New pages can be converted from reStructured text using the `convert_tutorial.py`
script. Here are the tutorials under construction:

* Control Flow [source][controlflow.md].

[controlflow.md]: cpython/controlflow.md
