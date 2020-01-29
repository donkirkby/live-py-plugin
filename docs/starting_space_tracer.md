---
title: Getting Started with Space Tracer
subtitle: Read What Happened Like a Book
---
Space Tracer lets you visualize what happens in your Python code without having
to step through it a line at a time. Imagine that you're working on a large
enterprise code base, and you have to figure out what's going wrong in some
obscure function that you've never looked at before. The documentation is poor,
and you haven't figured out how to run it on your workstation. (Of course, this
has never happened to me, but I hear other people sometimes have this problem.)
Before you start pasting print statements all through the production code, see
what Space Tracer can show you about your code.

In this example, I'm going to pretend that the `urlencode()` function is a part
of my large enterprise code base, and my users have told me it needs to stop
encoding spaces as plus signs. They want to see the standard encoding of `%20`.

Here's the script I have that the users are complaining about:

    from urllib.parse import urlencode
    
    encoded = urlencode({'a ?': 'Yes!', 'b/c': 'No'})
    
    print(encoded)


When I run this script, I see the results of calling `urlencode()`:

    $ python url_client.py 
    a+%3F=Yes%21&b%2Fc=No
    $

The dictionary entries are converted to URL parameters, and all the special
characters are encoded to safe characters. Most of them use a percent sign and
a hexadecimal number, but the space in `'a ?'` is converted to `+`. I've been
asked to switch that to `%20`, but I'm not sure exactly why the `urlencode()`
function is treating spaces specially, and if I can change that.

How can Space Tracer help? I start by installing it with pip.

    pip install space_tracer

If you haven't installed Python packages before, read Brett Cannon's
[quick-and-dirty guide]. Then I use it to run the script. It will show what's
happening inside. Just replace the `python` part of the command line with
`space_tracer`.

    $ space_tracer url_client.py 
    from urllib.parse import urlencode                |
                                                      |
    encoded = urlencode({'a ?': 'Yes!', 'b/c': 'No'}) | encoded = 'a+%3F=Yes%21&b%2Fc=No'
                                                      |
    print(encoded)                                    | print('a+%3F=Yes%21&b%2Fc=No')
    $

It shows the result of calling `urlencode()`, and the print statement. Now
let's dig deeper: what's happening inside `urlencode()`? The `--traced` option
tells Space Tracer which part of the code to trace. It can be a module, a class,
or a function.

    $ space_tracer --traced=urllib.parse.urlencode url_client.py
    def urlencode(query, doseq=False, safe='', encoding=None, errors=None,          | query = {'a ?': 'Yes!', 'b/c': 'No'} | doseq = False | safe
                  quote_via=quote_plus):                                            |
        [...]
        if not doseq:                                                               |
            for k, v in query:                                                      | k = 'a ?' | v = 'Yes!' | k = 'b/c' | v = 'No'
                if isinstance(k, bytes):                                            |                        |
                    k = quote_via(k, safe)                                          |                        |
                else:                                                               |                        |
                    k = quote_via(str(k), safe, encoding, errors)                   | k = 'a+%3F'            | k = 'b%2Fc'
                                                                                    |                        |
                if isinstance(v, bytes):                                            |                        |
                    v = quote_via(v, safe)                                          |                        |
                else:                                                               |                        |
                    v = quote_via(str(v), safe, encoding, errors)                   | v = 'Yes%21'           | v = 'No'
                l.append(k + '=' + v)                                               | l = ['a+%3F=Yes%21']   | l = ['a+%3F=Yes%21', 'b%2Fc=No']
        [...]
        return '&'.join(l)                                                          | return 'a+%3F=Yes%21&b%2Fc=No'
    $

I replaced some of the display above with `[...]` to focus on the important
parts. It loops through the keys and values, and the display on the right adds a
column for each key/value pair. The `quote_via()` function is what actually
encodes the special characters, so I want to see what's happening there. Back up
at the top of the function, I see that `quote_via` defaults to `quote_plus`, so
let's trace that.

    $ space_tracer --traced=urllib.parse.quote_plus url_client.py 
    def quote_plus(string, safe='', encoding=None, errors=None):                 | string = 'a ?' | safe = '' | encoding = None | errors = None |
        """Like quote(), but also replace ' ' with '+', as required for quoting  |                                                              |
        HTML form values. Plus signs in the original string are escaped unless   |                                                              |
        they are included in safe. It also does not have safe default to '/'.    |                                                              |
        """                                                                      |                                                              |
        # Check if ' ' in string, where string may either be a str or bytes.  If |                                                              |
        # there are no spaces, the regular quote will produce the right answer.  |                                                              |
        if ((isinstance(string, str) and ' ' not in string) or                   |                                                              |
            (isinstance(string, bytes) and b' ' not in string)):                 |                                                              |
            return quote(string, safe, encoding, errors)                         |                                                              |
        if isinstance(safe, str):                                                |                                                              |
            space = ' '                                                          | space = ' '                                                  |
        else:                                                                    |                                                              |
            space = b' '                                                         |                                                              |
        string = quote(string, safe + space, encoding, errors)                   | string = 'a %3F'                                             |
        return string.replace(' ', '+')                                          | return 'a+%3F'           
    $

There's some conversion between strings and bytes, but then it looks like the
code I was looking for, converting space to `+`. Just to be sure, let's see
what the `quote()` function is doing.

    $ space_tracer --traced=urllib.parse.quote url_client.py 
    def quote(string, safe='/', encoding=None, errors=None):                     | string = 'a ?' | safe = ' ' | encoding = None | errors = None 
        [...]
        if isinstance(string, str):                                              |                                                               
            if not string:                                                       |                                                               
                return string                                                    |                                                               
            if encoding is None:                                                 |                                                               
                encoding = 'utf-8'                                               | encoding = 'utf-8'                                            
            if errors is None:                                                   |                                                               
                errors = 'strict'                                                | errors = 'strict'                                             
            string = string.encode(encoding, errors)                             | string = b'a ?'                                               
        else:                                                                    |                                                               
            if encoding is not None:                                             |                                                               
                raise TypeError("quote() doesn't support 'encoding' for bytes")  |                                                               
            if errors is not None:                                               |                                                               
                raise TypeError("quote() doesn't support 'errors' for bytes")    |                                                               
        return quote_from_bytes(string, safe)                                    | return 'a %3F'                                                
    $

It's converting to bytes, and then calling `quote_from_bytes()`. That's where I
go next.

    $ space_tracer --traced=urllib.parse.quote_from_bytes url_client.py 
    def quote_from_bytes(bs, safe='/'):                                            | bs = b'a ?' | safe = ' ' | bs = b'Yes!' | safe = '' | bs = b
        """Like quote(), but accepts a bytes object rather than a str, and does    |                          |                          |       
        not perform string-to-bytes encoding.  It always returns an ASCII string.  |                          |                          |       
        quote_from_bytes(b'abc def\x3f') -> 'abc%20def%3f'                         |                          |                          |       
        """                                                                        |                          |                          |       
        if not isinstance(bs, (bytes, bytearray)):                                 |                          |                          |       
            raise TypeError("quote_from_bytes() expected bytes")                   |                          |                          |       
        if not bs:                                                                 |                          |                          |       
            return ''                                                              |                          |                          |       
        if isinstance(safe, str):                                                  |                          |                          |       
            # Normalize 'safe' by converting to bytes and removing non-ASCII chars |                          |                          |       
            safe = safe.encode('ascii', 'ignore')                                  | safe = b' '              | safe = b''               | safe =
        else:                                                                      |                          |                          |       
            safe = bytes([c for c in safe if c < 128])                             |                          |                          |       
        if not bs.rstrip(_ALWAYS_SAFE_BYTES + safe):                               |                          |                          |       
            return bs.decode()                                                     |                          |                          |       
        try:                                                                       |                          |                          |       
            quoter = _safe_quoters[safe]                                           | KeyError: b' '           | KeyError: b''            |       
        except KeyError:                                                           |                          |                          |       
            _safe_quoters[safe] = quoter = Quoter(safe).__getitem__                |                          |                          |       
        return ''.join([quoter(char) for char in bs])                              | return 'a %3F'           | return 'Yes%21'          | return
    $

I can see the function being called the first couple of times: for the key and
then the value. The try/except block at the end is a little odd, but it looks
like the encoding is being done by the `Quoter` class.

    $ space_tracer --traced=urllib.parse.Quoter url_client.py 
    class Quoter(collections.defaultdict):                                       |
        """A mapping from bytes (in range(0,256)) to strings.                    |
                                                                                 |
        String values are percent-encoded byte values, unless the key < 128, and |
        in the "safe" set (either the specified safe set, or default set).       |
        """                                                                      |
        # Keeps a cache internally, using defaultdict, for efficiency (lookups   |
        # of cached keys don't call Python code at all).                         |
        def __init__(self, safe):                                                | safe = b' '                                                   
            """safe: bytes object."""                                            |                                                               
            self.safe = _ALWAYS_SAFE.union(safe)                                 | self.safe = frozenset({32, 45, 46, 48, 49, 50, [233 chars]117,
                                                                                 |
        def __repr__(self):                                                      |
            # Without this, will just display as a defaultdict                   |
            return "<%s %r>" % (self.__class__.__name__, dict(self))             |
                                                                                 |
        def __missing__(self, b):                                                | b = 97         | b = 32         | b = 63           | b = 89   
            # Handle a cache miss. Store quoted string in cache and return.      |                |                |                  |          
            res = chr(b) if b in self.safe else '%{:02X}'.format(b)              | res = 'a'      | res = ' '      | res = '%3F'      | res = 'Y'
            self[b] = res                                                        | self[97] = 'a' | self[32] = ' ' | self[63] = '%3F' | self[89] 
            return res                                                           | return 'a'     | return ' '     | return '%3F'     | return 'Y
    $

At last, I've found the actual code that's doing the encoding. It also seems to
store the encoded results in a default dictionary. I can see the first few calls
in columns off to the right. To see all of the calls, I redirect the display to
a text file, then open it with `less`.

    $ space_tracer --traced=urllib.parse.Quoter url_client.py > quoter.txt
    $ less --chop-long-lines quoter.txt
    class Quoter(collections.defaultdict):                                       |
        """A mapping from bytes (in range(0,256)) to strings.                    |
                                                                                 |
        String values are percent-encoded byte values, unless the key < 128, and |
        in the "safe" set (either the specified safe set, or default set).       |
        """                                                                      |
        # Keeps a cache internally, using defaultdict, for efficiency (lookups   |
        # of cached keys don't call Python code at all).                         |
        def __init__(self, safe):                                                | safe = b' '                                                                                   | safe = b''
            """safe: bytes object."""                                            |                                                                                               |
            self.safe = _ALWAYS_SAFE.union(safe)                                 | self.safe = frozenset({32, 45, 46, 48, 49, 50, [233 chars]117, 118, 119, 120, 121, 122, 126}) | self.safe = frozenset({45, 46, 48, 49, 50, 51, [229 chars]117, 118, 119, 120, 121, 122, 126})
                                                                                 |
        def __repr__(self):                                                      |
            # Without this, will just display as a defaultdict                   |
            return "<%s %r>" % (self.__class__.__name__, dict(self))             |
                                                                                 |
        def __missing__(self, b):                                                | b = 97         | b = 32         | b = 63           | b = 89         | b = 101         | b = 115         | b = 33           | b = 98         | b = 47           | b = 99
            # Handle a cache miss. Store quoted string in cache and return.      |                |                |                  |                |                 |                 |                  |                |                  |
            res = chr(b) if b in self.safe else '%{:02X}'.format(b)              | res = 'a'      | res = ' '      | res = '%3F'      | res = 'Y'      | res = 'e'       | res = 's'       | res = '%21'      | res = 'b'      | res = '%2F'      | res = 'c'
            self[b] = res                                                        | self[97] = 'a' | self[32] = ' ' | self[63] = '%3F' | self[89] = 'Y' | self[101] = 'e' | self[115] = 's' | self[33] = '%21' | self[98] = 'b' | self[47] = '%2F' | self[99] = 'c'
            return res                                                           | return 'a'     | return ' '     | return '%3F'     | return 'Y'     | return 'e'      | return 's'      | return '%21'     | return 'b'     | return '%2F'     | return 'c'

I use the arrow keys to scroll all the way to the right, and see all the calls.
I wonder why "No" doesn't get passed in, when "Yes!" does. Then I look back up
at the `quote_from_bytes()` function, and see that "No" gets returned without
encoding, because all its characters are safe.

Finally, to make my script encode spaces with `%20`, I can change the default of
`quote_plus` to just `quote`.

    from urllib.parse import urlencode, quote

    encoded = urlencode({'a ?': 'Yes!', 'b/c': 'No'}, quote_via=quote)

    print(encoded)

That gives me the results my users were asking for:

    $ python url_client.py 
    a%20%3F=Yes%21&b%2Fc=No
    $

I was able to dig around in the code without changing any of it, and I only had
to install one package.

## Static Display
The static display of variable values, loop iterations, and function calls can
be easier to read than stepping through a debugger. You can go forward and
backward in time just by reading up and down, left and right.

For example, here's a binary search function that I'm in the middle of writing.
The search function takes in a number and a sorted list of numbers, then
searches the list to find where the number is in the list. Each loop, it looks
at a portion of the list, finds the middle number, and decides whether to
continue looking in the first half or the second half of the list.

    def search(n, a):             | n = 4 | a = [1, 2, 4] 
        low = 0                   | low = 0 
        high = len(a) - 1         | high = 2 
        while True:               |         | 
            mid = low + high // 2 | mid = 1 | mid = 3 
            v = a[mid]            | v = 2   | IndexError: list index out of range 
            if n == v:            |         | 
                return mid        |         | 
            if n < v:             |         | 
                high = mid - 1    |         | 
            else:                 |         | 
                low = mid + 1     | low = 2 | 
        return -1                 | 
                                  | 
    i = search(4, [1, 2, 4])      | IndexError: list index out of range 

Oops, I get an IndexError. Without this display, I would just get a traceback
that shows where the error happened, but not how it happened. Now, I can walk
back from the error to see where things went wrong. `mid` is the index
value, and it's calculated at the top of the loop. The two values that go into
it are both 2, so they should average to 2. Oh, I need parentheses to calculate
the average.

    def search(n, a):               | n = 4 | a = [1, 2, 4] 
        low = 0                     | low = 0 
        high = len(a) - 1           | high = 2 
        while True:                 |         | 
            mid = (low + high) // 2 | mid = 1 | mid = 2 
            v = a[mid]              | v = 2   | v = 4 
            if n == v:              |         | 
                return mid          |         | return 2 
            if n < v:               |         | 
                high = mid - 1      |         | 
            else:                   |         | 
                low = mid + 1       | low = 2 | 
        return -1                   | 
                                    | 
    i = search(4, [1, 2, 4])        | i = 2 

## Other Features
There are a couple of other features that might be helpful, run
`space_tracer -h` for a complete list. If a script reads standard input, you can
redirect it from a file with the `--stdin` option. If you want to control how
the source code on the left appears, you can trim it with `--source_width` and
pad it with `--source_indent`. You can also run modules like `unittest` by using
the `-m` option.

Remember, you can find installation instructions and descriptions of all the
other plugins and tools by visiting [donkirkby.github.com][livepy]. Help me test
it, and report your bugs. I'd also love to hear about any other projects working
on the same kind of tools.

[quick-and-dirty guide]: https://snarky.ca/a-quick-and-dirty-guide-on-how-to-install-packages-for-python/
[livepy]: https://donkirkby.github.io/live-py-plugin/
