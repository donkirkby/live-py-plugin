from space_tracer import traced


# When tracing another file that imports this file, this decorator shouldn't
# mute that other file.
@traced
def traced_function():
    print('Hello, World!')
