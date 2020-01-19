from unittest.mock import patch

# noinspection PyUnresolvedReferences
with patch.object(__builtins__, 'sum', return_value=99):
    from example_source import foo

print(foo(10))
