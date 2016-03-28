""" Example unit test to be called when live coding. """

from unittest import TestCase
from tools.lib import foo


class FooTest(TestCase):
    def test_foo(self):
        n = foo(13)
        expected_n = 18

        self.assertEqual(expected_n, n)
