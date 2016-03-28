""" Demonstrates using a unit test to exercise your code when live coding. """


def foo(x):
    return x + 5

if __name__ == '__live_coding__':
    import unittest
    from tools.lib_test import FooTest

    suite = unittest.TestSuite()
    suite.addTest(FooTest("test_foo"))
    test_results = unittest.TextTestRunner().run(suite)

    print(test_results.errors)
    print(test_results.failures)
