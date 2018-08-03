from unittest import TestCase

from example_source import bar


class Bucket(object):
    def __init__(self):
        self.contents = []

    def add(self, item):
        self.contents.append(item)


class BarTest(TestCase):
    def test_bar(self):
        bucket = Bucket()

        bar(bucket)

        self.assertEqual(['bar'], bucket.contents)
