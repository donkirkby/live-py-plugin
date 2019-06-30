import argparse
import re
from ast import (fix_missing_locations, iter_fields, parse, Add, Assign, AST,
                 Attribute, BitAnd, BitOr, BitXor, Call, Div, Ellipsis,
                 ExceptHandler, Expr, ExtSlice, FloorDiv, ImportFrom, Index,
                 List, Load, LShift, Mod, Mult, Name, NodeTransformer, Num,
                 Pow, Raise, Return, RShift, Slice, Store, Str, Sub, Subscript,
                 Tuple, Yield, keyword)
from contextlib import contextmanager
from copy import deepcopy
import __future__
from inspect import currentframe
import io

import sys
import traceback
import types
import os
from importlib import import_module

try:
    from importlib.abc import MetaPathFinder, Loader
    from importlib.machinery import ModuleSpec
    from importlib.util import find_spec
    imp = None
except ImportError:
    # Stub out the classes for older versions of Python.
    class MetaPathFinder(object):
        pass

    Loader = ModuleSpec = object
    find_spec = None
    import imp
try:
    builtins = import_module('__builtin__')
except ImportError:
    import builtins


IS_PYODIDE = __name__ == 'builtins'
if IS_PYODIDE:
    # noinspection PyUnresolvedReferences
    from js import document, window
    standard_b64encode = Canvas = MockTurtle = MockPyglet = None
else:
    from base64 import standard_b64encode
    from .canvas import Canvas
    from .mock_turtle import MockTurtle, monkey_patch_pyglet
    from .report_builder import ReportBuilder
    from .traced_finder import PSEUDO_FILENAME, TracedFinder, LIVE_MODULE_NAME, DEFAULT_MODULE_NAME

    document = None

# Import some classes that are only available in Python 3.
try:
    from ast import arg, Starred
except ImportError:
    arg = Starred = None
try:
    from ast import FormattedValue
except ImportError:
    FormattedValue = None
try:
    from ast import TryExcept, TryFinally
except ImportError:
    # Make Python 3.3 try class compatible with old versions.
    from ast import Try as TryExcept
    TryFinally = TryExcept

try:
    from itertools import izip_longest
except ImportError:
    from itertools import zip_longest as izip_longest

CONTEXT_NAME = '__live_coding_context__'
RESULT_NAME = '__live_coding_result__'

OPERATOR_CHARS = {Add: '+',
                  Sub: '-',
                  Mult: '*',
                  Div: '/',
                  FloorDiv: '//',
                  Mod: '%',
                  Pow: '**',
                  RShift: '>>',
                  LShift: '<<',
                  BitAnd: '&',
                  BitXor: '^',
                  BitOr: '|'}


def find_line_numbers(node, line_numbers):
    """ Populates a set with all line numbers for the node and its descendants.

    :param node: AST node to scan
    :param set line_numbers: all the line numbers will be added to it.
    """
    if FormattedValue is not None and isinstance(node, FormattedValue):
        # FormattedValue is a separate code block with its own line nums.
        return

    line_number = getattr(node, 'lineno', None)
    if line_number is not None:
        line_numbers.add(line_number)
    for _, value in iter_fields(node):
        if isinstance(value, list):
            for item in value:
                if isinstance(item, AST):
                    find_line_numbers(item, line_numbers)
        elif isinstance(value, AST):
            find_line_numbers(value, line_numbers)


# noinspection PyPep8Naming
class Tracer(NodeTransformer):
    @staticmethod
    def _set_statement_line_numbers(statements,
                                    previous_line_number=None):
        """ Make sure that a series of statements have line numbers in order.
        previous_line_number is the line number to start with, or None."""
        for statement in statements:
            line_number = getattr(statement, 'lineno', None)
            if (line_number is None and statement is not None and
                    previous_line_number is not None):
                statement.lineno = previous_line_number
            else:
                line_numbers = set()
                find_line_numbers(statement, line_numbers)
                previous_line_number = max(line_numbers)

    def visit(self, node):
        new_node = super(Tracer, self).visit(node)
        body = getattr(new_node, 'body', None)
        if body is not None:
            previous_line_number = getattr(new_node, 'lineno', None)
            try:
                statements = iter(body)
            except TypeError:
                # body doesn't contain statements
                statements = []
            self._set_statement_line_numbers(statements, previous_line_number)
        return new_node

    @staticmethod
    def _get_attribute_names(attribute_node):
        names = []
        while isinstance(attribute_node, Attribute):
            names.insert(0, attribute_node.attr)
            attribute_node = attribute_node.value

        if not names:
            return None

        names.insert(0, getattr(attribute_node, 'id', '<?>'))
        return names

    def _wrap_subscript_target(self, subscript, index_to_get=None):
        """ Build string describing subscript target and wrap indexes.

        For example, "x[{!r}]" for one index. Each index will be wrapped in a
        call to context.add_assignment_index() or
        context.get_assignment_index().
        @param index_to_get: if this is None, wrap in add_assignment_index(),
            otherwise, wrap in get_assignment_index(index_to_get).
        @return: string, or None if no assignment can be reported.
        """
        slice_text, next_index = self._wrap_slice(subscript.slice,
                                                  index_to_get)
        value = subscript.value
        if isinstance(value, Name):
            value_text = value.id
        elif isinstance(value, Subscript):
            value_text = self._wrap_subscript_target(value,
                                                     index_to_get=next_index)
        elif isinstance(value, Attribute):
            value_text = '.'.join(self._get_attribute_names(value))
        else:
            value_text = None
        if value_text is None:
            format_text = None
        else:
            format_text = '{}[{}]'.format(value_text, slice_text)
        return format_text

    def _wrap_assignment_index(self, index_node, index_to_get):
        """ Wrap an index node in an assignment index method.

        @param index_node: the node to read when setting the index.
        @param index_to_get: None when setting the index, or an integer when
            getting the index.
        @return: the wrapped node
        """
        if index_to_get is None:
            return self._create_bare_context_call(
                'add_assignment_index',
                [index_node])
        return self._create_bare_context_call(
            'get_assignment_index',
            [Num(n=index_to_get)])

    def _wrap_slice(self, sliceNode, index_to_get=None):
        """ Wrap a slice in calls to assignment index methods.

        Also build a format string for the slice.
        @param index_to_get: if this is None, wrap in add_assignment_index(),
            otherwise, wrap in get_assignment_index(index_to_get).
        @return: format_text, next_index_to_get
        """
        if isinstance(sliceNode, (Index, Ellipsis)):
            if (isinstance(sliceNode, Ellipsis) or
                    isinstance(sliceNode.value, Ellipsis)):
                index_to_get = None
                format_text = '...'
            else:
                sliceNode.value = self._wrap_assignment_index(
                    sliceNode.value,
                    index_to_get)
                format_text = '{!r}'
                if index_to_get is not None:
                    index_to_get -= 1
        elif isinstance(sliceNode, Slice):
            index_to_get = None
            if sliceNode.step is None:
                step_text = ''
            else:
                step_text = ':{!r}'
                sliceNode.step = self._wrap_assignment_index(
                    sliceNode.step,
                    index_to_get)
            if sliceNode.upper is None:
                upper_text = ''
            else:
                upper_text = '{!r}'
                sliceNode.upper = self._wrap_assignment_index(
                    sliceNode.upper,
                    index_to_get)
            if sliceNode.lower is None:
                lower_text = ''
            else:
                lower_text = '{!r}'
                sliceNode.lower = self._wrap_assignment_index(
                    sliceNode.lower,
                    index_to_get)
            format_text = '{}:{}{}'.format(lower_text, upper_text, step_text)
        else:
            assert isinstance(sliceNode, ExtSlice)
            index_to_get = None
            format_text = ', '.join(self._wrap_slice(subslice)[0]
                                    for subslice in sliceNode.dims)
        return format_text, index_to_get

    def visit_Call(self, node):
        existing_node = self.generic_visit(node)
        func_node = existing_node.func

        if self._is_untraceable_attribute(func_node):
            return existing_node

        comparisons = []  # [(name, node)]
        names = self._get_attribute_names(func_node)
        if names is not None:
            comparisons.append(('.'.join(names[:-1]),
                                existing_node.func.value))

        for arg_node in existing_node.args:
            if isinstance(arg_node, Name):
                comparisons.append((arg_node.id, arg_node))

        if not comparisons:
            return existing_node
        args = [List(elts=[], ctx=Load()),
                List(elts=[], ctx=Load()),
                existing_node,
                List(elts=[], ctx=Load()),
                Num(n=existing_node.lineno)]
        for name, node in comparisons:
            args[0].elts.append(Str(s=name))  # name
            args[1].elts.append(  # repr() before
                self._create_bare_context_call('get_repr', [node]))
            args[3].elts.append(  # repr() after
                self._create_bare_context_call('get_repr', [node]))
        new_node = self._create_bare_context_call('record_call', args)
        return new_node

    def visit_Delete(self, node):
        existing_node = self.generic_visit(node)
        for target in existing_node.targets:
            attribute_names = self._get_attribute_names(target)
            if attribute_names:
                target_name = '.'.join(attribute_names[:-1])
            else:
                target_value = getattr(target, 'value', None)
                attribute_names = self._get_attribute_names(target_value)
                if attribute_names:
                    target_name = '.'.join(attribute_names)
                else:
                    target_name = getattr(target_value, 'id', None)
            if target_name is not None:
                args = [Str(s=target_name), target.value, Num(n=target.lineno)]
                target.value = self._create_bare_context_call('record_delete',
                                                              args)
        return existing_node

    def _is_untraceable_attribute(self, node):
        if isinstance(node, Attribute):
            if isinstance(node.value, Name):
                return False
            if isinstance(node.value, Attribute):
                return self._is_untraceable_attribute(node.value)
            return True
        return False

    def visit_Assign(self, node):
        existing_node = self.generic_visit(node)
        try:
            targets = existing_node.targets
        except AttributeError:
            targets = [existing_node.target]
        if any(map(self._is_untraceable_attribute, targets)):
            return existing_node
        line_numbers = set()
        find_line_numbers(existing_node, line_numbers)
        first_line_number = min(line_numbers)
        last_line_number = max(line_numbers)
        new_nodes = []
        format_string = self._wrap_assignment_targets(
            targets)
        if (len(targets) == 1 and
                isinstance(targets[0], Tuple)):
            existing_node.value = Call(func=Name(id='tuple', ctx=Load()),
                                       args=[existing_node.value],
                                       keywords=[],
                                       starargs=None,
                                       kwargs=None)
        existing_node.value = self._create_bare_context_call(
            'set_assignment_value',
            [existing_node.value])
        new_nodes.append(self._create_context_call('start_assignment'))
        try_body = [existing_node]
        if format_string is not None:
            try_body.append(self._create_context_call(
                'report_assignment',
                [Str(s=format_string), Num(n=existing_node.lineno)]))
        end_assignment = self._create_context_call('end_assignment')
        finally_body = [end_assignment]
        new_nodes.append(TryFinally(body=try_body,
                                    finalbody=finally_body,
                                    handlers=[],
                                    orelse=[],
                                    lineno=first_line_number))
        self._set_statement_line_numbers(try_body, first_line_number)
        self._set_statement_line_numbers(finally_body, last_line_number)

        return new_nodes

    def visit_AnnAssign(self, node):
        return self.visit_Assign(node)

    def visit_AugAssign(self, node):
        read_target = deepcopy(node.target)
        existing_node = self.generic_visit(node)
        line_numbers = set()
        find_line_numbers(existing_node, line_numbers)
        first_line_number = min(line_numbers)
        last_line_number = max(line_numbers)
        new_nodes = []
        try_body = [existing_node]
        new_nodes.append(self._create_context_call('start_assignment'))
        format_string = self._wrap_assignment_target(existing_node.target)
        if format_string is not None:
            if ':' in format_string:
                existing_node.value = self._create_bare_context_call(
                    'set_assignment_value',
                    [existing_node.value])
                operator_char = OPERATOR_CHARS.get(type(existing_node.op), '?')
                format_string += ' {}= {{}} '.format(operator_char)
            else:
                self._wrap_assignment_target(read_target, index_to_get=-1)
                read_target.ctx = Load()
                set_assignment_value = self._create_context_call(
                    'set_assignment_value',
                    [read_target])
                try_body.append(set_assignment_value)
                format_string += ' = {}'
            try_body.append(self._create_context_call(
                'report_assignment',
                [Str(s=format_string), Num(n=existing_node.lineno)]))
        end_assignment = self._create_context_call('end_assignment')
        finally_body = [end_assignment]
        new_nodes.append(TryFinally(body=try_body,
                                    finalbody=finally_body,
                                    handlers=[],
                                    orelse=[],
                                    lineno=first_line_number))
        self._set_statement_line_numbers(try_body, first_line_number)
        self._set_statement_line_numbers(finally_body, last_line_number)

        return new_nodes

    def visit_For(self, node):
        new_node = self.generic_visit(node)

        # Collect line numbers from all except else block.
        line_numbers = set()
        find_line_numbers(new_node.target, line_numbers)
        find_line_numbers(new_node.iter, line_numbers)
        for statement in new_node.body:
            find_line_numbers(statement, line_numbers)
        line_numbers.add(new_node.lineno)
        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        new_body = [self._create_context_call('start_block', args)]
        new_body.extend(self._trace_assignment_list(new_node.target))
        new_body.extend(new_node.body)
        new_node.body = new_body
        return new_node

    def visit_While(self, node):
        new_node = self.generic_visit(node)

        # Collect line numbers from all except else block.
        line_numbers = set()
        find_line_numbers(new_node.test, line_numbers)
        for statement in new_node.body:
            find_line_numbers(statement, line_numbers)
        line_numbers.add(new_node.lineno)
        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        new_node.body.insert(0,
                             self._create_context_call('start_block', args))
        return new_node

    # noinspection PyTypeChecker
    def visit_FunctionDef(self, node):
        """ Instrument a function definition by creating a new report builder
        for this stack frame and putting it in a local variable. The local
        variable has the same name as the global variable so all calls can
        use the same CONTEXT_NAME symbol, but it means that I had to use this:
        x = globals()['x'].start_frame()
        Kind of ugly, but I think it was worth it to handle recursive calls.
        """
        new_node = self.generic_visit(node)

        line_numbers = set()
        find_line_numbers(new_node, line_numbers)
        first_line_number = min(line_numbers)
        last_line_number = max(line_numbers)
        args = [Num(n=first_line_number),
                Num(n=last_line_number)]
        start_frame_keywords = []
        for decorator in new_node.decorator_list:
            if decorator.id == 'traced':
                start_frame_keywords.append(
                    keyword(arg='is_decorated', value=Name(id='True',
                                                           ctx=Load())))
        try_body = new_node.body
        globals_call = Call(func=Name(id='globals', ctx=Load()),
                            args=[],
                            keywords=[],
                            starargs=None,
                            kwargs=None)
        global_context = Subscript(value=globals_call,
                                   slice=Index(value=Str(s=CONTEXT_NAME)),
                                   ctx=Load())
        start_frame_call = Call(func=Attribute(value=global_context,
                                               attr='start_frame',
                                               ctx=Load()),
                                args=args,
                                keywords=start_frame_keywords,
                                starargs=None,
                                kwargs=None)
        context_assign = Assign(targets=[Name(id=CONTEXT_NAME, ctx=Store())],
                                value=start_frame_call)
        new_node.body = [context_assign]
        if isinstance(try_body[0], Expr) and isinstance(try_body[0].value, Str):
            # Move docstring back to top of function.
            # noinspection PyUnresolvedReferences
            new_node.body.insert(0, try_body.pop(0))

        # trace function parameter values
        for target in new_node.args.args:
            if isinstance(target, Name) and target.id == 'self':
                continue
            if arg and isinstance(target, arg) and target.arg == 'self':
                continue
            new_node.body.append(self._trace_assignment(target, node.lineno))
        if new_node.args.vararg is not None:
            new_node.body.append(
                self._trace_assignment(new_node.args.vararg, node.lineno))
        if new_node.args.kwarg is not None:
            new_node.body.append(
                self._trace_assignment(new_node.args.kwarg, node.lineno))

        if try_body:
            handler_body = [self._create_context_call('exception'),
                            Raise()]
            new_node.body.append(
                TryExcept(body=try_body,
                          handlers=[ExceptHandler(body=handler_body)],
                          orelse=[],
                          finalbody=[]))
            self._set_statement_line_numbers(try_body, first_line_number)
            self._set_statement_line_numbers(handler_body, last_line_number)
        return new_node

    @staticmethod
    def _is_module_header(statement):
        if isinstance(statement, ImportFrom):
            return statement.module == '__future__'
        if isinstance(statement, Expr):
            return isinstance(statement.value, Str)
        return False

    def visit_Module(self, node):
        new_node = self.generic_visit(node)
        line_numbers = set()
        new_body = []
        try_body = new_node.body
        if try_body:
            while try_body and self._is_module_header(try_body[0]):
                # noinspection PyUnresolvedReferences
                new_body.append(try_body.pop(0))
            find_line_numbers(new_node, line_numbers)
        if line_numbers:
            first_line_number = min(line_numbers)
            last_line_number = max(line_numbers)
            handler_body = [self._create_context_call('exception'),
                            Raise()]
            handler = ExceptHandler(body=handler_body,
                                    lineno=last_line_number)
            new_body.append(TryExcept(body=try_body,
                                      handlers=[handler],
                                      orelse=[],
                                      finalbody=[]))
            new_node.body = new_body
            self._set_statement_line_numbers(try_body, first_line_number)
            self._set_statement_line_numbers(handler_body, last_line_number)
        return new_node

    # noinspection PyPep8Naming
    def visit_Lambda(self, node):
        new_node = self.generic_visit(node)

        line_numbers = set()
        find_line_numbers(new_node, line_numbers)

        arg_names = (getattr(old_arg, 'id', getattr(old_arg, 'arg', None))
                     for old_arg in new_node.args.args)
        new_args = [Num(n=min(line_numbers)),
                    Num(n=max(line_numbers))]
        new_args.extend(Name(id=name, ctx=Load())
                        for name in arg_names)
        new_args.append(new_node.body)
        new_node.body = self._create_bare_context_call('report_lambda',
                                                       new_args)
        return new_node

    # noinspection PyPep8Naming
    def visit_Return(self, node):
        existing_node = self.generic_visit(node)
        value = existing_node.value
        if value is None:
            return existing_node

        return [Assign(targets=[Name(id=RESULT_NAME, ctx=Store())],
                       value=value),
                self._create_context_call('return_value',
                                          [Name(id=RESULT_NAME, ctx=Load()),
                                           Num(n=existing_node.lineno)]),
                Return(value=Name(id=RESULT_NAME, ctx=Load()))]

    def visit_TryExcept(self, node):
        existing_node = self.generic_visit(node)
        for handler in existing_node.handlers:
            handler_name = getattr(handler.name, 'id', handler.name)
            if handler_name is not None:
                handler.body.insert(0, self._create_context_call(
                    'assign',
                    [Str(s=handler_name),
                     Name(id=handler_name, ctx=Load()),
                     Num(n=handler.lineno)]))
            handler.body.insert(0, self._create_context_call('exception'))
        return existing_node

    def visit_Try(self, node):
        # Python 3.3 renamed TryExcept and TryFinally to Try
        return self.visit_TryExcept(node)

    def visit_Raise(self, node):
        existing_node = self.generic_visit(node)
        new_nodes = [existing_node]
        node_exc = getattr(node, 'type', None)
        node_exc = getattr(node, 'exc', node_exc)
        if node_exc is None:
            # Reraising the current exception, so we have to report this line.

            new_nodes.insert(0, self._create_context_call(
                'exception',
                [Num(n=existing_node.lineno)]))
        return new_nodes

    def visit_Yield(self, node):
        existing_node = self.generic_visit(node)
        value = existing_node.value
        if value is None:
            value = Name(id='None', ctx=Load())

        return Yield(value=self._create_bare_context_call(
                    'yield_value',
                    [value, Num(n=existing_node.lineno)]))

    def visit_YieldFrom(self, node):
        existing_node = self.generic_visit(node)
        value = existing_node.value
        existing_node.value = self._create_bare_context_call(
            'yield_from',
            [value, Num(n=existing_node.lineno)])
        return existing_node

    def _trace_assignment_list(self, targets):
        """ Build a list of assignment calls based on the contents of targets.
        If targets is a single name, then return a list with one call.
        If targets is a Tuple or a List, then make recursive calls for each
        item, combine the results into a list, and return it."""

        new_nodes = []
        # Tuple and List hold their contents in elts.
        todo = getattr(targets, 'elts', targets)
        try:
            todo = list(todo)
        except TypeError:
            # wasn't iterable, treat it as a single item
            trace = self._trace_assignment(targets)
            if trace:
                new_nodes.append(trace)
            return new_nodes
        for target in todo:
            new_nodes.extend(self._trace_assignment_list(target))
        return new_nodes

    def _trace_assignment(self, target, default_lineno=None):
        lineno = getattr(target, 'lineno', default_lineno)
        # name, value, line number
        if isinstance(target, Name):
            arg_name = target.id
        elif arg and isinstance(target, arg):
            arg_name = target.arg
        else:
            assert_message = 'Target type was {}.'.format(type(target))
            assert isinstance(target, str), assert_message
            arg_name = target

        args = [Str(s=arg_name),
                Name(id=arg_name, ctx=Load()),
                Num(n=lineno)]
        return self._create_context_call('assign', args)

    def _wrap_assignment_target(self, target, index_to_get=None):
        """ Build string describing one assignment target and wrap indexes.

        For example, "x" for a variable target, or "x[{!r}] for an indexed
        target. An indexed target will have each index wrapped in a call to
        context.add_assignment_index() or context.get_assignment_index().
        @param index_to_get: if this is None, wrap in add_assignment_index(),
            otherwise, wrap in get_assignment_index(index_to_get).
        @return: string, or None if no assignment can be reported.
        """
        if isinstance(target, Name):
            return target.id
        if isinstance(target, Subscript):
            return self._wrap_subscript_target(target, index_to_get)
        if isinstance(target, Tuple) or isinstance(target, List):
            target_names = map(self._wrap_assignment_target, target.elts)
            wrapped = '({})'.format(', '.join(target_names))
            if len(target.elts) == 1:
                wrapped = wrapped[:-1] + ',)'
            return wrapped
        if Starred is not None and isinstance(target, Starred):
            return '*{}'.format(target.value.id)
        assert_message = 'Assignment target had type {}.'.format(type(target))
        assert isinstance(target, Attribute), assert_message
        names = self._get_attribute_names(target)
        return '.'.join(names)

    def _wrap_assignment_targets(self, targets):
        """ Build string describing assignment targets and wrap indexes.

        For example, "x = {}" for a single target, "x = y = {}" for
        multiple targets, or "x[{!r}] = {} for an indexed target.
        @return: string, or None if no assignment can be reported.
        """
        strings = []
        for target in targets:
            format_text = self._wrap_assignment_target(target)
            if format_text is not None:
                strings.append(format_text)
        if not strings:
            return None
        strings.append('{}')  # for assignment value
        return ' = '.join(strings)

    def _create_context_call(self, function_name, args=None):
        """ Create a method call expression on the live coding context object. """
        return Expr(value=self._create_bare_context_call(function_name, args))

    @staticmethod
    def _create_bare_context_call(function_name, args=None):
        """ Create a method call on the live coding context object.

        Bare means that it is not wrapped in an expression. """
        if args is None:
            args = []
        context_name = Name(id=CONTEXT_NAME, ctx=Load())
        func = Attribute(value=context_name,
                         attr=function_name,
                         ctx=Load())
        return Call(func=func,
                    args=args,
                    keywords=[],
                    starargs=None,
                    kwargs=None)


class LineNumberCleaner(NodeTransformer):
    def __init__(self):
        self.max_line = 0

    def visit(self, node):
        lineno = getattr(node, 'lineno', None)
        if lineno is not None:
            if lineno < self.max_line:
                node.lineno = self.max_line
            else:
                self.max_line = lineno
        return self.generic_visit(node)


class DelegatingModuleFinder(MetaPathFinder):
    def find_spec(self, fullname, path, target):
        for finder in self.following_finders:
            finder_find_spec = getattr(finder, 'find_spec', None)
            if finder_find_spec:
                spec = finder_find_spec(fullname, path, target)
                if spec is not None:
                    return spec

    # find_module() and load_module() are used in Python 2.
    def find_module(self, fullname, path):
        for finder in self.following_finders:
            loader = finder.find_module(fullname, path)
            if loader is not None:
                return loader

    @property
    def following_finders(self):
        is_after = False
        for finder in sys.meta_path:
            if not is_after:
                is_after = finder is self
                continue
            yield finder


class PatchedModuleFinder(DelegatingModuleFinder):
    is_desperate = False

    def __init__(self, is_zoomed):
        self.is_zoomed = is_zoomed

    def find_spec(self, fullname, path, target=None):
        if fullname not in ('matplotlib',
                            'matplotlib.pyplot',
                            'numpy.random',
                            'random',
                            'pyglet'):
            return None
        spec = super(PatchedModuleFinder, self).find_spec(fullname, path, target)
        if spec is not None:
            spec.loader = PatchedModuleLoader(fullname,
                                              spec.loader,
                                              self.is_zoomed)
            return spec

    # find_module() and load_module() are used in Python 2.
    def find_module(self, fullname, path=None):
        if fullname not in ('matplotlib',
                            'matplotlib.pyplot',
                            'numpy.random',
                            'random',
                            'pyglet'):
            return None
        loader = super(PatchedModuleFinder, self).find_module(fullname, path)
        if loader is not None:
            return PatchedModuleLoader(fullname, loader, self.is_zoomed)
        if sys.version_info < (3, 0) and not PatchedModuleFinder.is_desperate:
            # Didn't find anyone to load the module, get desperate.
            PatchedModuleFinder.is_desperate = True
            return PatchedModuleLoader(fullname, None, self.is_zoomed)


# noinspection PyAbstractClass
class TracedModuleImporter(DelegatingModuleFinder, Loader):

    def __init__(self,
                 source_code,
                 traced,
                 environment,
                 filename,
                 driver_module):
        """ Import the code that has been instrumented for live coding.

        :param str source_code: source code to load for traced module
        :param environment: global variables for the module
        :param filename: the name of the file this code came from
        :param str driver_module: module name, if the driver is a module
        """
        self.source_code = source_code
        self.traced = traced
        self.environment = environment
        self.filename = filename
        self.driver_module = driver_module
        self.source_finder = None
        self.module_files = {}

    def find_spec(self, fullname, path, target=None):
        if (fullname == self.traced or
                fullname in (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME) and
                self.traced.startswith(fullname)):
            return ModuleSpec(fullname, self, origin=self.filename)
        return None

    def exec_module(self, module):
        module_spec = getattr(module, '__spec__', None)
        if module_spec:
            module_file = module.__spec__.origin
        else:
            module_file = self.filename
        if (self.traced.startswith(DEFAULT_MODULE_NAME) or
                self.traced.startswith(LIVE_MODULE_NAME)):
            # self.source_finder = TracedFinder(self.source_code, self.traced)
            source_code = self.source_code
        elif self.filename is not None and module_file == self.filename:
            if self.source_code is None:
                with open(self.filename) as source_file:
                    self.source_code = source_file.read()
            source_code = self.source_code
        else:
            with open(module_file) as source_file:
                source_code = source_file.read()
        module_name = module.__name__
        is_module_traced = False
        source_tree = None
        if self.traced == module_name:
            is_module_traced = True
        else:
            if self.traced.startswith(module_name):
                traced_child = self.traced[len(module_name)+1:]
            elif self.traced in (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME):
                traced_child = self.traced
            else:
                traced_child = None
            if traced_child:
                source_finder = TracedFinder(source_code, traced_child)
                source_tree = source_finder.source_tree
                if source_finder.traced_node is not None:
                    is_module_traced = True
                    self.source_finder = source_finder
        if source_tree is None:
            source_tree = parse(source_code, PSEUDO_FILENAME)
        if is_module_traced:
            source_tree = Tracer().visit(source_tree)
            fix_missing_locations(source_tree)
            LineNumberCleaner().visit(source_tree)
        if (module_name in (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME) and
                self.driver_module):
            target_module = self.driver_module
        else:
            target_module = module_name
        if '.' in target_module:
            package_name, child_name = target_module.rsplit('.', 1)
        else:
            package_name = None
        module.__package__ = package_name
        module.__file__ = module_file
        module.__builtins__ = builtins
        module.__dict__.update(self.environment)
        self.environment = module.__dict__
        # from ast import dump
        # print(dump(source_tree, include_attributes=True))
        compiled_code = compile(source_tree, PSEUDO_FILENAME, 'exec')

        exec(compiled_code, self.environment)

    # find_module() and load_module() are used in Python 2.
    def find_module(self, fullname, path=None):
        if (fullname == self.traced or
                fullname in (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME) and
                self.traced.startswith(fullname)):
            return self

    def load_module(self, fullname):
        # noinspection PyDeprecation
        new_mod = imp.new_module(fullname)
        sys.modules[fullname] = new_mod

        self.exec_module(new_mod)
        return new_mod


# noinspection PyAbstractClass
class PatchedModuleLoader(Loader):
    def __init__(self, fullname, main_loader, is_zoomed):
        self.fullname = fullname
        self.main_loader = main_loader
        self.is_zoomed = is_zoomed
        self.plt = None

    def exec_module(self, module):
        if self.main_loader is not None:
            self.main_loader.exec_module(module)
        if self.fullname in ('numpy.random', 'random'):
            module.seed(0)
        elif self.fullname == 'matplotlib':
            module.use('Agg')
        elif self.fullname == 'matplotlib.pyplot':
            self.plt = module
            # noinspection PyProtectedMember
            turtle_screen = MockTurtle._screen
            screen_width = turtle_screen.cv.cget('width')
            screen_height = turtle_screen.cv.cget('height')
            module.show = self.mock_show
            module.live_coding_size = (screen_width, screen_height)
            module.live_coding_zoom = self.live_coding_zoom
            if self.is_zoomed:
                self.live_coding_zoom()
        elif self.fullname == 'pyglet':
            # noinspection PyProtectedMember
            monkey_patch_pyglet(MockTurtle._screen.cv)

    def load_module(self, fullname):
        if self.main_loader is not None:
            module = self.main_loader.load_module(fullname)
        else:
            module = import_module(fullname)
            PatchedModuleFinder.is_desperate = False
        self.exec_module(module)
        return module

    def mock_show(self, *_args, **_kwargs):
        figure = self.plt.gcf()
        # noinspection PyProtectedMember
        turtle_screen = MockTurtle._screen
        screen_width = turtle_screen.cv.cget('width')
        screen_height = turtle_screen.cv.cget('height')
        figure_width = figure.get_figwidth()*figure.dpi
        figure_height = figure.get_figheight()*figure.dpi
        if figure_width < screen_width:
            x = (screen_width - figure_width) // 2
        else:
            x = 0
        if figure_height < screen_height:
            y = (screen_height - figure_height) // 2
        else:
            y = 0
        # noinspection PyUnresolvedReferences
        data = io.BytesIO()
        self.plt.savefig(data, format='PNG')

        image = data.getvalue()
        encoded = standard_b64encode(image)
        image_text = str(encoded.decode('UTF-8'))
        MockTurtle.display_image(x, y, image=image_text)

    def live_coding_zoom(self):
        screen_width, screen_height = self.plt.live_coding_size
        fig = self.plt.gcf()
        fig_width, fig_height = fig.get_figwidth(), fig.get_figheight()
        x_dpi = screen_width/fig_width
        y_dpi = screen_height/fig_height
        fig.dpi = min(x_dpi, y_dpi)


@contextmanager
def swallow_output(stdin_path=None):
    old_main_mod = sys.modules.pop(DEFAULT_MODULE_NAME, None)
    old_stdout = sys.stdout
    old_stderr = sys.stderr
    old_stdin = sys.stdin
    # noinspection PyUnresolvedReferences
    old_string_io = io.StringIO
    try:
        sys.stdout = FileSwallower(old_stdout)
        sys.stderr = FileSwallower(old_stderr, target_name='sys.stderr')
        sys.stdin = stdin_path and open(stdin_path) or io.StringIO()
        with sys.stdin:
            io.StringIO = TracedStringIO
            yield
    finally:
        if old_main_mod is not None:
            sys.modules[DEFAULT_MODULE_NAME] = old_main_mod
        else:
            sys.modules.pop(DEFAULT_MODULE_NAME, None)
        sys.stdout = old_stdout
        sys.stderr = old_stderr
        sys.stdin = old_stdin
        io.StringIO = old_string_io


class CodeTracer(object):
    def __init__(self, canvas=None):
        self.message_limit = 10000
        self.max_width = None
        self.keepalive = False
        if MockTurtle is not None:
            MockTurtle.monkey_patch(canvas)
        self.environment = {}
        self.return_code = None

    def run_python_module(self, modulename):
        """Run a python module, as though with ``python -m name args...``.

        `modulename` is the name of the module, possibly a dot-separated name.

        This is based on code from coverage.py, by Ned Batchelder.
        https://bitbucket.org/ned/coveragepy
        """
        if find_spec:
            spec = find_spec(modulename)
            pathname = spec.origin
            packagename = spec.name
            if pathname.endswith("__init__.py") and not modulename.endswith("__init__"):
                mod_main = modulename + ".__main__"
                spec = find_spec(mod_main)
                if not spec:
                    raise ImportError(
                        "No module named %s; "
                        "%r is a package and cannot be directly executed"
                        % (mod_main, modulename))
                pathname = spec.origin
                packagename = spec.name
            packagename = packagename.rpartition(".")[0]
        else:
            openfile = None
            glo, loc = globals(), locals()
            try:
                # Search for the module - inside its parent package, if any -
                # using standard import mechanics.
                if '.' in modulename:
                    packagename, name = modulename.rsplit('.', 1)
                    package = __import__(packagename, glo, loc, ['__path__'])
                    searchpath = package.__path__
                else:
                    packagename, name = None, modulename
                    searchpath = None  # "top-level search" in imp.find_module()
                # noinspection PyDeprecation
                openfile, pathname, _ = imp.find_module(name, searchpath)

                # If `modulename` is actually a package, not a mere module,
                # then we pretend to be Python 2.7 and try running its
                # __main__.py script.
                if openfile is None:
                    packagename = modulename
                    name = '__main__'
                    package = __import__(packagename, glo, loc, ['__path__'])
                    searchpath = package.__path__
                    # noinspection PyDeprecation
                    openfile, pathname, _ = imp.find_module(name, searchpath) 
            finally:
                if openfile:
                    openfile.close()

        # Finally, hand the file off to run_python_file for execution.
        pathname = os.path.abspath(pathname)
        self.run_python_file(pathname, package=packagename)

    def run_python_file(self, filename, package=None):
        """Run a python file as if it were the main program on the command line.

        `filename` is the path to the file to execute.
        """
        # Create a module to serve as __main__
        # noinspection PyUnresolvedReferences
        main_mod = types.ModuleType('__main__')
        sys.modules['__main__'] = main_mod
        main_mod.__file__ = filename
        main_mod.__builtins__ = builtins
        if package:
            main_mod.__package__ = package

        code = self.make_code_from_py(filename)

        # Execute the code object.
        exec(code, main_mod.__dict__)

    @staticmethod
    def make_code_from_py(filename):
        """Get source from `filename` and make a code object of it."""
        with open(filename, 'rU') as f:
            source = f.read()

        code = compile(source, filename, "exec")

        return code

    @staticmethod
    def split_lines(messages):
        for message in messages:
            for line in message.splitlines():
                yield line

    def trace_turtle(self, source):
        self.trace_code(source)

        return '\n'.join(MockTurtle.get_all_reports())

    def report_driver_result(self, builder, messages):
        messages = list(self.split_lines(messages))
        block_size = len(messages) + 2
        builder.start_block(1, block_size)
        message_width = 1
        for lineno, message in enumerate(messages, 2):
            message_width = max(len(message), message_width)
            builder.add_message(message, lineno)

        header = '-' * message_width + ' '
        builder.add_message(header, 1)
        builder.add_message(header, block_size)
        builder.start_block(1, block_size)

    def trace_code(self,
                   source,
                   traced=DEFAULT_MODULE_NAME,
                   is_module=False,
                   source_width=0,
                   source_indent=0,
                   driver=None,
                   traced_file=None,
                   stdin=None,
                   bad_driver=None,
                   is_zoomed=False):
        """ Trace a module of source code, possibly by running a driver script.

        :param str source: the source code to trace, or None to load normally
        :param str traced: the module, method, or function name to trace
        :param bool is_module: True if the driver is a module name instead of a
        file name
        :param int? source_width: Width of source code - use 0 to hide or
        negative numbers to trim columns from the end, None to fit source code.
        :param int source_indent: Number of spaces to indent source code.
        Negative to skip first columns of source code.
        :param list driver: the driver script's file name or module name and args
        :param str traced_file: the file name of the source code
        :param str stdin: the file name to redirect stdin from
        :param str bad_driver: a message to display if the driver doesn't call
        the module
        :param bool is_zoomed: True if matplotlib is zoomed
        __main__.
        """
        builder = ReportBuilder(self.message_limit)
        builder.max_width = self.max_width
        self.return_code = 0

        try:
            # Set sys.argv properly.
            old_argv = sys.argv
            sys.argv = driver or [traced_file or traced]

            try:
                self.run_code(source,
                              builder,
                              traced,
                              is_module,
                              driver,
                              traced_file,
                              bad_driver,
                              is_zoomed,
                              stdin)
            finally:
                # Restore the old argv and path
                sys.argv = old_argv

                # During testing, we import these modules for every test case,
                # so force a reload. This is only likely to happen during testing.
                to_delete = []
                for name in sys.modules:
                    if traced.startswith(name) or name == LIVE_MODULE_NAME:
                        to_delete.append(name)
                for target in to_delete:
                    del sys.modules[target]
                for i in reversed(range(len(sys.meta_path))):
                    if (isinstance(sys.meta_path[i], TracedModuleImporter) or
                            isinstance(sys.meta_path[i], PatchedModuleFinder)):
                        sys.meta_path.pop(i)

            for value in self.environment.values():
                if isinstance(value, types.GeneratorType):
                    value.close()
        except SyntaxError:
            self.return_code = 1
            ex = sys.exc_info()[1]
            messages = traceback.format_exception_only(type(ex), ex)
            message = messages[-1].strip()
            if ex.filename == PSEUDO_FILENAME:
                line_number = ex.lineno
            else:
                line_number = 1
                message = '{} line {}: {}'.format(ex.filename,
                                                  ex.lineno,
                                                  message)
            builder.add_message(message, line_number)
        except BaseException as ex:
            self.return_code = getattr(ex, 'code', 1)
            etype, value, tb = sys.exc_info()
            is_reported = False
            entries = traceback.extract_tb(tb)
            for traced_file, _, _, _ in entries:
                if traced_file == PSEUDO_FILENAME:
                    is_reported = True
            while not is_reported and tb is not None:
                frame = tb.tb_frame
                code = frame.f_code
                traced_file = code.co_filename
                if __file__ not in (traced_file, traced_file + 'c'):
                    break
                tb = tb.tb_next
            if not is_reported:
                if tb:
                    messages = traceback.format_exception(etype, value, tb)
                else:
                    messages = traceback.format_exception_only(etype, value)
                self.report_driver_result(builder, messages)

        report = builder.report(source.count('\n'))
        if source_width != 0:
            if source_indent >= 0:
                indent = source_indent
                start_char = 0
            else:
                indent = 0
                start_char = -source_indent
            source_lines = source.splitlines()
            reported_source_lines = []
            for first_line, last_line in builder.reported_blocks:
                for line_number in range(first_line, last_line+1):
                    reported_source_lines.append(source_lines[line_number-1][start_char:])
            report_lines = report.splitlines()
            dump_lines = []
            max_width = max(map(len, reported_source_lines))
            if source_width is None:
                source_width = max_width + indent
            elif source_width < 0:
                source_width += max_width + indent
            for source_line, report_line in izip_longest(reported_source_lines,
                                                         report_lines,
                                                         fillvalue=''):
                padded_source_line = indent * ' ' + source_line
                padded_source_line += (source_width - len(source_line)) * ' '
                line = (padded_source_line[:source_width] +
                        ' | ' + report_line)
                dump_lines.append(line)
            report = '\n'.join(dump_lines)

        return report

    def run_code(self,
                 code,
                 builder,
                 traced,
                 is_module,
                 driver,
                 filename,
                 bad_driver,
                 is_zoomed,
                 stdin_path=None):
        """ Run the traced module, plus its driver.

        :param code: the source code for the traced module, or None to load
        from the normal file
        :param builder: the report builder
        :param str traced: the module, method, or function name to trace
        :param bool is_module: True if the driver is a module name instead of a
        file name
        :param list driver: the driver script's file name or module name and args
        :param str filename: the file name of the source code
        :param str bad_driver: a message to display if the driver doesn't call
        the module
        :param bool is_zoomed: True if matplotlib is zoomed
        :param str stdin_path: Path to redirect stdin from
        __main__.
        """
        self.environment[CONTEXT_NAME] = builder
        for module_name in ('random', 'numpy.random'):
            random_module = sys.modules.get(module_name)
            if random_module is not None:
                random_module.seed(0)

        sys.meta_path.insert(0, PatchedModuleFinder(is_zoomed))
        driver_module = driver[0] if is_module and driver else None
        module_importer = TracedModuleImporter(code,
                                               traced,
                                               self.environment,
                                               filename,
                                               driver_module)
        sys.meta_path.insert(0, module_importer)
        output_context = swallow_output(stdin_path)
        try:
            if traced.startswith(DEFAULT_MODULE_NAME):
                with output_context:
                    import_module(DEFAULT_MODULE_NAME)
            elif traced.startswith(LIVE_MODULE_NAME):
                with output_context:
                    import_module(LIVE_MODULE_NAME)
            else:
                with output_context:
                    try:
                        if not is_module:
                            self.run_python_file(driver[0])
                        else:
                            module_name = driver[0]
                            self.run_python_module(module_name)
                        if sys.stdout.saw_failures:
                            self.report_driver_result(builder, ['Pytest reported failures.'])
                            self.return_code = 1
                    except SystemExit as ex:
                        if ex.code:
                            self.return_code = ex.code
                            messages = traceback.format_exception_only(type(ex),
                                                                       ex)
                            message = messages[-1].strip()
                            self.report_driver_result(builder, [message])
                if traced not in sys.modules:
                    driver_name = os.path.basename(driver[0])
                    message = (bad_driver or "{} doesn't call the {} module."
                                             " Try a different driver.".format(driver_name,
                                                                               traced))
                    self.report_driver_result(builder, [message])
        finally:
            self.environment = module_importer.environment
            if (module_importer.source_finder and
                    module_importer.source_finder.traced_node):
                line_numbers = set()
                find_line_numbers(module_importer.source_finder.traced_node,
                                  line_numbers)
                builder.trace_block(min(line_numbers), max(line_numbers))


class FileSwallower(object):
    def __init__(self,
                 target,
                 check_buffer=True,
                 target_name=None):
        self.target = target
        self.target_name = target_name
        self.saw_failures = False
        if check_buffer:
            buffer = getattr(target, 'buffer', None)
            if buffer is not None:
                self.buffer = FileSwallower(buffer, check_buffer=False)

    def write(self, *args, **_):
        text = args and str(args[0]) or ''
        if re.search(r'^=+\s*FAILURES\s*=+$', text):
            self.saw_failures = True
        frame = currentframe()
        while frame is not None:
            report_builder = frame.f_locals.get(CONTEXT_NAME)
            if report_builder is not None:
                has_print_function = (
                    sys.version_info >= (3, 0) or
                    __future__.print_function in frame.f_globals.values())
                report_builder.add_output(text,
                                          frame.f_lineno,
                                          has_print_function,
                                          target_name=self.target_name)
                break
            frame = frame.f_back

    def __getattr__(self, name):
        return getattr(self.target, name)


def find_string_io_targets(frame):
    for name, value in frame.f_locals.items():
        yield name, value
        if name == 'self':
            for attr_name, attr_value in value.__dict__.items():
                yield 'self.' + attr_name, attr_value


# noinspection PyUnresolvedReferences
class TracedStringIO(io.StringIO):
    def write(self, text):
        super(TracedStringIO, self).write(text)
        frame = currentframe()
        while frame is not None:
            report_builder = frame.f_locals.get(CONTEXT_NAME)
            if report_builder is not None:
                for name, value in find_string_io_targets(frame):
                    if value is self:
                        report_builder.add_output(text,
                                                  frame.f_lineno,
                                                  target_name=name)
                        return
            frame = frame.f_back


def analyze(source_code):
    tracer = CodeTracer()
    tracer.max_width = 200000
    code_report = tracer.trace_code(source_code)
    return code_report


def web_main():
    window.analyze = analyze


def main():
    parser = argparse.ArgumentParser(
        description='Trace Python code.',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-c',
                        '--canvas',
                        action='store_true',
                        help='Should canvas commands be printed?')
    parser.add_argument('-x',
                        '--width',
                        type=int,
                        default=800,
                        help='width of the canvas in pixels')
    parser.add_argument('-y',
                        '--height',
                        type=int,
                        default=600,
                        help='height of the canvas in pixels')
    parser.add_argument('-z',
                        '--zoomed',
                        action='store_true',
                        help='matplotlib is zoomed to fit the canvas size')
    parser.add_argument('--source_width',
                        type=int,
                        help='Width of source code - use 0 to hide or '
                             'negative numbers to trim columns from the end, '
                             'None to fit source code.')
    parser.add_argument('-n',
                        '--source_indent',
                        type=int,
                        default=0,
                        help='Number of spaces to indent source code. '
                             'Negative to skip first columns of source code.')
    parser.add_argument('-b',
                        '--bad_driver',
                        help="message to display if driver doesn't call module")
    parser.add_argument('-i',
                        '--input',
                        help="file to redirect stdin from")
    parser.add_argument('--traced_file',
                        help='file to replace with source code from stdin')
    parser.add_argument('--traced',
                        help='module, function, or method to display trace '
                             'for. Default: %%(default)s to trace %s or %s.' %
                             (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME))
    parser.add_argument('--live',
                        action='store_true',
                        help='load main module as %s instead of %s.' %
                             (LIVE_MODULE_NAME, DEFAULT_MODULE_NAME))
    parser.add_argument('-m',
                        dest='module',
                        action='store_true',
                        help='driver is an importable module, not a script')
    parser.add_argument('driver',
                        nargs=argparse.REMAINDER,
                        help='script to call traced code, plus any arguments')
    args = parser.parse_args()
    if args.driver and args.driver[0] in ('-m', '--module'):
        args.module = True
        args.driver = args.driver[1:]

    code = None
    if args.traced_file is not None:
        code = sys.stdin.read()
    elif args.traced in (None, DEFAULT_MODULE_NAME, LIVE_MODULE_NAME):
        if args.driver:
            if args.module:
                args.traced_file = find_module_path(args.driver[0])
            else:
                args.traced_file = args.driver[0]
            with open(args.traced_file) as source_file:
                code = source_file.read()
    if args.traced in (None, DEFAULT_MODULE_NAME, LIVE_MODULE_NAME):
        args.traced = LIVE_MODULE_NAME if args.live else DEFAULT_MODULE_NAME

    canvas = Canvas(args.width, args.height)
    tracer = CodeTracer(canvas)
    tracer.max_width = 200000
    code_report = tracer.trace_code(code,
                                    source_width=args.source_width,
                                    source_indent=args.source_indent,
                                    traced=args.traced,
                                    is_module=args.module,
                                    driver=args.driver,
                                    traced_file=args.traced_file,
                                    stdin=args.input,
                                    bad_driver=args.bad_driver,
                                    is_zoomed=args.zoomed)
    turtle_report = MockTurtle.get_all_reports()
    if turtle_report and args.canvas:
        print('start_canvas')
        print('\n'.join(turtle_report))
        print('end_canvas')
        print('.')
    print(code_report)
    if tracer.return_code:
        exit(tracer.return_code)


def find_module_path(module_name):
    if find_spec is not None:
        # noinspection PyCallingNonCallable
        spec = find_spec(module_name)
        return spec and spec.origin
    try:
        source_file, pathname, description = imp.find_module(module_name)
        source_file.close()
        return pathname
    except ImportError:
        return None


if __name__ == '__main__':
    main()
elif IS_PYODIDE:
    web_main()
