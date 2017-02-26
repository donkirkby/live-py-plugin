import argparse
from ast import (fix_missing_locations, iter_fields, parse, Add, Assign, AST,
                 Attribute, BinOp, BitAnd, BitOr, BitXor, Call, Div, Ellipsis,
                 ExceptHandler, Expr, ExtSlice, FloorDiv, ImportFrom, Index,
                 List, Load, LShift, Mod, Mult, Name, NodeTransformer, Num,
                 Pow, Raise, Return, RShift, Slice, Store, Str, Sub, Subscript,
                 Tuple, Yield)
from contextlib import contextmanager
from copy import deepcopy
import imp
from pydoc import locate
import sys
import traceback
import types
import os
import importlib

from canvas import Canvas
from mock_turtle import MockTurtle
from report_builder import ReportBuilder

# Import some classes that are only available in Python 3.
arg = locate('ast.arg')
Starred = locate('ast.Starred')
FormattedValue = locate('ast.FormattedValue')
TryExcept = locate('ast.TryExcept')
TryFinally = locate('ast.TryFinally')
if TryExcept is None:
    # Make Python 3.3 try class compatible with old versions.
    TryFinally = TryExcept = locate('ast.Try')

izip_longest = (locate('itertools.izip_longest') or
                locate('itertools.zip_longest'))

CONTEXT_NAME = '__live_coding_context__'
RESULT_NAME = '__live_coding_result__'
PSEUDO_FILENAME = '<live coding source>'
SCOPE_NAME = '__live_coding__'

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


class Tracer(NodeTransformer):

    def _set_statement_line_numbers(self,
                                    statements,
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
                self._find_line_numbers(statement, line_numbers)
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

    def _trace_print_function(self, existing_node):
        starargs = getattr(existing_node, 'starargs', None)
        values = []
        formats = []
        for a in existing_node.args:
            if Starred is not None and isinstance(a, Starred):
                starargs = a.value
            else:
                values.append(a)
                formats.append('%r')
        if starargs is not None:
            values.append(starargs)
            formats.append('*%r')
        for keyword in existing_node.keywords:
            values.append(keyword.value)
            formats.append('{}=%r'.format(keyword.arg))
        message_format = 'print(' + ', '.join(formats) + ') '
        return self._create_bare_context_call('add_message',
                                              [BinOp(left=Str(message_format),
                                                     op=Mod(),
                                                     right=Tuple(elts=values,
                                                                 ctx=Load())),
                                               Num(existing_node.lineno)])

    def _get_attribute_names(self, attribute_node):
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
        if isinstance(func_node, Name) and func_node.id == 'print':
            return self._trace_print_function(existing_node)

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
                Call(func=Name(id='repr', ctx=Load()),
                     args=[node],
                     keywords=[],
                     starargs=None,
                     kwargs=None))
            args[3].elts.append(  # repr() after
                Call(func=Name(id='repr', ctx=Load()),
                     args=[node],
                     keywords=[],
                     starargs=None,
                     kwargs=None))
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

    def visit_Print(self, node):
        existing_node = self.generic_visit(node)
        values = existing_node.values
        message_format = 'print' + ','.join([' %r']*len(values)) + ' '
        return self._create_context_call('add_message',
                                         [BinOp(left=Str(message_format),
                                                op=Mod(),
                                                right=Tuple(elts=values,
                                                            ctx=Load())),
                                          Num(existing_node.lineno)])

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
        if any(map(self._is_untraceable_attribute, existing_node.targets)):
            return existing_node
        line_numbers = set()
        self._find_line_numbers(existing_node, line_numbers)
        first_line_number = min(line_numbers)
        last_line_number = max(line_numbers)
        new_nodes = []
        format_string = self._wrap_assignment_targets(
            existing_node.targets)
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

    def visit_AugAssign(self, node):
        read_target = deepcopy(node.target)
        existing_node = self.generic_visit(node)
        line_numbers = set()
        self._find_line_numbers(existing_node, line_numbers)
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
                format_string += ' {}= {{!r}} '.format(operator_char)
            else:
                self._wrap_assignment_target(read_target, index_to_get=-1)
                read_target.ctx = Load()
                set_assignment_value = self._create_context_call(
                    'set_assignment_value',
                    [read_target])
                try_body.append(set_assignment_value)
                format_string += ' = {!r}'
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

    def _find_line_numbers(self, node, line_numbers):
        """ Populates a set containing all line numbers used by the node and its
        descendants.

        line_numbers is a set that all the line numbers will be added to."""
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
                        self._find_line_numbers(item, line_numbers)
            elif isinstance(value, AST):
                self._find_line_numbers(value, line_numbers)

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

    def visit_While(self, node):
        new_node = self.generic_visit(node)

        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)
        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        new_node.body.insert(0,
                             self._create_context_call('start_block', args))
        return new_node

    def visit_FunctionDef(self, node):
        """ Instrument a function definition by creating a new report builder
        for this stack frame and putting it in a local variable. The local
        variable has the same name as the global variable so all calls can
        use the same CONTEXT_NAME symbol, but it means that I had to use this:
        x = globals()['x'].start_frame()
        Kind of ugly, but I think it was worth it to handle recursive calls.
        """
        if node.name == '__repr__':
            return node

        new_node = self.generic_visit(node)

        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)
        first_line_number = min(line_numbers)
        last_line_number = max(line_numbers)
        args = [Num(n=first_line_number),
                Num(n=last_line_number)]
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
                                keywords=[],
                                starargs=None,
                                kwargs=None)
        context_assign = Assign(targets=[Name(id=CONTEXT_NAME, ctx=Store())],
                                value=start_frame_call)
        new_node.body = [context_assign]
        if isinstance(try_body[0], Expr) and isinstance(try_body[0].value, Str):
            # Move docstring back to top of function.
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

    def _is_module_header(self, statement):
        if isinstance(statement, ImportFrom):
            return statement.module == '__future__'
        if isinstance(statement, Expr):
            return isinstance(statement.value, Str)
        return False

    def visit_Module(self, node):
        new_node = self.generic_visit(node)
        try_body = new_node.body
        if try_body:
            new_body = []
            while try_body and self._is_module_header(try_body[0]):
                new_body.append(try_body.pop(0))
            line_numbers = set()
            self._find_line_numbers(new_node, line_numbers)
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

    def visit_Lambda(self, node):
        """ Instrument a lambda expression by displaying the parameter values.

        We create calls to trace assignment to each argument, then wrap them
        all in a tuple together with the original expression, and pull the
        original expression out of the tuple.
        """

        new_node = self.generic_visit(node)

        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)

        # trace lambda argument values
        calls = [getattr(self._trace_assignment(target, node.lineno),
                         'value',
                         None)
                 for target in new_node.args.args
                 if getattr(target, 'id', 'self') != 'self' or
                 getattr(target, 'arg', 'self') != 'self']

        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        calls.insert(0, self._create_context_call('start_block', args).value)
        calls.append(new_node.body)
        new_node.body = Subscript(value=Tuple(elts=calls,
                                              ctx=Load()),
                                  slice=Index(value=Num(n=-1)),
                                  ctx=Load())
        return new_node

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
        return existing_node

    def visit_Try(self, node):
        # Python 3.3 renamed TryExcept and TryFinally to Try
        return self.visit_TryExcept(node)

    def visit_Yield(self, node):
        existing_node = self.generic_visit(node)
        value = existing_node.value
        if value is None:
            value = Name(id='None', ctx=Load())

        return Yield(value=self._create_bare_context_call(
                    'yield_value',
                    [value, Num(n=existing_node.lineno)]))

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
            # todo wasn't iterable, treat it as a single item
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
            return '({})'.format(', '.join(target_names))
        assert_message = 'Assignment target had type {}.'.format(type(target))
        assert isinstance(target, Attribute), assert_message
        names = self._get_attribute_names(target)
        return '.'.join(names)

    def _wrap_assignment_targets(self, targets):
        """ Build string describing assignment targets and wrap indexes.

        For example, "x = {!r}" for a single target, "x = y = {!r}" for
        multiple targets, or "x[{!r}] = {!r} for an indexed target.
        @return: string, or None if no assignment can be reported.
        """
        strings = []
        for target in targets:
            format_text = self._wrap_assignment_target(target)
            if format_text is not None:
                strings.append(format_text)
        if not strings:
            return None
        strings.append('{!r}')  # for assignment value
        return ' = '.join(strings)

    def _create_context_call(self, function_name, args=None):
        " Create a method call expression on the live coding context object. "
        return Expr(value=self._create_bare_context_call(function_name, args))

    def _create_bare_context_call(self, function_name, args=None):
        """ Create a method call on the live coding context object.

        Bare means that it is not wrapped in an expression. """
        if args is None:
            args = []
        context_name = Name(id=CONTEXT_NAME, ctx=Load())
        function = Attribute(value=context_name,
                             attr=function_name,
                             ctx=Load())
        return Call(func=function,
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


class CodeTracer(object):
    def __init__(self, canvas=None):
        self.message_limit = 10000
        self.max_width = None
        self.keepalive = False
        MockTurtle.monkey_patch(canvas)
        self.environment = {}

    def run_python_module(self, modulename, args):
        """Run a python module, as though with ``python -m name args...``.

        `modulename` is the name of the module, possibly a dot-separated name.
        `args` is the argument array to present as sys.argv, including the
        first element naming the module being executed.

        This is based on code from coverage.py, by Ned Batchelder.
        https://bitbucket.org/ned/coveragepy
        """
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
            openfile, pathname, _ = imp.find_module(name, searchpath)

            # If `modulename` is actually a package, not a mere module,
            # then we pretend to be Python 2.7 and try running its
            # __main__.py script.
            if openfile is None:
                packagename = modulename
                name = '__main__'
                package = __import__(packagename, glo, loc, ['__path__'])
                searchpath = package.__path__
                openfile, pathname, _ = imp.find_module(name, searchpath)
        finally:
            if openfile:
                openfile.close()

        # Finally, hand the file off to run_python_file for execution.
        pathname = os.path.abspath(pathname)
        args[0] = pathname
        self.run_python_file(pathname, args, package=packagename)

    def run_python_file(self, filename, args, package=None):
        """Run a python file as if it were the main program on the command line.

        `filename` is the path to the file to execute.
        `args` is the argument array to present as sys.argv, including the
        first element naming the file being executed. `package` is the name of
        the enclosing package, if any.
        """
        # Create a module to serve as __main__
        old_main_mod = sys.modules['__main__']
        main_mod = imp.new_module('__main__')
        sys.modules['__main__'] = main_mod
        main_mod.__file__ = filename
        if package:
            main_mod.__package__ = package

        # Set sys.argv properly.
        old_argv = sys.argv
        sys.argv = args

        try:
            code = self.make_code_from_py(filename)

            # Execute the code object.
            exec(code, main_mod.__dict__)
        finally:
            # Restore the old __main__
            sys.modules['__main__'] = old_main_mod

            # Restore the old argv and path
            sys.argv = old_argv

    def make_code_from_py(self, filename):
        """Get source from `filename` and make a code object of it."""
        with open(filename, 'rU') as f:
            source = f.read()

        code = compile(source, filename, "exec")

        return code

    @contextmanager
    def swallow_output(self):
        old_stdout = sys.stdout
        old_stderr = sys.stderr
        try:
            sys.stdout = FileSwallower(old_stdout)
            sys.stderr = FileSwallower(old_stderr)
            yield
        finally:
            sys.stdout = old_stdout
            sys.stderr = old_stderr

    def run_instrumented_module(self, code, module_name, filename):
        new_mod = imp.new_module(module_name)
        sys.modules[module_name] = new_mod
        if filename is not None:
            new_mod.__file__ = filename
        if '.' in module_name:
            package_name, child_name = module_name.rsplit('.', 1)
            importlib.import_module(package_name)
            setattr(sys.modules[package_name], child_name, new_mod)
        else:
            package_name = None
        new_mod.__package__ = package_name

        new_mod.__dict__.update(self.environment)
        self.environment = new_mod.__dict__

        with self.swallow_output():
            exec(code, self.environment)
            
    def split_lines(self, messages):
        for message in messages:
            for line in message.splitlines():
                yield line

    def trace_turtle(self, source):
        exec(source, self.environment, self.environment)

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
                   load_as=None,
                   module=False,
                   dump=False,
                   driver=None,
                   filename=None):
        """ Trace a module of source code, possibly by running a driver script.
        
        :param str source: the source code to trace
        :param str load_as: the module name to load the source code as
        :param bool module: True if the driver is a module name instead of a
        file name
        :param bool dump: True if the source code should be included in the
        output
        :param str driver: the driver script's file name or module name
        :param str filename: the file name of the source code
        """
        builder = ReportBuilder(self.message_limit)
        builder.max_width = self.max_width
        self.return_code = 0

        try:
            tree = parse(source)

            new_tree = Tracer().visit(tree)
            fix_missing_locations(new_tree)
            LineNumberCleaner().visit(new_tree)
#             from ast import dump
#             print(dump(new_tree, include_attributes=False))
            code = compile(new_tree, PSEUDO_FILENAME, 'exec')

            self.environment[CONTEXT_NAME] = builder
            self.run_instrumented_module(code, load_as or SCOPE_NAME, filename)
            if driver:
                with self.swallow_output():
                    if module:
                        module_name = driver[0]
                        try:
                            self.run_python_module(module_name, driver)
                        except SystemExit as ex:
                            if ex.code:
                                if module_name != 'unittest':
                                    raise
                                self.return_code = ex.code
                                result = (sys.stderr.last_line or
                                          'FAIL        ')
    
                                result = 'unittest: ' + result
                                self.report_driver_result(builder, [result])
                    else:
                        self.run_python_file(driver[0], driver)
            for value in self.environment.values():
                if isinstance(value, types.GeneratorType):
                    value.close()
        except SyntaxError:
            self.return_code = 1
            ex = sys.exc_info()[1]
            messages = traceback.format_exception_only(type(ex), ex)
            builder.add_message(messages[-1].strip() + ' ', ex.lineno)
        except BaseException as ex:
            self.return_code = getattr(ex, 'code', 1)
            etype, value, tb = sys.exc_info()
            is_reported = False
            messages = traceback.format_exception_only(etype, value)
            entries = traceback.extract_tb(tb)
            for filename, _, _, _ in entries:
                if filename == PSEUDO_FILENAME:
                    is_reported = True
            while not is_reported and tb is not None:
                frame = tb.tb_frame
                code = frame.f_code
                filename = code.co_filename
                if __file__ not in (filename, filename + 'c'):
                    break
                tb = tb.tb_next
            if not is_reported:
                if tb:
                    messages = traceback.format_exception(etype, value, tb)
                else:
                    messages = traceback.format_exception_only(etype, value)
                self.report_driver_result(builder, messages)

        report = builder.report()
        if dump:
            source_lines = source.splitlines()
            report_lines = report.splitlines()
            dump_lines = []
            source_width = max(map(len, source_lines))
            indent = 4
            for source_line, report_line in izip_longest(source_lines,
                                                         report_lines,
                                                         fillvalue=''):
                line = (indent * ' ' + source_line +
                        (source_width-len(source_line))*' ' +
                        ' | ' + report_line)
                dump_lines.append(line)
            report = '\n'.join(dump_lines)

        return report


class FileSwallower(object):
    def __init__(self, target):
        self.target = target
        self.last_line = None

    def write(self, *args, **kwargs):
        text = args and str(args[0]) or ''
        lines = text.strip().splitlines()
        if lines:
            self.last_line = lines[-1]

    def __getattr__(self, name):
        return getattr(self.target, name)


def main():
    parser = argparse.ArgumentParser(description='Trace Python code.')
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
    parser.add_argument('-d',
                        '--dump',
                        action='store_true',
                        help='dump source code with report')
    parser.add_argument('-f',
                        '--filename',
                        help='file name to save in __file__')
    parser.add_argument('-m',
                        '--module',
                        action='store_true',
                        help='driver is an importable module, not a script')
    parser.add_argument('source',
                        nargs='?',
                        default='-',
                        help='source file to trace, or - for stdin')
    parser.add_argument('load_as',
                        nargs='?',
                        help='load traced code as a module with this name')
    parser.add_argument('driver',
                        nargs=argparse.REMAINDER,
                        help='script to call traced code, plus any arguments')

    args = parser.parse_args()
    if args.driver and args.driver[0] in ('-m', '--module'):
        args.module = True
        args.driver = args.driver[1:]
    if args.source == '-':
        code = sys.stdin.read()
    else:
        with open(args.source, 'rU') as source:
            code = source.read()
    canvas = Canvas(args.width, args.height)
    tracer = CodeTracer(canvas)
    tracer.max_width = 200000
    code_report = tracer.trace_code(code,
                                    dump=args.dump,
                                    load_as=args.load_as,
                                    module=args.module,
                                    driver=args.driver,
                                    filename=args.filename)
    turtle_report = MockTurtle.get_all_reports()
    if turtle_report and args.canvas:
        print('start_canvas')
        print('\n'.join(turtle_report))
        print('end_canvas')
        print('.')
    print(code_report)
    if tracer.return_code:
        exit(tracer.return_code)

if __name__ == '__main__':
    main()
