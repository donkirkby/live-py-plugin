from ast import (arg, fix_missing_locations, iter_fields, Add, Assign,
                 AST, Attribute, BitAnd, BitOr, BitXor, Call, Div, Ellipsis,
                 ExceptHandler, Expr, ExtSlice, FloorDiv, ImportFrom, Index,
                 List, Load, LShift, Mod, Mult, Name, NodeTransformer, Num,
                 Pow, Raise, Return, RShift, Slice, Starred, Store, Str, Sub,
                 Subscript, Try, Tuple, Yield)
from copy import deepcopy

try:
    from ast import FormattedValue, Constant
except ImportError:
    # Not available in Python 3.5
    FormattedValue = Constant = None

try:
    from ast import MatchAs, MatchSequence, MatchStar
except ImportError:
    # Not available before Python 3.10
    MatchAs = MatchSequence = MatchStar = None

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
        slice_text, next_index = self._wrap_slice(subscript, index_to_get)
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

    def _wrap_slice(self, subscript_node, index_to_get=None):
        """ Wrap a slice in calls to assignment index methods.

        Also build a format string for the slice.
        @param subscript_node: the subscript that owns the slice
        @param index_to_get: if this is None, wrap in add_assignment_index(),
            otherwise, wrap in get_assignment_index(index_to_get).
        @return: format_text, next_index_to_get
        """
        slice_node = subscript_node.slice
        if isinstance(slice_node, (Index, Ellipsis)):
            if (isinstance(slice_node, Ellipsis) or
                    isinstance(slice_node.value, Ellipsis)):
                index_to_get = None
                format_text = '...'
            else:
                slice_node.value = self._wrap_assignment_index(
                    slice_node.value,
                    index_to_get)
                format_text = '{!r}'
                if index_to_get is not None:
                    index_to_get -= 1
        elif Constant is not None and isinstance(slice_node, (Constant, Name)):
            # Python 3.9 stopped wrapping the constant in an Index object.
            subscript_node.slice = self._wrap_assignment_index(
                slice_node,
                index_to_get)
            format_text = '{!r}'
            if index_to_get is not None:
                index_to_get -= 1
        elif isinstance(slice_node, Slice):
            index_to_get = None
            if slice_node.step is None:
                step_text = ''
            else:
                step_text = ':{!r}'
                slice_node.step = self._wrap_assignment_index(
                    slice_node.step,
                    index_to_get)
            if slice_node.upper is None:
                upper_text = ''
            else:
                upper_text = '{!r}'
                slice_node.upper = self._wrap_assignment_index(
                    slice_node.upper,
                    index_to_get)
            if slice_node.lower is None:
                lower_text = ''
            else:
                lower_text = '{!r}'
                slice_node.lower = self._wrap_assignment_index(
                    slice_node.lower,
                    index_to_get)
            format_text = '{}:{}{}'.format(lower_text, upper_text, step_text)
        elif isinstance(slice_node, ExtSlice):
            index_to_get = None
            subscripts = [Subscript(slice=element)
                          for element in slice_node.dims]
            format_text = ', '.join(self._wrap_slice(subscript)[0]
                                    for subscript in subscripts)
        else:
            assert isinstance(slice_node, Tuple), slice_node
            # Python 3.9 replaced ExtSlice with a Tuple.
            index_to_get = None
            subscripts = [Subscript(slice=element)
                          for element in slice_node.elts]
            format_text = ', '.join(self._wrap_slice(subscript)[0]
                                    for subscript in subscripts)
            slice_node.elts = [subscript.slice for subscript in subscripts]
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

        func_args = existing_node.args + [kwarg.value
                                          for kwarg in existing_node.keywords]
        for arg_node in func_args:
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
        if existing_node.value is None:
            return existing_node
        line_numbers = set()
        find_line_numbers(existing_node, line_numbers)
        first_line_number = min(line_numbers)
        last_line_number = max(line_numbers)
        if len(targets) == 1 and isinstance(targets[0], Name):
            existing_node.value = self._create_bare_context_call(
                'assign',
                [Str(s=targets[0].id),
                 existing_node.value,
                 Num(n=first_line_number)])
            return existing_node
        new_nodes = []
        format_string = self._wrap_assignment_targets(targets)
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
        self._create_end_assignment(new_nodes,
                                    try_body,
                                    first_line_number,
                                    last_line_number)

        return new_nodes

    def visit_AnnAssign(self, node):
        # noinspection PyTypeChecker
        return self.visit_Assign(node)

    def visit_NamedExpr(self, node):
        # noinspection PyTypeChecker
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
        self._create_end_assignment(new_nodes,
                                    try_body,
                                    first_line_number,
                                    last_line_number)

        return new_nodes

    def _create_end_assignment(self,
                               new_nodes,
                               try_body,
                               first_line_number,
                               last_line_number):
        end_assignment = self._create_context_call('end_assignment')
        finally_body = [end_assignment]
        new_nodes.append(Try(body=try_body,
                             finalbody=finally_body,
                             handlers=[],
                             orelse=[],
                             lineno=first_line_number))
        self._set_statement_line_numbers(try_body, first_line_number)
        self._set_statement_line_numbers(finally_body, last_line_number)

    def visit_Match(self, node):
        match_node = self.generic_visit(node)
        line_numbers = set()
        find_line_numbers(match_node, line_numbers)
        first_line_number = min(line_numbers)
        new_nodes = [self._create_context_call('start_assignment')]
        try_body = [match_node]
        finally_body = [self._create_context_call('end_assignment')]
        new_nodes.append(Try(body=try_body,
                             finalbody=finally_body,
                             handlers=[],
                             orelse=[],
                             lineno=first_line_number))
        match_node.subject = self._create_bare_context_call(
            'set_assignment_value',
            [match_node.subject])
        for case in match_node.cases:
            try:
                format_string = self._wrap_assignment_targets((case.pattern,))
            except ValueError:
                # Not a capture pattern, nothing to report.
                format_string = None
            if format_string is not None:
                case.body.insert(0,
                                 self._create_context_call(
                                     'report_assignment',
                                     [Str(s=format_string),
                                      Num(n=case.pattern.lineno)]))
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

    def visit_With(self, node):
        new_node = self.generic_visit(node)

        # Collect line numbers from all except else block.
        line_numbers = set()
        for item in new_node.items:
            find_line_numbers(item, line_numbers)
        for statement in new_node.body:
            find_line_numbers(statement, line_numbers)
        line_numbers.add(new_node.lineno)
        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        new_body = [self._create_context_call('start_block', args)]
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
            # noinspection PyUnresolvedReferences
            new_node.body.insert(0, try_body.pop(0))

        # trace function parameter values
        arg_nodes = []
        arg_nodes.extend(getattr(new_node.args, 'posonlyargs', []))
        arg_nodes.extend(new_node.args.args)
        arg_nodes.append(new_node.args.kwarg)
        arg_nodes.append(new_node.args.vararg)
        arg_nodes.extend(new_node.args.kwonlyargs)
        for target in arg_nodes:
            if target is None:
                continue
            if isinstance(target, Name) and target.id == 'self':
                continue
            if isinstance(target, arg) and target.arg == 'self':
                continue
            new_node.body.append(self._trace_assignment(target, node.lineno))

        if try_body:
            handler_body = [self._create_context_call('exception'),
                            Raise()]
            new_node.body.append(
                Try(body=try_body,
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
            new_body.append(Try(body=try_body,
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
            if Constant is None:
                value = Name(id='None', ctx=Load())
            else:
                value = Constant(value=None)

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
            try:
                trace = self._trace_assignment(targets)
            except TypeError:
                trace = None
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
        elif isinstance(target, Starred):
            arg_name = target.value.id
        elif isinstance(target, Attribute):
            args = [Str(s='{}.{}'.format(target.value.id, target.attr)),
                    Attribute(value=Name(id=target.value.id, ctx=Load()),
                              attr=target.attr,
                              ctx=Load()),
                    Num(n=lineno)]
            return self._create_context_call('assign', args)
        elif isinstance(target, str):
            arg_name = target
        else:
            message = 'Cannot trace assignment to a {}.'.format(type(target))
            raise TypeError(message)

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
        if MatchAs is not None and isinstance(target, MatchAs):
            return target.name
        if MatchSequence is not None and isinstance(target, MatchSequence):
            return self._wrap_assignment_sequence(target.patterns)
        if isinstance(target, Subscript):
            return self._wrap_subscript_target(target, index_to_get)
        if isinstance(target, Tuple) or isinstance(target, List):
            return self._wrap_assignment_sequence(target.elts)
        if Starred is not None and isinstance(target, Starred):
            return '*{}'.format(target.value.id)
        if MatchStar is not None and isinstance(target, MatchStar):
            return '*{}'.format(target.name)
        if not isinstance(target, Attribute):
            raise ValueError('Assignment target had type {}.'.format(
                type(target)))
        names = self._get_attribute_names(target)
        return '.'.join(names)

    def _wrap_assignment_sequence(self, targets):
        target_names = map(self._wrap_assignment_target, targets)
        wrapped = '({})'.format(', '.join(target_names))
        if len(targets) == 1:
            wrapped = wrapped[:-1] + ',)'
        return wrapped

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


def trace_source_tree(source_tree):
    source_tree = Tracer().visit(source_tree)
    fix_missing_locations(source_tree)
    LineNumberCleaner().visit(source_tree)
    return source_tree
