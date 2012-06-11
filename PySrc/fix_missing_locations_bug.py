from ast import (dump, fix_missing_locations, parse, AST,
                 Call, Expr, Load, Name)

def iter_fields(node):
    """
    Yield a tuple of ``(fieldname, value)`` for each field in ``node._fields``
    that is present on *node*.
    """
    for field in node._fields:
        try:
            yield field, getattr(node, field)
        except AttributeError:
            pass


def iter_child_nodes(node):
    """
    Yield all direct child nodes of *node*, that is, all fields that are nodes
    and all items of fields that are lists of nodes.
    """
    for _, field in iter_fields(node):
        if isinstance(field, AST):
            yield field
        elif isinstance(field, list):
            for item in field:
                if isinstance(item, AST):
                    yield item

def fix_missing_locations2(node):
    """
    When you compile a node tree with compile(), the compiler expects lineno and
    col_offset attributes for every node that supports them.  This is rather
    tedious to fill in for generated nodes, so this helper adds these attributes
    recursively where not already set, by setting them to the values of the
    parent node.  It works recursively starting at *node*.
    """
    def _fix(node, lineno, col_offset):
        if 'lineno' in node._attributes:
            if not hasattr(node, 'lineno'):
                node.lineno = lineno
                print '%r.lineno = %d' % (node, lineno)
            else:
                lineno = node.lineno
                print '%r.lineno is %d' % (node, lineno)
        if 'col_offset' in node._attributes:
            if not hasattr(node, 'col_offset'):
                node.col_offset = col_offset
            else:
                col_offset = node.col_offset
        for child in iter_child_nodes(node):
            _fix(child, lineno, col_offset)
    _fix(node, 1, 0)
    return node

def raise_error(is_error):
    if is_error:
        raise RuntimeError('an error')
    
code = """\
while True:

    j = 1
    raise_error(True)
"""

tree = parse(code)
old_node, old_call = tree.body[0].body
new_call = Expr(value=Call(func=Name(id='raise_error', ctx=Load()),
                              args=[Name(id='False', ctx=Load())],
                              keywords=[],
                              starargs=None,
                              kwargs=None), lineno=5, col_offset=100)

tree.body[0].body = [old_node, new_call, old_call]
print dump(tree)
fix_missing_locations2(tree)

code = compile(tree, '<source string>', 'exec')

env = {'raise_error': raise_error}

exec code in env
