""" Wrap syntax that's specific to Python 2's exec statement.
To use this method, import it like this:
try:
    from exec_python2 import exec_code #@UnusedImport
except:
    from exec_python3 import exec_code #@Reimport
"""
def exec_code(source, global_vars, local_vars):
    """ Execute source using global_vars as the global names and 
    local_vars as the local names. source can be a string or code object.
    """
    exec source in global_vars, local_vars