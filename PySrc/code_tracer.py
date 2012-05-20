import copy
import sys

class CodeTracer(object):
    def dump_frame(self, frame, event, arg):
        i = self.previous_line - 1
        if i >= 0:
            while i >= len(self.report):
                self.report.append("")
            for k, v in frame.f_locals.iteritems():
                if self.locals.get(k) != v:
                    self.report[i] += "%s = %r " % (k, v)
                    self.locals[k] = copy.deepcopy(v)
            
        if frame.f_lineno < self.previous_line:
            # must have started a new loop iteration
            width = 0
            for j in range(frame.f_lineno-1, self.previous_line):
                width = max(width, len(self.report[j]))
            for j in range(frame.f_lineno-1, self.previous_line):
                self.report[j] = self.report[j].ljust(width) + '| '
        self.previous_line = frame.f_lineno
        return self.dump_frame
    
    def trace_code(self, code):
        original_trace = sys.gettrace()
        self.previous_line = 0
        self.report = []
        self.locals = dict()
        
        sys.settrace(self.dump_frame)
        exec code in dict(), dict()
        
        sys.settrace(original_trace)
        return '\n'.join(self.report)
    
if __name__ == '__main__':
    code = sys.stdin.read()
    
    print CodeTracer().trace_code(code)