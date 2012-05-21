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
                    self.log.append("%d: %s = %r" % (self.previous_line, k, v))
                    self.locals[k] = copy.deepcopy(v)
            
        self.log.append("%d: %s %r" % (frame.f_lineno, event, arg))
        if frame.f_lineno < self.previous_line:
            # must have started a new loop iteration
            loop_start = frame.f_lineno - 1
            loop_bound = self.previous_line
            iteration = self.report[loop_start].count('|')
            blank = ''.join([(c if c == '|' else ' ') 
                             for c in self.report[loop_start]])
            blank = blank.rstrip() + ' '
            
            width = 0
            for j in range(loop_start, loop_bound):
                if self.report[j].count('|') < iteration:
                    self.report[j] = blank + self.report[j]
                width = max(width, len(self.report[j]))
            for j in range(loop_start, loop_bound):
                self.report[j] = self.report[j].ljust(width) + '| '
        self.previous_line = frame.f_lineno
        return self.dump_frame
    
    def trace_code(self, code):
        original_trace = sys.gettrace()
        self.previous_line = 0
        self.report = []
        self.log = []
        self.locals = dict()
        
        sys.settrace(self.dump_frame)
        exec code in dict(), dict()
        
        sys.settrace(original_trace)
        return '\n'.join(self.report)
    
if __name__ == '__main__':
    code = sys.stdin.read()
    
    print CodeTracer().trace_code(code)