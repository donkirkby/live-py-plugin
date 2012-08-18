class Canvas:
    def __init__(self, width=0, height=0):
        self.options = {'width': width,
                        'height': height}
        self.report = []
        def make_call(method_name):
            return lambda *args, **kwargs: self.call(method_name, 
                                                     *args, 
                                                     **kwargs)

        method_names = ('create_line', 'create_rectangle')
        for method_name in method_names:
            self.__dict__[method_name] = make_call(method_name)
        
    def call(self, method_name, *args, **kwargs):
        self.report.append("%s(%s)" % (method_name, 
                                       ', '.join([str(x) for x in args])))
    
    def cget(self, option):
        return self.options[option]
    
#echo off
#echo width 50
if __name__ == '__live_coding__':
    canvas = Canvas(800, 600)
    canvas.create_line(1, 2, 100, 200)
    canvas.create_rectangle(5, 10, 500, 1000)
    report = canvas.report
