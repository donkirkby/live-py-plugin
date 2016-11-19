class Canvas(object):
    def __init__(self, width=0, height=0):
        self.options = {'width': width,
                        'height': height}
        self.report = []

        def make_call(method_name):
            return lambda *args, **kwargs: self.call(method_name,
                                                     *args,
                                                     **kwargs)

        method_names = ('create_line',
                        'create_rectangle',
                        'create_polygon',
                        'create_text')
        for method_name in method_names:
            self.__dict__[method_name] = make_call(method_name)

    def call(self, method_name, *args, **kwargs):
        self.report.append(method_name)
        for arg in args:
            self.report.append("    %r" % int(round(arg)))
        keys = list(kwargs.keys())
        keys.sort()
        for key in keys:
            value = kwargs[key]
            self.report.append("    %s=%r" % (key, value))

    def cget(self, option):
        return self.options[option]
