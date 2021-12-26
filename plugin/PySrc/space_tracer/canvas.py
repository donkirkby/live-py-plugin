class Canvas(object):
    def __init__(self, width=0, height=0):
        self.options = {'width': width,
                        'height': height,
                        'bg': 'white'}
        self.report = []
        self.is_recording = True

        def make_call(method_name):
            return lambda *args, **kwargs: self.call(method_name,
                                                     *args,
                                                     **kwargs)

        method_names = ('create_line',
                        'create_rectangle',
                        'create_polygon',
                        'create_text',
                        'create_image')
        for name in method_names:
            self.__dict__[name] = make_call(name)

    def call(self, method_name, *args, **kwargs):
        if not self.is_recording:
            return
        self.report.append(method_name)
        if method_name == 'create_polygon':
            args = args[0]
        for arg in args:
            self.report.append("    %r" % int(round(arg)))
        keys = list(kwargs.keys())
        keys.sort()
        for key in keys:
            value = kwargs[key]
            self.report.append("    %s=%r" % (key, value))

    def cget(self, option):
        return self[option]

    def __getitem__(self, item):
        return self.options[item]

    def winfo_width(self):
        return self['width']

    def winfo_height(self):
        return self['height']

    def config(self, **kwargs):
        self.options.update(kwargs)

    def delete(self, item):
        pass

    def update(self):
        pass

    def unbind(self, *args, **kwargs):
        pass

    def after(self, delay):
        pass

    def tag_raise(self, item):
        pass
