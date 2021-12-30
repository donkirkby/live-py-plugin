from operator import itemgetter


class Canvas(object):
    def __init__(self, width=0, height=0):
        self.options = {'width': width,
                        'height': height,
                        'bg': 'white'}
        self.items = []
        self.max_zorder = 0

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
        if method_name == 'create_polygon':
            args = args[0]
        item = kwargs.copy()
        item['coords'] = args
        item['method_name'] = method_name
        item['zorder'] = self.max_zorder
        item['deleted'] = False
        item_id = len(self.items)
        self.items.append(item)
        return item_id

    def build_report(self):
        report = []
        xoff = self.winfo_width() / 2
        yoff = self.winfo_height() / 2
        for item_details in sorted(self.items, key=itemgetter('zorder')):
            copy_details = item_details.copy()
            method_name = item_details['method_name']
            coords = list(item_details['coords'])
            if method_name == 'create_polygon':
                copy_details['outline'] = ''
                try:
                    del copy_details['width']
                except KeyError:
                    pass
            for i in range(0, len(coords), 2):
                x, y = coords[i:i+2]
                x = x + xoff
                y = y + yoff
                coords[i] = x
                coords[i+1] = y
            if method_name != 'create_line':
                copy_details['coords'] = coords
                build_item_report(copy_details, report)
            if method_name in ('create_line', 'create_polygon'):
                copy_details = item_details.copy()
                if method_name == 'create_polygon':
                    copy_details['method_name'] = 'create_line'
                    coords.extend(coords[:2])
                try:
                    outline = copy_details.pop('outline')
                    copy_details['fill'] = outline
                except KeyError:
                    pass
                for i in range(0, len(coords)-3, 2):
                    section_details = copy_details.copy()
                    section_details['coords'] = coords[i:i+4]
                    build_item_report(section_details, report)
        return report

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

    def coords(self, item, *coords):
        item_details = self.items[item]  # type: dict
        if len(coords) == 0:
            return item_details['coords']
        item_details['coords'] = coords

    def itemconfigure(self, item, **kwargs):
        item_details = self.items[item]
        item_details.update(kwargs)

    def delete(self, item):
        if item == 'all':
            self.items.clear()
        else:
            item_details = self.items[item]
            item_details['deleted'] = True

    def update(self):
        pass

    def bind(self, *args, **kwargs):
        pass

    def unbind(self, *args, **kwargs):
        pass

    def tag_bind(self, *args, **kwargs):
        pass

    def tag_unbind(self, *args, **kwargs):
        pass

    def focus_force(self):
        pass

    def after(self, *args, **kwargs):
        pass

    def tag_raise(self, item):
        item_details = self.items[item]  # type: dict
        self.max_zorder += 1
        item_details['zorder'] = self.max_zorder

    def bbox(self, item):
        item_details = self.items[item]  # type: dict
        x, y = item_details['coords']
        return x, y, x, y


def build_item_report(item_details: dict, report: list):
    if item_details['deleted']:
        return
    if item_details.get('fill') == '':
        return
    if item_details.get('image') == '':
        return
    try:
        del item_details['capstyle']
    except KeyError:
        pass
    try:
        width = item_details.pop('width')
        item_details['pensize'] = width
    except KeyError:
        pass
    del item_details['zorder']
    del item_details['deleted']
    report.append(item_details.pop('method_name'))
    for arg in item_details.pop('coords'):
        report.append("    %r" % int(round(arg)))
    for key, value in sorted(item_details.items()):
        value = item_details[key]
        report.append("    %s=%r" % (key, value))
