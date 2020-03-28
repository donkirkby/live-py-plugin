from svg_turtle import SvgTurtle


def draw_spiral(t):
    t.fillcolor('blue')
    t.begin_fill()
    for i in range(20):
        d = (50 + i*i*1.5)
        t.pencolor(0, 0.05*i, 0)
        t.width(i)
        t.forward(d)
        t.right(144)
    t.end_fill()


def write_file(filename):
    t = SvgTurtle.create(500, 500)
    draw_spiral(t)
    t.save_as(filename)


def main():
    write_file('example.svg')
    print('Done.')


if __name__ == '__main__':
    main()
