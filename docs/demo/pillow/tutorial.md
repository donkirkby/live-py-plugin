---
title: Pillow Tutorial
layout: react
is_react: True
modules: pillow,numpy
files: hopper.jpg
---

This is a starting point for a Pillow tutorial. See the [Pillow docs] for
inspiration.

    ### Canvas
    from PIL import Image
    from space_tracer import LivePillowImage
    
    image = Image.open('hopper.jpg')
    scale = 0.75
    image = image.resize((
        round(image.width*scale),
        round(image.height*scale)))
    
    image.show()

[Pillow docs]: https://pillow.readthedocs.io/en/stable/handbook/tutorial.html
