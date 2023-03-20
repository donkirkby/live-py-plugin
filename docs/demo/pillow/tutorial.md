---
title: Pillow Tutorial
layout: react
is_react: True
modules: pillow
files: hopper.jpg
---

This is a starting point for a Pillow tutorial. Until the `Image.show()` method
is monkey patched, use the `LivePillowImage` workaround.

    ### Canvas
    from PIL import Image
    from space_tracer import LivePillowImage
    
    image = Image.open('hopper.jpg')
    scale = 0.75
    image = image.resize((
        round(image.width*scale),
        round(image.height*scale)))
    
    live_image = LivePillowImage(image)
    live_image.display((-image.width/2, image.height/2))
