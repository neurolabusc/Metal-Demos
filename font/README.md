## About

This is a simple Lazarus project that displays text using the Apple Metal framework. Fonts are typically difficult to render using high performance low-level graphics. Dragging the mouse moves the position of the text.

 - This project exploits Viktor Chlumský's [Multi-channel signed distance field generator](https://github.com/Chlumsky/msdfgen).
 - For a nice description of (single-channel) signed distance fonts, see [Valve's seminal 2007 Siggraph paper](https://steamcdn-a.akamaihd.net/apps/valve/2007/SIGGRAPH2007_AlphaTestedMagnification.pdf), this [page](https://blog.mapbox.com/drawing-text-with-signed-distance-fields-in-mapbox-gl-b0933af6f817) (which links to a [WebGL Demo](https://mapbox.s3.amazonaws.com/kkaefer/sdf/index.html)), this [descriptio](https://github.com/libgdx/libgdx/wiki/Distance-field-fonts).
 - This project is ports a cross platform [OpenGL](https://github.com/neurolabusc/OpenGLCoreTutorials) project.

![Font rendering](font.png)
