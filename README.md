## About

Metal is an Apple framework that allows supports high performance graphics. These simple projects exploit the [Lazarus Metal Package created by Ryan Joseph of the Alchemist Guild](https://github.com/genericptr/Metal-Framework).

## Installation

 - You will need to have the latest version of [Lazarus](https://www.lazarus-ide.org/) installed on a MacOS computer (or better yet, have the latest [Lazarus SVN](http://wiki.lazarus.freepascal.org/Getting_Lazarus)).
 - You will need to download [the Alchemist Guild Lazarus Metal Package by Ryan Joseph](https://github.com/genericptr/Metal-Framework).
 - From Lazarus, choose Package/OpenPackageFile and select lazmetalcontrol.lpk - install this into Lazarus.
 - You should now be able to open and compile the projects provided here.
 - To use Metal you will need MacOS 10.12 (Sierra) or later and a Mac computer that [supports Metal](https://support.apple.com/kb/SP765?locale=en_US) - typically Macs from 2012 and later.

## Compiling for OpenGL

Each of these projects can be compiled to use Apple's Metal Framework. However, most of the projects can also be compiled to use OpenGL 3.3 Core. This allows you to build these projects for computers that do not support Metal (Windows, Linux and older Mac hardware).

 - For MacOS, to compile for OpenGL you will want to comment out the line "{$DEFINE METALAPI}" at the start of each projects main form unit.
 - For Windows and Linux, you will need to open the project and select the Project/ProjectInspector menu item. Then remove the required package "lazmetalcontrol".

## Notes

 - Be aware that [OpenGL clip space differs from Metal](https://mellinoe.github.io/veldrid-docs/articles/backend-differences.html) so you will need to modify your [orthographic](https://stackoverflow.com/questions/36295339/metal-nothing-is-rendered-when-using-orthographic-projection-matrix#40856855) or [perspective](https://stackoverflow.com/questions/48311452/glkit-vs-metal-perspective-matrix-difference) projection matrix.
 - The current Metal component will continuously refresh at your systems [preferredFramesPerSecond](https://developer.apple.com/documentation/metal/devices_and_commands?language=objc), regardless of whether any property of your image has changed. To conserve battery life, you may want to write custom code to exit the OnPaint() event if nothing has changed. Alternatively, you may want to modify the component to allow [on demand rendering](https://metashapes.com/blog/advanced-nsview-setup-opengl-metal-macos/).
 - The current Metal component does not support multiple shaders or [compute](https://developer.apple.com/documentation/metal/hello_compute) shaders. Future versions that include these will allow nice effects like [bloom](http://weblog.jamisbuck.org/2016/2/27/bloom-effect-in-metal.html) and ambient occlusion.
  - Be aware that MacOS support for Metal is still in its infancy. The integration and support for the AMD, Intel and NVidia graphics cards used in Macs may not be as strong as with Apple's own graphics cards used in its iPhones. The Mac implementation of metal does not support some features such as [tile-based deferred rendering](https://developer.apple.com/documentation/metal/advanced_techniques/deferred_lighting), meaning one can not always use the same code for MacOS and iOS. Until Apple releases its own graphics cards for Mac or changes its priorities, you should have realistic expectations for Metal on your Mac. Support for [compute shaders](https://www.fractalarchitect.net/blog/2017/10/is-it-time-to-remove-gpu-rendering-support-from-the-app/) has been spotty. In OpenGL/OpenCL, not-a-number values are nons-signalling. However, at least on Macs Metal shaders will [lock up](https://www.fractalarchitect.net/blog/category/support/) when they encounter [NaNs](https://www.fractalarchitect.net/blog/2015/10/apple-are-you-going-to-fix-opencl-metal-compute-in-el-capitan/). You will need to design your code to avoid any edge cases that might generate NaNs (this explains the addFuzz() function in this repository's volume rendering example). There are also examples where Metal shows much poorer discard() and loop performance than more mature GLSL code running on the same hardware. Metal for Mac is clearly a work in progress, and there is a price for living on the bleeding edge. Metal will only benefit [certain situations](https://arstechnica.com/gadgets/2015/10/metal-performance-in-os-x-el-capitan-sometimes-great-often-mixed/). While OpenGL may be deprecated, for many developers it may be wise to let Metal mature a bit before investing time into supporting this. Perhaps Apple will update Metal to support many handy features (like geometry shaders) or functions that will make porting OpenGL applications easier and allow Metal applications to perform faster.

## Projects

With the exception of the basic project, each of these projects can compile to use the OpenGL or Metal framework. To compile for OpenGL, comment out the line '{$DEFINE METALPI}.

- [A basic hello triangle example](basic/)
- [Display a 2D bitmap as a texture](texture/)
- [A simple parallel compute project that converts a color image to grayscale](compute)
- [Display high quality fonts](font/)
- [A simple OBJ/PLY mesh viewer](mesh/)
- [A variation of the mesh viewer that adds ambient occlusion](occlusion/)
- [Volume render NIfTI (MRI/CT) format images](volumerender/)
