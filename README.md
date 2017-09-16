# snell

Raytracer in haskell

## Goal

Implementing a haskell raytracer which can render physically believable
pictures. The 'pbrt' of haskell ( https://github.com/mmp ).

## TODO

- [ ] Absract stuff into modules.
- [ ] Use lua to define scene(s), camera's, objects, shaders, texture (image and mapping), position and scale (STR matrices).
- [ ] Use lua, multiple scenes and something like ffmpeg to create animation.
- [ ] Make asynchreous processing using the lua scripting -possible- default.

Use the Surface datatype with only funtions. Add state using (custom) constructor function. Static state by inserting partially evaluated functions. Like colorplain :: Color -> Hitpoint -> Color the Surface takes a Hitpoint -> Color so suplly the plain color. -Dynamic state by using a ST/Ref or something. Most of the time a Configuration as SRT-matrix as state is enough.- Dynamic state is not needed. Add SRT to init and make lua plumbing. (With an object keeping track of STR. Make STR setting (even if null) happen before setting shaders etc.) Actual contructor can be in haskell taking some long record.

![Diffuse balls](/examples/images/diffuse_balls.png)

Example using a point light:

![Diffuse balls pointlight](/examples/images/diffuse_balls_pointlight.png)

Example using a point light and the reflection shader with a blue teint:

![Reflection](/examples/images/reflection.png)

Example with sheen (schlick approximation):

![Sheen](/examples/images/schlick_sheen.png)

Example with Schlick- (yellow ivory) and SchlickMetal-shader (gold):

![Gold](/examples/images/yellow_ivory_vs_gold.png)
