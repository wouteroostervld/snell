# snell
Raytracer in haskell

## Goal

Implementing a haskell raytracer which can render physically believable
pictures. The 'pbrt' of haskell ( https://github.com/mmp ).

## TODO

Things I intent to implement. Pull requests welcome.

- [ ] Scene definition language (aeson? parsec?)
- [ ] Scaling, Rotation and Translation ( matrices/quaternions )
- [ ] physically based shading (monte carlo sampling, gloss)
- [ ] reading RAW-vertex data
- [ ] reading Stanford PLY-vertex data

Example using directional light:

![Diffuse balls](/examples/images/diffuse_balls.png)

Example using a point light:

![Diffuse balls pointlight](/examples/images/diffuse_balls_pointlight.png)

Example using a point light and the reflection shader with a blue teint:

![Reflection](/examples/images/reflection.png)

Example with sheen (schlick approximation):

![Sheen](/examples/images/schlick_sheen.png)

Example with Schlick- (yellow ivory) and SchlickMetal-shader (gold):

![Gold](/examples/images/yellow_ivory_vs_gold.png)
