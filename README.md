# Ray-Marching
Basic scene of procedurally generated terrain constructed using Ray Marching.

![Terrain.jpeg](https://github.com/Satoniou/Ray-Marching/blob/main/Images/Terrain.jpeg)

This project is based on SimonDev's course [GLSL Shaders from Scratch](https://simondev.teachable.com/courses/). The code referenced is located in the `fragment-shader.glsl` file.

Sphere Tracing was used to render the geometry. The shadow uses almost the same algorithm as the main `RayMarch()` loop, but reduced to a `float` factor.

The terrain is generated using [Fractal Brownian Motion](https://thebookofshaders.com/13/). We take a union of both the terrain and water materials using `opU()`, which just outputs the closest of the inputs.

Each material is stored in a `struct MaterialData`. It stores both `vec3 colour` and `float dist` arguments, so the `RayMarch()` loop can refer to `colour` given `dist` is small enough, which would mean the ray intersected the object.

Here is an example of a simpler scene that can be constructed with this method:

![Cubes.jpeg](https://github.com/Satoniou/Ray-Marching/blob/main/Images/Cubes.jpeg)

The `map()` function for this scene is the following:

```
MaterialData map(vec3 pos) {
  MaterialData result = MaterialData(
  GRAY, sdfPlane(pos - vec3(0.0, -2.0, 0.0)));
  
  float dist;

  dist = sdfBox(pos - vec3(-2.0, -0.85, 5.0), vec3(1.0));
  MaterialData cube = MaterialData(RED, dist);
  result = opU(result, cube);
  
  dist = sdfBox(pos - vec3(2.0, -0.85, 5.0), vec3(1.0));
  cube = MaterialData(BLUE, dist);
  result = opU(result, cube);

  dist = sdfBox(pos - vec3(2.0, 1.0, 50.0 + sin(time) * 0.0), vec3(2.0));
  cube = MaterialData(BLUE, dist);
  result = opU(result, cube);

  return result;
}
```

Notice that this new scene also displays penumbra (soft shadow near the borders). The approach used can be found on [Inigo Quilez](https://iquilezles.org/articles/rmshadows/).
