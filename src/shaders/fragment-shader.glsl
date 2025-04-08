
varying vec2 vUvs;

uniform vec2 resolution;
uniform float time;

float sdfSphere(vec3 pos, float r) {
    return length(pos) - r;
}

float sdfHole(vec3 p, float r) {
  return max(length(p) - r, p.y);
}

float sdfBox( vec3 p, vec3 b ) {
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

float sdfTorus( vec3 p, vec2 t ) {
  vec2 q = vec2(length(p.xz)-t.x,p.y);
  return length(q)-t.y;
}

float sdfPlane(vec3 pos) {
    return pos.y;
}

// Struct type to make it easier to track colour wrt object being used
struct MaterialData {
    vec3 colour;
    float dist;
};

MaterialData opU(MaterialData a, MaterialData b) {
  if (a.dist < b.dist) {
    return a;
  }

  return b;
}

MaterialData opSetMinus(MaterialData a, MaterialData b) {
  MaterialData c = MaterialData(b.colour, -b.dist);
  if (a.dist > -b.dist) {
    return a;
  }

  return c;
}

const vec3 RED   = vec3(1.0, 0.0, 0.0);
const vec3 GREEN = vec3(0.0, 1.0, 0.0);
const vec3 BLUE  = vec3(0.0, 0.0, 1.0);
const vec3 GRAY  = vec3(0.5);
const vec3 WHITE = vec3(1.0);
const vec3 BLACK = vec3(0.0);

MaterialData map(vec3 pos) {
    MaterialData result = MaterialData(
        GRAY, sdfPlane(pos - vec3(0.0, -2.0, 0.0)));

    float dist;

    dist = sdfBox(pos - vec3(-2.0, -0.85, 5.0), vec3(1.0));
    result.colour = ((dist < result.dist) ? RED : result.colour);
    result.dist = min(result.dist, dist);

    return result;
}

vec3 CalculateNormal(vec3 pos) {
    const float EPS = 0.0001;
    vec3 n = vec3(
      map(pos + vec3(EPS, 0.0, 0.0)).dist - map(pos - vec3(EPS, 0.0, 0.0)).dist,
      map(pos + vec3(0.0, EPS, 0.0)).dist - map(pos - vec3(0.0, EPS, 0.0)).dist,
      map(pos + vec3(0.0, 0.0, EPS)).dist - map(pos - vec3(0.0, 0.0, EPS)).dist
  );
  return normalize(n);
}

vec3 CalculateLighting(vec3 pos, vec3 normal, vec3 lightColour, vec3 lightDir) {
    float dp = max(0.0, dot(normal, lightDir));

    return lightColour * dp;
}

float CalculateShadow(vec3 pos, vec3 lightDir, float k) {
    float res = 1.0;
    float d = 0.01;
    // Very similar to RayMarch()
    for (int i = 0; i < 64; ++i) {
        float distToScene = map(pos + lightDir * d).dist;

        if (distToScene < 0.001) {
            return 0.0;
        }
        // res works for soft shadows (penumbra)
        res = min(res, k * distToScene / d);
        d += distToScene;
    }

    return res; // If want only strong shadows, sub for 1.0
}

const int NUM_STEPS = 256;
const float MAX_DIST = 1000.0;

// Performs sphere tracing for the scene.
vec3 RayMarch(vec3 cameraOrigin, vec3 cameraDir) {
    vec3 pos;
    MaterialData material = MaterialData(BLACK, 0.0);

    vec3 skyColour = vec3(0.55, 0.6, 1.0);

    for (int i = 0; i < NUM_STEPS; ++i) {
        pos = cameraOrigin + material.dist * cameraDir;

        MaterialData result = map(pos);

        // Case 1: result.dist < 0 (or close), intersected scene
        // BREAK
        if (result.dist < 0.001) {
            break;
        }
        // Update material.dist for next iteration
        material.dist += result.dist;
        material.colour = result.colour;

        // Case 2: material.dist > MAX_DIST, out of scene entirely
        // RETURN default value
        if (material.dist > MAX_DIST) {
            return skyColour;
        }

        // Case 3: repeat process (do nothing)
    }

    // Finished loop

    vec3 normal = CalculateNormal(pos);
    vec3 lightColour = WHITE;
    vec3 lightDir = normalize(vec3(1.0, 2.0, -1.0));

    float shadow = CalculateShadow(pos, lightDir, 12.0);
    
    vec3 lighting = CalculateLighting(pos, normal, lightColour, lightDir);
    lighting *= shadow;

    // vec3 specular = CalculatePhong(normal, lightDir);

    vec3 colour = material.colour * lighting;// + specular;

    float fogFactor = 1.0 - exp(-pos.z * 0.01);
    colour = mix(colour, skyColour, fogFactor);

    return colour;
}

mat3 makeCameraMatrix(vec3 cameraOrigin, vec3 cameraLookAt, vec3 cameraUp) {
    vec3 z = normalize(cameraLookAt - cameraOrigin);
    vec3 x = normalize(cross(z, cameraUp));
    vec3 y = cross(x, z);
    return mat3(x, y, z);
}

void main() {
    vec2 pixelCoords = (vUvs - 0.5) * resolution;

    vec3 rayDir = normalize(vec3(pixelCoords * 2.0 / resolution, 1.0));
    vec3 rayOrigin = vec3(0.0);
    vec3 rayLookAt = vec3(0.0, 0.0, 1.0);
    vec3 cameraUp = vec3(0.0, 1.0, 0.0);
    mat3 camera = makeCameraMatrix(rayOrigin, rayLookAt, cameraUp);

    vec3 colour = RayMarch(rayOrigin, rayDir);

    gl_FragColor = vec4(pow(colour, vec3(1.0 / 2.2)), 1.0);
}