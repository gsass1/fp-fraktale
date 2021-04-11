#version 410
out vec4 colorOut;
uniform double screen_width;
uniform double screen_height;
uniform double cx;
uniform double cy;
uniform double zoom;
uniform int iterations;

vec4 map_to_color(float t) {
    float r = 9.0 * (1.0 - t) * t * t * t;
    float g = 15.0 * (1.0 - t) * (1.0 - t) * t * t;
    float b = 8.5 * (1.0 - t) * (1.0 - t) * (1.0 - t) * t;

    return vec4(r, g, b, 1.0);
}

void main(void) {
  dvec2 z, c;
  z.x = (screen_width / screen_height) * (gl_FragCoord.x / screen_width - 0.5);
  z.y = (gl_FragCoord.y / screen_height - 0.5);

  z.x /= zoom;
  z.y /= zoom;

  c.x += cx;
  c.y += cy;

  int i;
  for(i = 0; i < iterations; i++) {
    double x = (z.x * z.x - z.y * z.y) + c.x;
    double y = (z.y * z.x + z.x * z.y) + c.y;

    if((x * x + y * y) > 2.0) break;
    z.x = x;
    z.y = y;
  }

  double t = double(i) / double(iterations);

  colorOut = map_to_color(float(t));
}

