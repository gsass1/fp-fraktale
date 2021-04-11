#version 410
out vec4 colorOut;

uniform double screen_width;
uniform double screen_height;

/* Gradient-Shader */
void main(void) {
  vec2 st = vec2(gl_FragCoord.x / screen_width, gl_FragCoord.y / screen_height);

  vec3 color1 = vec3(1.0, 0.89, 0.47);
  vec3 color2 = vec3(0.24, 0.1, 0.45);

  float mixValue = distance(st,vec2(0,1));
  vec3 color = mix(color1, color2, mixValue);

  colorOut = vec4(color, mixValue);
}