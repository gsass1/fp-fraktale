attribute vec2 vp;
void main(void) {
  gl_Position = vec4(vp, 0.0, 1.0);
}

