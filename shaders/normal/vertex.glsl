attribute vec2 vp;
uniform double cx;
uniform double cy;
uniform double zoom;
uniform double screen_width;
uniform double screen_height;

void main(void) {
  /* Ideally you should use Matrix-transformations for this but since we're only in 2D space we can do with simple linear transformations */
  gl_Position = vec4((screen_height/screen_width)*(vp.x - cx)*zoom, (vp.y - cy)*zoom, 0.0, 1.0);
}

