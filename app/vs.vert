in vec2 co;
out vec4 vertexColor;

vec4 color[3] = vec4[](
    vec4(1., 0., 0., 1.)
  , vec4(0., 1., 0., 1.)
  , vec4(0., 0., 1., 1.)
  );

void main() {
  gl_Position = vec4(co, 0., 1.);
  vertexColor = color[gl_VertexID];
}