uniform mat4 i_modelViewProjection;

in vec2 co;

void main()
{
	// Simply pass the texture coordinate and the position.
	gl_Position = vec4(co, 0., 1.);
}