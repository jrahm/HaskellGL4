#version 150
in vec3 in_position ;
out vec2 texMap ;

void main() {
    texMap = (in_position.xy + 1.0) / 2.0 ;
    gl_Position = vec4(in_position,1.0) ;
}
