#version 150
#extension GL_ARB_explicit_attrib_location : enable
layout(location = 0) in vec3 in_position ;
out vec2 texMap ;

void main() {
    texMap = (in_position.xy + 1.0) / 2.0 ;
    gl_Position = vec4(in_position,1.0) ;
}
