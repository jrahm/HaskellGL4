#version 150
#extension GL_ARB_explicit_attrib_location : enable

layout(location = 0) in vec3 in_position ;
layout(location = 1) in vec3 in_normal ;
layout(location = 3) in vec2 in_texMapping ;

uniform mat4 mvMat ;
uniform mat4 pMat ;
uniform float time ;

out vec2 texMapping ;
out vec3 position ;
out vec3 normal ;

void main () {
    vec4 tmp = mvMat * vec4(in_position, 1.0);
    gl_Position = pMat * tmp ;
    gl_PointSize = 4.0 ;
    position = vec3( tmp ) ;
    texMapping = in_texMapping ;
    normal = inverse(transpose(mat3(mvMat))) * in_normal ;
}
