#version 150
#extension GL_ARB_explicit_attrib_location : enable

layout(location = 0) in vec3 in_position ;
layout(location = 1) in vec3 in_normal ;
// # 2 is color
layout(location = 3) in vec2 in_texMapping ;

uniform mat4 pMat ;
uniform mat4 mvMat ;
uniform vec3 light ;

out vec3 trueNormal ;
out vec2 texCoord ;
out float intensity ;
out vec4 location ;
out vec3 light2 ;
out vec2 texMapping ;
out mat3 normalMat ;

// (-4.330127,5.0,7.4999995)
void main() {
    light2 = light ; //multMat(mvMat * vec4(light,1)).xyz ;

    location = mvMat * vec4(in_position, 1.0) ;
    gl_Position = pMat * location;
    texCoord = vec2(in_position) * vec2(0.5) + vec2(0.5);
    texMapping = in_texMapping ;
    trueNormal = inverse(transpose(mat3(mvMat))) * (-in_normal) ;
}
