#version 150
#extension GL_ARB_explicit_attrib_location : enable

layout(location = 0) in vec3 in_position ;
layout(location = 3) in vec2 in_texMapping ;

uniform sampler2D noiseTexture ;

uniform mat4 mvMatrix ;
uniform mat4 pMatrix ;
uniform float time ;
uniform vec3 light ;

out vec4 position ;
out vec3 normal ;
out vec2 texMapping ;
out vec4 origPos ;

void main() {
    vec4 u = texture2D( noiseTexture, in_texMapping ) ;
    texMapping = in_texMapping ;

    float r = pow(0.1,in_position.x)+1.2 ;
    float th = in_position.y - ((u.x+1.0) * time/(sqrt(r) * 50.0)) + u.z * 360;
    float ph = 2*(in_position.z - 0.5) ;// pow(in_position.z-0.2,0.5) + ;
    ph *= pow( abs(ph), 2.0 ) * sin( time  / (sqrt(r) * 50));

    vec4 real_position = vec4(
        -r * sin(th) * cos(ph),
        r * sin(ph),
        r * cos(th) * cos(ph),
        1.0 ) ;
    origPos = real_position ;

    vec4 tmp = mvMatrix * real_position ;
    position = tmp ;
    normal = inverse(transpose(mat3(mvMatrix))) * vec3(tmp) ;
    tmp = pMatrix * tmp;
    gl_PointSize = texture2D(noiseTexture,in_texMapping+vec2(.1,.1)).a * 4.0 / length(vec3(tmp)) ;
    gl_Position = tmp ;
}
