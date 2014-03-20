#version 150

out vec4 frag_color ;

uniform vec3 light ;
uniform sampler2D noiseTexture ;

// normal == position ;
in vec4 position ;
in vec3 normal ;
in vec2 texMapping ;

in float rad ;
in vec3 mNormal ;
in vec4 origPos_ ;

void main() {
    float intensity = dot( normalize(vec3(position) - light), normalize(mNormal) );
    frag_color = vec4(
        mix(vec3(texture2D(noiseTexture,origPos_.xy))*intensity,
            vec3(1.0), 0.90)
        ,0.1/pow(rad,1.7)) ;
}
