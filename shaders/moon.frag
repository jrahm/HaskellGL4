#version 150
in vec2 texMapping ;
in vec3 position ;
in vec3 normal ;

uniform sampler2D texture ;
uniform vec3 lightPos ;
uniform float dX ;
uniform float dY ;

out vec4 frag_color ;

vec4 sample(float xc,float yc) {
   return texture2D(texture,texMapping + vec2(xc,yc));
}


vec3 calNormChange( vec3 norm, vec3 down, vec3 right ) {
    float x00 = length(sample(-dX, dY)) ; 
    float x01 = length(sample(  0, dY)) ; 
    float x02 = length(sample( dX, dY)) ; 

    float x10 = length(sample(-dX, 0)) ; 
    float x11 = length(sample(  0, 0)) ; 
    float x12 = length(sample( dX, 0)) ; 

    float x20 = length(sample(-dX,-dY)) ; 
    float x21 = length(sample(  0,-dY)) ; 
    float x22 = length(sample( dX,-dY)) ; 

    down = ((x11 - x00) + (x11 - x01) + (x11 - x02) - (x11 - x20) - (x11 - x21) - (x11 - x22)) * down ;
    right = ((x11 - x00) + (x11 - x10) + (x11 - x20) - (x11 - x02) - (x11 - x12) - (x11 - x22)) * right ;
    
    return (right + down + norm*3.0) / 5.0 ;
}

void main() {
    vec3 down = vec3( 0, -1, 0 ) ;
    vec3 right = normalize(cross( normal, down )) ;
    down = normalize(cross( normal, right ) );

    vec3 newNorm = calNormChange( normal, down, right ) ;
    vec3 diff = lightPos - position ;
    float intensity = max(dot( normalize(diff), normalize(newNorm) ),0.0) ;
    intensity = sqrt(sqrt( intensity )) ;
    
    vec4 tmpcolor = texture2D(texture, texMapping) * intensity ;
    tmpcolor *= pow(length(tmpcolor),4.0-min(length(position)/4.0,4.0)) ;
    frag_color = tmpcolor ;
}
