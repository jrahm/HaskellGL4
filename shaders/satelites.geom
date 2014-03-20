#version 150
layout(points) in;
layout(triangle_strip, max_vertices=28) out;

out float rad ;
out vec3 mNormal ;

uniform mat4 mvMatrix ;
in vec4 origPos[] ;
out vec4 origPos_ ;

void vertex( vec3 pos ) {
    gl_Position = gl_in[0].gl_Position + vec4(pos,0.0) ;
    origPos_ = origPos[0] ;
    EmitVertex() ;
}

void main( ) {
    mNormal = -inverse(transpose(mat3(mvMatrix))) * vec3(0,0,1.0) ;
    float r = 0.005 ;
    float th = 0.00 ;
    for( ; th < 6.3 ; th += 0.5 ) {
        rad = 2 ;
        vertex( vec3(r*sin(th),r*cos(th),0.0) ) ;
        rad = 0.0 ;
        vertex( vec3(0.0,0.0,0.0) ) ;
    }
    rad = 2 ;
    vertex( vec3(r*sin(0.0),r*cos(0.0),0.0) ) ;
    EndPrimitive();
}
