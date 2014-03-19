#version 150
uniform sampler2D texture ;
in vec2 texMap ;

out vec4 frag_color ; 

void main() {
    frag_color = texture2D( texture, texMap ) ;   
}
