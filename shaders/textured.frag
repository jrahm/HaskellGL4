#version 150
uniform sampler2D texture ;
uniform sampler2D earth ;
uniform sampler2D clouds ;
uniform sampler2D lights ;
uniform sampler2D random ;
uniform sampler2D winter ;

uniform float time ;
uniform vec3 light ;
uniform float dX ;
uniform float dY ;

in vec2 texCoord ;
in vec3 trueNormal ;
in vec4 location ;
in vec3 light2 ;
in mat3 normalMat ;

out vec4 frag_color ;
in vec2 texMapping ;


float cloudMotion = -time / 1100.0 ;
float earthMotion = -time / 1400.0 ;

vec3 lighting( vec3 norm, vec3 lightPos ) {
    vec3 diff = normalize(vec3(location) - lightPos) ;
    float tmp = dot(diff, normalize( norm )) + 0.3 ;

    if( tmp < 0.0 ) return vec3(0) ;
    return vec3(
        pow(tmp / length(diff),0.3),
        pow(tmp / length(diff),0.8),
        pow(tmp / length(diff),1.0)
    );
}

vec4 sample(sampler2D tex, float xc,float yc) {
   return texture2D(texture,texMapping + vec2(earthMotion,0.0) + vec2(xc,yc));
}

float f( float x ) {
    return x * (x - 1.0) ;
}

vec3 calNormChange( sampler2D tex, vec3 norm, vec3 down, vec3 right ) {
    float x00 = length(sample(tex,-dX, dY)) ; 
    float x01 = length(sample(tex,  0, dY)) ; 
    float x02 = length(sample(tex, dX, dY)) ; 

    float x10 = length(sample(tex,-dX, 0)) ; 
    float x11 = length(sample(tex,  0, 0)) ; 
    float x12 = length(sample(tex, dX, 0)) ; 

    float x20 = length(sample(tex,-dX,-dY)) ; 
    float x21 = length(sample(tex,  0,-dY)) ; 
    float x22 = length(sample(tex, dX,-dY)) ; 

    down = ((x11 - x00) + (x11 - x01) + (x11 - x02) - (x11 - x20) - (x11 - x21) - (x11 - x22)) * down ;
    right = ((x11 - x00) + (x11 - x10) + (x11 - x20) - (x11 - x02) - (x11 - x12) - (x11 - x22)) * right ;
    
    return (right + down + norm) / 3.0 ;
}

vec4 sampleBlur( sampler2D tex ) {
    return
        texture2D(texture,texMapping + vec2(earthMotion,0.0) + vec2( dX, dY)) / 9.0 +
        texture2D(texture,texMapping + vec2(earthMotion,0.0) + vec2(  0, dY)) / 9.0 +
        texture2D(texture,texMapping + vec2(earthMotion,0.0) + vec2(-dX, dY)) / 9.0 +
        texture2D(texture,texMapping + vec2(earthMotion,0.0) + vec2( dX,  0)) / 9.0 +
        texture2D(texture,texMapping + vec2(earthMotion,0.0) + vec2(  0,  0)) / 9.0 +
        texture2D(texture,texMapping + vec2(earthMotion,0.0) + vec2(-dX,  0)) / 9.0 +
        texture2D(texture,texMapping + vec2(earthMotion,0.0) + vec2( dX,-dY)) / 9.0 +
        texture2D(texture,texMapping + vec2(earthMotion,0.0) + vec2(  0,-dY)) / 9.0 +
        texture2D(texture,texMapping + vec2(earthMotion,0.0) + vec2(-dX,-dY)) / 9.0;
}

void main() {
    vec3 down = vec3( 0, -1, 0 ) ;
    vec3 right = normalize(cross( trueNormal, down )) ;
    down = normalize(cross( trueNormal, right ) );

    /* Calculate the new normal using bump mapping */
    vec3 newNorm = calNormChange( texture, trueNormal, down, right ) ;

    /* Calculate the shadow casted by the clouds. Blur it a litte for
     * realism */
    vec4 shadow = (texture2D(clouds, texMapping+vec2(cloudMotion-0.005,-0.005))+
                   texture2D(clouds, texMapping+vec2(cloudMotion-0.01,-0.01))+
                   texture2D(clouds, texMapping+vec2(cloudMotion-0.01,-0.005))+
                   texture2D(clouds, texMapping+vec2(cloudMotion-0.005,-0.01))) / 8.0 ;

    /* The color of the clouds at the position */
    vec4 cloudsColor = texture2D(clouds, texMapping+vec2(cloudMotion,0.0)) ;

    /* Mix the colors of the earth in the summer and earth in the winter with
     * as a function of the sin of the time */
    vec4 color = mix(
            texture2D(earth, texMapping+vec2(earthMotion,0.0)),
            texture2D(winter, texMapping+vec2(earthMotion,0.0)), (sin(time / 400.0) + 1.0) / 2.0
            /* Add the clouds an subtract the shadows */
            ) + cloudsColor - shadow ;
    
    /* Calulate the light intensity using the new norrmal */
    vec3 light = lighting(newNorm, light2) + vec3(0.05);
    vec4 tmpcolor = vec4(color.x*light.x, color.y*light.y, color.z*light.z, 1.0);

    /* Get a couple of random values from the noise texture */
    vec4 cmp = texture2D(random, texMapping+vec2(earthMotion,0.0)) ;
    vec4 cmp2 = texture2D(random, texMapping+vec2(earthMotion,0.1)) ;

    /* Get the texture for the city lights */
    vec4 cityLights = texture2D(lights, texMapping+vec2(earthMotion,0.0)) ;

    if ( pow(length(cmp),2) > pow(length(tmpcolor)*1.3,2) && length(cityLights) > 1.0) {
        /* if the random value is larger than the current color and there is a light
         * in this position, turn it on. This is to simulate lights in regions
         * under the shadow of a cloud */
        tmpcolor += vec4(1.0,1.0,0.5,1.0) * cityLights ;
    }

    if (0.1 > light.x) {
        /* It is night time all overt the place, so include the night texture
         * and subtract out the clouds. Mix between two reddish-yellow colors as
         * a function of time to simulate twinkling */
        tmpcolor *= mix(
            vec4(cmp.r*2.0,min(cmp.g,cmp.r)*1.5,min(cmp.b,cmp.r),1.0),
            vec4(2.0,2.0,2.0,2.0), sin(time/10.0+cmp.a*27.3) ) ;
        tmpcolor -= cloudsColor;
    }

    /* Draw the atmosphere of the earth. The blue ring at the edge of the
     * Earth*/
    float blue = max(
        pow(1.0-dot(vec3(0,0,-1), normalize(trueNormal)),3.0),
        0.0);
    tmpcolor += vec4(0.0,0.0,blue*length(light),0.0) ;

    /* Calculate the Sun's reflection off of the water on the Earth.
     * Get a blurred sample from the earth to check for blue */
    vec4 sample = sampleBlur(earth);

    /* calculate the coefficient of the specular dot based on
     * the amount of blue in the sample */
    float lightlen = pow(max(length(light)-0.8,0.0),5.0) * (sample.b/(sample.r+sample.g)) * (1-length(shadow));
    tmpcolor += vec4(0.3,0.2,0.15,0.0) *
        max(
            pow(dot(reflect(normalize(vec3(location) - light2), normalize(trueNormal)),
                - normalize(vec3(location))),1.0) *
        lightlen, 0.0) ;
    frag_color = tmpcolor ;
}



