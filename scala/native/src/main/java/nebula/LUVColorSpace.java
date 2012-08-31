package nebula;

import java.awt.color.ColorSpace;

// TODO: Remove this class, I'm not using it.

/**
 * Represents the LUV colorspace. Algorithms taken from
 * http://www.easyrgb.com/
 * 
 * @author Derek Chen-Becker
 *
 */
public class LUVColorSpace extends ColorSpace {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public LUVColorSpace() {
	super(ColorSpace.TYPE_Luv, 3);
    }
    
    // array indices
    private int L = 0, U = 1, V = 2;
    private int X = 0, Y = 1, Z = 2;
    
    // Constants
    private float EPSILON = 0.008856f;
    
    private float ref_X = 96.422f;     //Observer= 2°, Illuminant= D50
    private float ref_Y = 100.000f;
    private float ref_Z = 82.521f;
    
    private static ColorSpace sRGB = ColorSpace.getInstance(ColorSpace.CS_sRGB);

    @Override
	public float[] fromCIEXYZ(float[] colorvalue) {
	float[] result = new float[3];
	
	float var_U = ( 4 * colorvalue[X] ) / ( colorvalue[X] + ( 15 * colorvalue[Y] ) + ( 3 * colorvalue[Z] ) );
	float var_V = ( 9 * colorvalue[Y] ) / ( colorvalue[X] + ( 15 * colorvalue[Y] ) + ( 3 * colorvalue[Z] ) );

	float var_Y = Y / 100;
	if ( var_Y > EPSILON ) {
	    var_Y = (float) Math.pow(var_Y, 1/3f);
	} else {
	    var_Y = ( 7.787f * var_Y ) + ( 16f / 116f );
	}

	float ref_U = ( 4 * ref_X ) / ( ref_X + ( 15 * ref_Y ) + ( 3 * ref_Z ) );
	float ref_V = ( 9 * ref_Y ) / ( ref_X + ( 15 * ref_Y ) + ( 3 * ref_Z ) );

	result[L] = ( 116 * var_Y ) - 16;
	result[U] = 13 * result[L] * ( var_U - ref_U );
	result[V] = 13 * result[L] * ( var_V - ref_V );
	
	return result;
    }

    @Override
	public float[] fromRGB(float[] rgbvalue) {
	return fromCIEXYZ(sRGB.toCIEXYZ(rgbvalue));
    }

    @Override
	public float[] toCIEXYZ(float[] colorvalue) {
	float result[] = new float[3];

	float var_Y;
	
	var_Y = ( colorvalue[L] + 16f ) / 116f;
	float var_Y_cubed = (float) Math.pow(var_Y, 3); 
	if ( var_Y_cubed > EPSILON ) {
	    var_Y = var_Y_cubed;
	} else {
	    var_Y = ( var_Y - 16f / 116f ) / 7.787f;
	}

	float ref_U = ( 4 * ref_X ) / ( ref_X + ( 15 * ref_Y ) + ( 3 * ref_Z ) );
	float ref_V = ( 9 * ref_Y ) / ( ref_X + ( 15 * ref_Y ) + ( 3 * ref_Z ) );

	float var_U = colorvalue[U] / ( 13 * colorvalue[L] ) + ref_U;
	float var_V = colorvalue[V] / ( 13 * colorvalue[L] ) + ref_V;

	result[Y] = var_Y * 100;
	result[X] =  - ( 9 * Y * var_U ) / ( ( var_U - 4 ) * var_V  - var_U * var_V );
	result[Z] = ( 9 * Y - ( 15 * var_V * Y ) - ( var_V * X ) ) / ( 3 * var_V );
	
	return result;
    }

    @Override
	public float[] toRGB(float[] colorvalue) {
	// convert to XYZ first, then to sRGB
	return sRGB.fromCIEXYZ(toCIEXYZ(colorvalue));
    }

}