package nebula.experimental;

import static java.lang.Math.cos;
import static java.lang.Math.sin;
import static org.bridj.Pointer.allocateFloats;

import java.io.File;
import java.io.IOException;
import java.nio.ByteOrder;

import org.bridj.Pointer;

import com.nativelibs4java.opencl.CLBuffer;
import com.nativelibs4java.opencl.CLContext;
import com.nativelibs4java.opencl.CLEvent;
import com.nativelibs4java.opencl.CLKernel;
import com.nativelibs4java.opencl.CLMem.Usage;
import com.nativelibs4java.opencl.CLProgram;
import com.nativelibs4java.opencl.CLQueue;
import com.nativelibs4java.opencl.JavaCL;
import com.nativelibs4java.util.IOUtils;



public class JavaCLTutorial1 {
    public static void example() throws IOException {
        CLContext context = JavaCL.createBestContext();
        System.out.println(context);
        CLQueue queue = context.createDefaultQueue();
        ByteOrder byteOrder = context.getByteOrder();
        
        int n = 1024;
        Pointer<Float>
            aPtr = allocateFloats(n).order(byteOrder),
            bPtr = allocateFloats(n).order(byteOrder);

        for (int i = 0; i < n; i++) {
            aPtr.set(i, (float)cos(i));
            bPtr.set(i, (float)sin(i));
        }

        // Create OpenCL input buffers (using the native memory pointers aPtr and bPtr) :
        CLBuffer<Float> 
            a = context.createBuffer(Usage.Input, aPtr),
            b = context.createBuffer(Usage.Input, bPtr);

        // Create an OpenCL output buffer :
        CLBuffer<Float> out = context.createFloatBuffer(Usage.Output, n);

        // Read the program sources and compile them :
        File srcFile = new File(ClassLoader.getSystemResource("/opencl/tutorial.cl").getFile());
        String src = IOUtils.readText(srcFile);
        CLProgram program = context.createProgram(src);

        // Get and call the kernel :
        CLKernel addFloatsKernel = program.createKernel("add_floats");
        addFloatsKernel.setArgs(a, b, out, n);
        CLEvent addEvt = addFloatsKernel.enqueueNDRange(queue, new int[] { n });
        
        Pointer<Float> outPtr = out.read(queue, addEvt); // blocks until add_floats finished

        // Print the first 10 output values :
        for (int i = 0; i < 10 && i < n; i++)
            System.out.println("out[" + i + "] = " + outPtr.get(i));
        
    }
}
