package nebula.graveyard

//import nebula._
//
//import com.nativelibs4java.opencl._
//import com.nativelibs4java.opencl.CLMem.Usage
//import com.nativelibs4java.opencl.util._
//import com.nativelibs4java.util._
//import org.bridj.Pointer
//import org.bridj.Pointer._  
//import java.nio.ByteOrder
//
//object OpenCLUtil { 
//    val context = JavaCL.createBestContext
//    
//    def arrayToCL(array: Array[Int]): Pointer[java.lang.Integer] = {
//      val pointer = allocateInts(array.size).order(context.getByteOrder)
//      for ((v, i) <- array.zipWithIndex) pointer.set(i, v)
//      pointer
//    }
//    
//    def clToArray(size: Int, pointer: Pointer[java.lang.Integer]): Array[Int] = {
//      val array = Array.fill(size)(0)
//      for (i <- 0 until size) array(i) = pointer.get(i)
//      array
//    }  
//}
