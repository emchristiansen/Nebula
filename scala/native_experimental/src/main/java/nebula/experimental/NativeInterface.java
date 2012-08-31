package nebula.experimental;

import com.sun.jna.Library;
import com.sun.jna.Native;

public class NativeInterface {
  public interface OpenCV extends Library {
    OpenCV INSTANCE = (OpenCV) Native.loadLibrary("wrapopencv", OpenCV.class);

    double lbphDistance(String leftImagePath, String rightImagePath);
    
    void detectFace(String imagePath, int box[]);
  }
}
