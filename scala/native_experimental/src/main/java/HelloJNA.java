package nebula.experimental;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;

/** Simple example of JNA interface mapping and usage. */
public class HelloJNA {

    // This is the standard, stable way of mapping, which supports extensive
    // customization and mapping of Java to native types.

    public interface CLibrary extends Library {
        CLibrary INSTANCE = (CLibrary)
            Native.loadLibrary((Platform.isWindows() ? "msvcrt" : "c"),
                               CLibrary.class);

        void printf(String format, Object... args);
    }
    
    public interface HelloLibrary extends Library {
      HelloLibrary INSTANCE = (HelloLibrary)
          Native.loadLibrary("hellolibrary",
                             HelloLibrary.class);

      void printGreeting();
      void printString(String string);
  }    

    public static void printString(String string) {
      HelloLibrary.INSTANCE.printString(string);
    }
    
//    public static void main(String[] args) {
//        CLibrary.INSTANCE.printf("Hello, World\n");
//        for (int i=0;i < args.length;i++) {
//            CLibrary.INSTANCE.printf("Argument %d: %s\n", i, args[i]);
//        }
//    }
}
