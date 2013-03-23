package nebula

import java.io.File
import grizzled.math.stats._

////////////////////////////////

object Benchmark {
  def ticTocNano[A](closure: => A): (A, Long) = {
    val before = System.nanoTime
    val result = closure
    val after = System.nanoTime
    (result, after - before)
  }  
  
  /**
   * Measures the average running time of |closure|. Does warm-up first.
   * |closure| should return a real value so the JVM doesn't optimize it away.
   * |numIterations| is the number of times |closure| is evaluated between
   * timing events. A good target is to make it high enough that it takes
   * around 1 second to run |closure| |numIterations| times.
   * 
   * Times are reported in milliseconds.
   */
  def measure[A](closure: () => A, numIterations: Int): Double = {
    val numMeasurements = 32
    
    val closures = (numIterations times closure) toList
    
    // Warm up
    val (warmupResults, warmupNanoseconds) = ticTocNano {
      closures map (_.apply)
    }
    
    val resultsAndTimings = numMeasurements times {
      ticTocNano {
        closures map (_.apply)
      }
    }
    
    // Do something with all the results so the compiler can't optimize
    // everything away.
    val allResults = warmupResults ++ resultsAndTimings.map(_._1).flatten
    val resultsString = (allResults map (_.toString)) mkString
    val file = File.createTempFile("benchmark", ".txt")
    file.writeString(resultsString)
    file.deleteOnExit
    
    val mainTimings = resultsAndTimings.map(_._2) map (_.toDouble)
    val perClosureTimings = mainTimings map (_ / numIterations)
    
    // Just return the median timing.
    median(perClosureTimings: _*) / 1000000
  }
}