package nebula

import java.awt.image.BufferedImage

///////////////////////////////////////////////////////////

trait HasImagePair {
  def leftImage: BufferedImage
  def rightImage: BufferedImage
}

///////////////////////////////////////////////////////////

trait HasGroundTruth[A] {
  def groundTruth: A
}



///////////////////////////////////////////////////////////

trait HasEstimate[A] {
  def estimate: A
}

///////////////////////////////////////////////////////////

trait Experiment extends HasOriginal {
  def name: String

  // Parameter names and values
  def parameters: Seq[Tuple2[String, String]]
  
  /////////////////////////////////////////////////////////// 
  
  val unixEpoch = System.currentTimeMillis / 1000L

  def stringMap = parameters.toMap
}