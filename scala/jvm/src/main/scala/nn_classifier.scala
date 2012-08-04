package nebula

import java.awt.image._

case class NNRecognizer[A](val distances: List[Tuple2[Int, NNRecognizer.ImageDistance]],
                           val trainData: List[Tuple2[A, BufferedImage]]) {
  def neighbors(distances: List[Tuple2[Int, NNRecognizer.ImageDistance]],
                candidates: List[Tuple2[A, BufferedImage]],
                query: BufferedImage): List[Tuple2[A, BufferedImage]] = {
    distances match {
      case Nil => {
        candidates
      }
      case (numNeighbors, distance) :: remainingDistances => {
        assert(numNeighbors > 0)
        val distancesAndLabels = candidates.par.map({case (label, image) => (distance(query, image), (label, image))}).toList
        val remainingCandidates = distancesAndLabels.sortBy(_._1).take(numNeighbors).map(_._2)
        neighbors(remainingDistances, remainingCandidates, query)
      }
    }
  }
  
  def predict(query: BufferedImage): Tuple2[Double, A] = {
    val (label, image) = neighbors(distances, trainData, query).head
    (distances.reverse.head._2(query, image), label)
  }
 
  // def predictFromImage(image: BufferedImage): Tuple2[Double, A] = {
  //   val path = Util.createTempFile("prediction", ".bmp")
  //   ImageIO.write(image, "bmp", path)
  //   println(path.toString)
  //   predict(path.toString)
  // }
 
  def accuracy(testData: List[Tuple2[A, BufferedImage]]): Double = {
    val predictionsAndTruths = testData.map({case (label, query) => (predict(query)._2, label)})
    val numRight = predictionsAndTruths.count({case (prediction, truth) => prediction == truth})
    numRight.toDouble / testData.size
  }
}

object NNRecognizer {
  type ImageDistance = (BufferedImage, BufferedImage) => Double
}
