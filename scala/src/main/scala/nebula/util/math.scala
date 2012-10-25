package nebula.util

///////////////////////////////////////////////////////////

object MathUtil {
  // The correct implementation of a % b.
  implicit def addMod(self: Double) = new {
    def mod(that: Double) = self - (self / that).floor * that
  }
  
  def gaussianKernel(std: Double): Seq[Seq[Double]] = {
    val width = (4 * std).ceil.toInt

    def density(pixel: Int): Double = {
      val normalizer = 1 / (std * math.sqrt(2 * math.Pi))

      val middle = (width - 1).toDouble / 2
      val unnormalizedDensity = math.exp(-math.pow(pixel - middle, 2) / (2 * math.pow(std, 2)))

      normalizer * unnormalizedDensity
    }

    // Isotropic Gaussians decompose.
    val projection = (for (i <- 0 until width) yield density(i)) toList

    for (p1 <- projection) yield {
      for (p2 <- projection) yield {
        p1 * p2
      }
    }
  }  
}