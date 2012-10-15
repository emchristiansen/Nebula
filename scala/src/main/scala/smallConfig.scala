package nebula

case class SmallBaselineExperiment(
  val searchRadius: Int,
  val imageClass: String,
  val extractor: Extractor,
  val matcher: Matcher)

object SmallBaselineExperiment {
  implicit def implicitExperiment(self: SmallBaselineExperiment): Experiment =
    new Experiment {
      override def name = "SmallBaselineExperiment"
      override def parameters = Seq(
        ("SR", self.searchRadius.toString),
        ("IC", self.imageClass),
        ("E", Util.abbreviate(self.extractor)),
        ("M", Util.abbreviate(self.matcher)))
    }

  implicit def implicitImagePairLike(self: SmallBaselineExperiment): ImagePairLike with HasGroundTruth[FlowField] =
    new ImagePairLike with HasGroundTruth[FlowField] {
      override val SmallBaselinePair(leftImage, rightImage, groundTruth) = SmallBaselinePair.fromName(
        Global.run[RuntimeConfig].projectChildPath("data/middleburyImages"),
        self.imageClass)
    }
}