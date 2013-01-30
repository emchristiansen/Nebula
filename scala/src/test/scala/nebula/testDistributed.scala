package nebula

import nebula._
import org.scalatest.FunSuite
import javax.imageio.ImageIO
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import nebula.util._
import nebula.summary._
import nebula.wideBaseline.WideBaselineExperiment
import shapeless._
import spray.json._
import reflect.runtime.universe._
import nebula.JsonProtocols._
import nebula.brown._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestDistributed extends FunSuite {
  test("ensure implicits are found for WideBaselineExperiment") {
    val imageClasses = Seq(
      "graffiti",
      "trees",
      "jpeg",
      "boat",
      "bark",
      "bikes",
      "light",
      "wall").sorted

    val otherImages = Seq(2, 3, 4, 5, 6)

    val detectors =
      BoundedPairDetector(
        BoundedDetector(OpenCVDetector.FAST, 5000),
        200) :: HNil

    val logPolarExtractor = LogPolarExtractor(
      false,
      2,
      24,
      16,
      16,
      3,
      "Gray")

    val extractors = logPolarExtractor :: OpenCVExtractor.BRIEF :: OpenCVExtractor.SIFT :: OpenCVExtractor.SURF :: HNil

    val logPolarMatcherNCCL1 = LogPolarMatcher(
      PatchNormalizer.NCC,
      Matcher.L1,
      true,
      true,
      8)

    val logPolarMatcherNCCL2 = LogPolarMatcher(
      PatchNormalizer.NCC,
      Matcher.L2,
      true,
      true,
      8)

    val logPolarMatcherL1 = LogPolarMatcher(
      PatchNormalizer.Rank,
      Matcher.L1,
      true,
      true,
      8)

    val logPolarMatcherL2 = LogPolarMatcher(
      PatchNormalizer.Rank,
      Matcher.L2,
      true,
      true,
      8)

    val matchers = logPolarMatcherNCCL1 :: logPolarMatcherNCCL2 :: logPolarMatcherL1 :: logPolarMatcherL2 :: Matcher.L1 :: Matcher.L2 :: HNil

    val transposed = for (
      imageClass <- imageClasses;
      otherImage <- otherImages
    ) yield {
      val tuples = HListUtil.mkTuple3(detectors, extractors, matchers)

      object constructExperiment extends Poly1 {
        implicit def default[D <% PairDetector, E <% Extractor[F], M <% Matcher[F], F] = at[(D, E, M)] {
          case (detector, extractor, matcher) => {
            WideBaselineExperiment(imageClass, otherImage, detector, extractor, matcher)
          }
        }
      }

      // This lifting, combined with flatMap, filters out types that can't be used                                                                                                                                                                                                              
      // to construct experiments.                                                                                                                                                                                                                                                              
      object constructExperimentLifted extends Lift1(constructExperiment)

      val experiments = tuples flatMap constructExperimentLifted

      object constructCapstone extends Poly1 {
        implicit def default[E <% RuntimeConfig => ExperimentRunner[R] <% RuntimeConfig => StorageInfo[R]: JsonFormat: TypeTag, R <% RuntimeConfig => ExperimentSummary: TypeTag] = at[E] {
          experiment => Distributed.unsafeCapstone(experiment)
        }
      }

      object getJson extends Poly1 {
        implicit def default[E: JsonFormat] = at[E] { experiment =>
          {
            experiment.toJson
          }
        }
      }

      val capstones = experiments map constructCapstone
      val jsons = experiments map getJson
      capstones.toList zip jsons.toList
    }

    transposed.transpose
  }

  test("ensure implicits are found for BrownExperiment") {
    val datasets = Seq("liberty")
    val numMatchess = Seq(1000)
    val extractors = OpenCVExtractor.SIFT :: OpenCVExtractor.SURF :: HNil
    val matchers = Matcher.L2 :: HNil

    val transposed = for (
      dataset <- datasets;
      numMatches <- numMatchess
    ) yield {
      val tuples = HListUtil.mkTuple2(extractors, matchers)

      object constructExperiment extends Poly1 {
        implicit def default[E <% Extractor[F], M <% Matcher[F], F] = at[(E, M)] {
          case (extractor, matcher) => {
            BrownExperiment(dataset, numMatches, extractor, matcher)
          }
        }
      }

      // This lifting, combined with flatMap, filters out types that can't be used                                                                                                                                                                                                              
      // to construct experiments.                                                                                                                                                                                                                                                              
      object constructExperimentLifted extends Lift1(constructExperiment)

      val experiments = tuples flatMap constructExperimentLifted

      object constructCapstone extends Poly1 {
        implicit def default[E <% RuntimeConfig => ExperimentRunner[R] <% RuntimeConfig => StorageInfo[R]: JsonFormat: TypeTag, R <% RuntimeConfig => ExperimentSummary: TypeTag] = at[E] {
          experiment => Distributed.unsafeCapstone(experiment)
        }
      }

      object getJson extends Poly1 {
        implicit def default[E: JsonFormat] = at[E] { experiment =>
          {
            experiment.toJson
          }
        }
      }

      val capstones = experiments map constructCapstone
      val jsons = experiments map getJson
      capstones.toList zip jsons.toList
    }

    transposed.transpose
  }

  test("hlist stuff") {
    object mkBrown extends Poly2 {
      implicit def default[E <% Extractor[F], M <% Matcher[F], F, B] = at[(E, M), B] {
        case ((e, m), b) => {
          val exp = BrownExperiment("liberty", 1000, e, m)
          exp.toString
        }
      }
    }

    def foo[H <: HList, Out <: HList](h: HList)(
        implicit lift: nebula.LiftA2[mkBrown.type,shapeless.HList,shapeless.HList,Out]) {
      HListUtil.liftA2(mkBrown)(h, h)
    }
    
//    val extractors = OpenCVExtractor.SIFT :: OpenCVExtractor.SURF :: HNil
//    val matchers = Matcher.L2 :: HNil
//    
//    val tuples = HListUtil.mkTuple2(extractors, matchers)
//    
//    foo(tuples)
  }
}