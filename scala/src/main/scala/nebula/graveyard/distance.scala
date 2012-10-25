package nebula.graveyard

import nebula._

import java.io.File
import javax.imageio.ImageIO
import scala.sys.process.Process

// object Distance {
//   private def sumPixelwiseDistance(pathPair: Tuple2[String, String], 
// 		  						   distance: (Double, Double) => Double, 
// 		  						   combine: Double => Double): Double = { 
//     val Tuple2(leftPath, rightPath) = pathPair

//     def pixels(path: String): List[Double] = {
//       Pixel.getPixels(ImageIO.read(new File(path))).map(_.toDouble)
//     }

//     val left = pixels(leftPath)
//     val right = pixels(rightPath)
    
//     combine(left.zip(right).map({case (x, y) => distance(x, y)}).sum)
//   }

//   private def callMatlabExperiment(train: List[Tuple3[String, String, Boolean]], 
// 		  						   test: List[Tuple2[String, String]], cmdName: String): List[Double] = { 
//     // fill up the training file
//     val trainName = File.createTempFile("train", ".txt", new File(Global.run.tempDirectory))
//     var out = new java.io.FileWriter(trainName)
//     val tuple3ToString = (x: Tuple3[String,String,Boolean]) => 
//       out.write(x._1+" "+x._2+" "+(if(x._3 == true) "1\n" else "0\n"))
//     train.foreach(tuple3ToString(_))
//     out.close

//     // setup the testing file
//     val testName = File.createTempFile("test", ".txt", new File(Global.run.tempDirectory))
//     out = new java.io.FileWriter(testName)
//     val tuple2ToString = (x: Tuple2[String,String]) => out.write(x._1+" "+x._2+"\n")
//     test.foreach(tuple2ToString(_))
//     out.close

//     // create an output name file for the matlab script to
//     // put the outputs to
//     val outName = File.createTempFile("outFile", ".txt", new File(Global.run.tempDirectory))

//     // run the system call
//     //val matlabCmd = cmdName + "(" + trainName + "," + testName + ","+outName + ")"
//     val matlabDir = Global.run.sfsRoot + "/src/matlab"
//     val matlabCmd = "matlabDistanceHelper('"+cmdName+"','"+trainName+"','"+testName+"','"+outName+"')"
//     val fullCmd = "matlab -nodesktop -nosplash -r addpath(genpath('%s'));%s;exit".format(matlabDir, matlabCmd)
//     Util.runSystemCommand(fullCmd)

//     // read in the scores
//     val source = scala.io.Source.fromFile(outName)
//     val scores = source.mkString.split("\n").map(_.toDouble).toList
//     source.close()

//     // delete the temp files after done with them
//     if (Global.run.deleteTemporaryFiles) {
//       trainName.deleteOnExit
//       testName.deleteOnExit
//       outName.deleteOnExit
//     }

//     scores
//   }

//   def l1Experiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     def l1(x: Double, y: Double): Double = (x - y).abs

//     test.map(p => sumPixelwiseDistance(p, l1, identity))
//   }

//   def l2Experiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     def l2(x: Double, y: Double): Double = math.pow(x - y, 2)
    
//     test.map(p => sumPixelwiseDistance(p, l2, math.sqrt))
//   }

//   def eigenfaceExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     callMatlabExperiment(train, test, "eigenExperiment")
//   }

//   def sparsefaceExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     callMatlabExperiment(train, test, "sparseExperiment")    
//   }

//   def ssimExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     callMatlabExperiment(train, test, "ssimExperiment")    
//   }

//   def blbpExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val (height, width) = {
//       val image = ImageIO.read(new File(test(0)._1))
//       (image.getHeight, image.getWidth)
//     }
//     val blbp = BLBP(8, 10, (7, 7), (height, width))
//     for ((left, right) <- test) yield {
//       blbp.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }

//   def blbmExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val blbm = LBM.BLBM(16, 10, 4, 0.25)
//     for ((left, right) <- test) yield {
//       blbm.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }

//   def l0lbmExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val lbm = LBM.l0LBM(4, 0, 0.25)
//     for ((left, right) <- test) yield {
//       lbm.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }

//   def l1lbmExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val lbm = LBM.l1LBM(10, 0, 1)
//     for ((left, right) <- test) yield {
//       lbm.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }

//   def ktlbmExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val ktlbm = LBM.KTLBM(4, 1, 0.25)
//     for ((left, right) <- test) yield {
//       ktlbm.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }

//   def l0lumExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val matcher = LUM.l0LUMOCL(8, 3)
//     for ((left, right) <- test) yield {
//       matcher.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }
  
//   def l1phExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val matcher = PHMatcher(8, 32 * (8 * 8 * 3))
//     for ((left, right) <- test) yield {
//       matcher.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }
  
//   def l1ephExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val matcher = EPHMatcher(8, 8)
//     for ((left, right) <- test) yield {
//       matcher.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }  
  
//   def l1lumExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val matcher = nebula.LUM.l1LUM(8, 2)
//     for ((left, right) <- test) yield {
//       matcher.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }  

//   def slbpExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val (height, width) = {
//       val image = ImageIO.read(new File(test(0)._1))
//       (image.getHeight, image.getWidth)
//     }
//     val slbp = SLBP((7, 7), (height, width))
//     for ((left, right) <- test) yield {
//       slbp.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }

//   def flbpExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     val (height, width) = {
//       val image = ImageIO.read(new File(test(0)._1))
//       (image.getHeight, image.getWidth)
//     }
//     val flbp = FLBP(2, (7, 7), (height, width))
//     for ((left, right) <- test) yield {
//       flbp.distance(ImageIO.read(new File(left)), ImageIO.read(new File(right)))
//     }
//   }

//   def opencvLBPHExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {     
//     NativeUtil.initNativeInterface
//     for ((left, right) <- test) yield {
//       NativeInterface.OpenCV.INSTANCE.lbphDistance(left, right)
//     }
//   }    
  
//   def lbpExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     callMatlabExperiment(train, test, "lbpExperiment")
//   }

//   def lbpWolfExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     callMatlabExperiment(train, test, "lbpWolfExperiment")
//   }

//   def shapeContextExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     callMatlabExperiment(train, test, "shapeContextExperiment")
//   }

//   def lbpShapeContextExperiment(train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = {
//     callMatlabExperiment(train, test, "lbpShapeContextExperiment")
//   }

//   def callDistance(distance: String, train: List[Tuple3[String, String, Boolean]], test: List[Tuple2[String, String]]): List[Double] = { 
//     distance match { 
//       case "L1" => Distance.l1Experiment(train, test)
//       case "L2" => Distance.l2Experiment(train, test)
//       case "Eigenface" => Distance.eigenfaceExperiment(train, test)
//       case "Sparseface" => Distance.sparsefaceExperiment(train, test)
//       case "SSIM" => Distance.ssimExperiment(train, test)
//       case "LBP" => Distance.lbpExperiment(train, test)
//       case "SLBP" => Distance.slbpExperiment(train, test)
//       case "FLBP" => Distance.flbpExperiment(train, test)
//       case "BLBP" => Distance.blbpExperiment(train, test)
//       case "BLBM" => Distance.blbmExperiment(train, test)
//       case "L0LBM" => Distance.l0lbmExperiment(train, test)
//       case "L1LBM" => Distance.l1lbmExperiment(train, test)
//       case "KTLBM" => Distance.ktlbmExperiment(train, test)
//       case "L0LUM" => Distance.l0lumExperiment(train, test)
//       case "L1LUM" => Distance.l1lumExperiment(train, test)
//       case "L1PH" => Distance.l1phExperiment(train, test)
//       case "L1EPH" => Distance.l1ephExperiment(train, test)
//       case "ShapeContext" => Distance.shapeContextExperiment(train, test)
//       case "LBPShapeContext" => Distance.shapeContextExperiment(train, test)
//       case "LBPWolf" => Distance.lbpWolfExperiment(train, test)
//       case "OCVLBPH" => Distance.opencvLBPHExperiment(train, test)
//       case _ => throw new IllegalArgumentException("distance not defined: %s".format(distance))
//     }
//   }
// }
