package nebula

import spray.json._
import nebula.util.JSONUtil._
import nebula.util.DMatchJsonProtocol._

///////////////////////////////////////////////////////////

trait BrownJsonProtocol extends DefaultJsonProtocol {
  implicit def brownExperimentJsonProtocol[E <% Extractor[F]: JsonFormat, M <% Matcher[F] : JsonFormat, F] =
    jsonFormat4(BrownExperiment.apply[E, M, F])
    
  implicit def brownExperimentResultsJsonProtocol[E <% Extractor[F]: JsonFormat, M <% Matcher[F] : JsonFormat, F] =
    jsonFormat2(BrownExperimentResults.apply[E, M, F])    
}

object BrownJsonProtocol extends BrownJsonProtocol