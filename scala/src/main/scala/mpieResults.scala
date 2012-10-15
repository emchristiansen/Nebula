package nebula

//case class MPIEExperimentResults(val experiment: MPIEExperiment,
//                                 val resultsDataList: List[ResultsData])
//
//object MPIEExperimentResults {
//  def fromCompletedExperiment(
//    experiment: MPIEExperiment): MPIEExperimentResults = {
//    assert(experiment.alreadyRun)
//    val Some(file) = experiment.existingResultsFile
//    IO.fromJSONFile[MPIEExperimentResults](file)
//  }
//}
