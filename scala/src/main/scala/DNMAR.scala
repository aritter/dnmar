package dnmar;

import scalala.scalar._;
import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.Numerics._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._;

import scala.collection.mutable.ListBuffer
import scala.util.Random

import math._
import java.io._

class HiddenVariablesHypothesisTwoSided(postZ:DenseMatrix[Double], postObs:DenseVector[Double], zPart:List[Int], rPart:Array[Double], obs:Array[Double], sPartial:Double, val score:Double) extends Hypothesis {
  def z:Array[Int] = {
    return zPartial.reverse.toArray
  }

  var rPartial = rPart

  var zPartial = zPart 

  def sucessors:Array[Hypothesis] = {
    val result = new ListBuffer[Hypothesis]

    if(zPartial.length == postZ.numRows) {
      return result.toArray
    }

    for(rel <- 0 until postZ.numCols) {
      val newZ = rel :: zPartial
      var newSpartial = sPartial + postZ(newZ.length-1, rel)
      var newScore = newSpartial

      //Update rPartial
      val newRpartial = rPartial.clone
      newRpartial(rel) = 1.0

      //Add max scores for rest of z's (note: this is an upper bound / admissible heuristic)
      for(i <- newZ.length until postZ.numRows) {
	newScore += postZ(i,::).max
      }

      //Observation factors
      for(rel <- 0 until postZ.numCols) {
	if(newRpartial(rel) == 1.0) {
	  newScore += postObs(rel)
	} else if(postObs(rel) > 0.0) {
	  //Find the bess possible way of changing one of the remaining z's (note: it is possible we could use the same z to satisfy 2 different relations, but hey this is an upper bound!)
	  var maxValue = Double.NegativeInfinity
	  for(i <- newZ.length until postZ.numRows) {
	    val possibleValue = postObs(rel) + postZ(i,rel) - postZ(i,::).max
	    if(possibleValue > maxValue) {
	      maxValue = possibleValue
	    }
	  }
	  if(maxValue > 0.0) {
	    newScore += maxValue
	  }
	}
      }

      result += new HiddenVariablesHypothesisTwoSided(postZ, postObs, newZ, newRpartial, obs, newSpartial, newScore)
    }

    return result.toArray
  }
}

class DNMAR(data:EntityPairData) extends Parameters(data) {
  //Randomly permute the training data
  //var training = Random.shuffle((0 until data.data.length).toList).filter((e12) => true)

  //TODO: seperate training for theta & phi?

  val mid2name = new FreebaseUtils.Mid2Name("../data/mid2name")

  def train(nIter:Int) = {
    train(nIter, null)
  }

  def train(nIter:Int, fw:FileWriter) = {
    for(i <- 0 until nIter) {
      //Randomly permute the training data
      var training = Random.shuffle((0 until data.data.length).toList).filter((e12) => true)

      //println("iteration " + i)
      var j = 0
      for(e12 <- training) {
	if(Constants.DEBUG && data.data(e12).features.length > 10) {
	  print("entity pair " + j + "/" + training.length + ":" + data.data(e12).features.length + "\n")
	}
	//Run le inference
	val iAll    = inferAll(data.data(e12))
	var iHidden:EntityPair = null  //Just needed to asign it something temporarily...
	var score = 0.0

	if(trainSimple) {
	  iHidden = inferHiddenMULTIR(data.data(e12))
	  data.data(e12).z = iHidden.z
	} else {
	  //val result = inferHiddenLocalSearch(data.data(e12), 10)

	  //val result = inferHiddenBranchAndBound(data.data(e12))
          //val result = inferHiddenLocalSearch(data.data(e12), 20)
	  val result = inferHiddenLocalSearch(data.data(e12), 1)

	  iHidden = result._1
	  score   = result._2

	  data.data(e12).z = iHidden.z

	  //Figure out search error (in cases where we can efficiently do exact inference)
	  //if(fw != null && data.data(e12).features.length > 1 && data.data(e12).features.length < 100) {
	  if(fw != null && data.data(e12).features.length > 1 && data.data(e12).features.length < 500) {
	    Utils.Timer.start("inferenceTime")
	    val (iHidden1rs,  score1rs) = inferHiddenLocalSearch(data.data(e12), 1)
	    val time1rs = Utils.Timer.reset("inferenceTime")

	    Utils.Timer.start("inferenceTime")
	    val (iHidden10rs, score10rs) = inferHiddenLocalSearch(data.data(e12), 10)
	    val time10rs = Utils.Timer.reset("inferenceTime")

	    Utils.Timer.start("inferenceTime")
	    val (iHidden20rs, score20rs) = inferHiddenLocalSearch(data.data(e12), 20)
	    val time20rs = Utils.Timer.reset("inferenceTime")

	    Utils.Timer.start("inferenceTime")
	    val (iHidden1kBeam, score1kBeam) = inferHiddenAstar(data.data(e12), 1000)
	    val time1kBeam = Utils.Timer.reset("inferenceTime")

	    Utils.Timer.start("inferenceTime")
	    val (iHiddenBNB, scoreBNB) = inferHiddenBranchAndBound(data.data(e12))
	    val timeBNB = Utils.Timer.reset("inferenceTime")

	    Utils.Timer.start("inferenceTime")
	    val (iHiddenExact, scoreExact) = inferHiddenAstar(data.data(e12), -1)
	    val timeExact = Utils.Timer.reset("inferenceTime")
	    fw.write(List(score1rs, time1rs, score10rs, time10rs, score20rs, time20rs, score1kBeam, time1kBeam, scoreBNB, timeBNB, scoreExact, timeExact, data.data(e12).features.length).map(_.toString).reduceLeft(_ + "\t" + _) + "\n")
	    fw.flush
	  }
	}

	if(updateTheta && j % 10 != 0) {
        //if(updateTheta) {
	  updateTheta(iAll, iHidden)
	}
	if(updatePhi && j % 10 == 0) {
        //if(updatePhi) {
	  updatePhi(iAll, iHidden)
	}
	j += 1
      }
    }
  }

  def simpleObsScore(ep:EntityPair):DenseVector[Double] = {
    val postObs     = DenseVector.zeros[Double](data.nRel)

    for(r <- 0 until data.nRel) {
      if(r == data.relVocab("NA")) {
	//postObs(r) = -4.0
	//postObs(r) = -2.0
	//postObs(r) = 0.0
	postObs(r) = -0.0
      } else if(ep.obs(r) == 0.0) {
	postObs(r) = -5.0
      } else {
	//postObs(r) = 100.0
	postObs(r) = 10000.0
      }
    }
    postObs
  }

  def simpleObsScoreKbp(ep:EntityPair):DenseVector[Double] = {
    val postObs     = DenseVector.zeros[Double](data.nRel)

    for(r <- 0 until data.nRel) {
      if(r == data.relVocab("NA")) {
	//postObs(r) = -4.0
	//postObs(r) = -2.0
	//postObs(r) = 0.0
	postObs(r) = -0.0
      } else if(ep.obs(r) == 0.0) {
	postObs(r) = -100.0
      } else {
	//postObs(r) = 100.0
	postObs(r) = 10000.0
      }
    }
    postObs
  }

  def simpleObsScoreNER(ep:EntityPair):DenseVector[Double] = {
    val postObs     = DenseVector.zeros[Double](data.nRel)
    for(r <- 0 until data.nRel) {
      if(r == data.relVocab("NA")) {
	postObs(r) = -0.0
      } else if(ep.obs(r) == 0.0) {
	//postObs(r) = -10.0
	postObs(r) = -100.0
	//postObs(r) = -5.0
      } else {
	//postObs(r) = 100.0
	postObs(r) = 10000.0
      }
    }
    postObs
  }

  //This version doesn't take other values into account or anything.   Just scales by the entity frequency as a model of missing text / DB probability
  def fbObsScore2(ep:EntityPair):DenseVector[Double] = {
    val postObs = DenseVector.zeros[Double](data.nRel)
    val e1  = data.entityVocab(ep.e1id)
    val e2  = data.entityVocab(ep.e2id)

    var allSenseRels = Set[String]()

    for(r <- 0 until data.nRel) {
      if(r == data.relVocab("NA")) {
	postObs(r) = 0.0
      } else if(ep.obs(r) == 0.0) {
          postObs(r) = -5.0
      } else {
	postObs(r) = 1000.0
      }

      //Scale based on the entity frequency...
      postObs(r) *= 0.01 * (1.0 + math.min(data.fbData.entityFreq(e1), data.fbData.entityFreq(e2)))
    }

    //println(e1 + "\t" + e2 + "\t" + (0 until postObs.length).map(i => i + ":" + data.relVocab(i) + postObs(i)).mkString("\t").filter(x => x != -5))
    postObs
  }

  def fbObsScore3(ep:EntityPair):DenseVector[Double] = {
    val postObs = DenseVector.zeros[Double](data.nRel)
    val e1  = data.entityVocab(ep.e1id)
    val e2  = data.entityVocab(ep.e2id)

    var allSenseRels = Set[String]()

    for(sense1 <- mid2name(e1).flatMap(x => mid2name(x))) {
      for(sense2 <- mid2name(e2).flatMap(x => mid2name(x))) {
	for(rel <- data.fbData.getRels(sense1, sense2)) {
	  allSenseRels += rel
	}
      }
    }

    for(r <- 0 until data.nRel) {
      if(r == data.relVocab("NA")) {
	//postObs(r) = 0.0
	postObs(r) = -1.0
      } else if(ep.obs(r) == 0.0) {
	//Simple missing data model
	val rel = data.relVocab(r)
	val values = data.fbData.getA2s(e1,rel);

	postObs(r) = -5.0
      } else {
	val rel = data.relVocab(r)
	if(rel == "/loction/location/contains" ||
	   rel == "/people/person/place_lived" ||
	   rel == "/people/person/nationality" ||
	   rel == "/people/person/children" ||
	   rel == "/location/neighborhood/neighborhood_of" ||
	   rel == "/business/person/company") {
	  //postObs(r) =  400.0
	  postObs(r) =  1000.0
	} else if(rel == "/location/country/capitol" ||
		  rel == "/location/country/administrative_divisions" ||
		  //rel == "/people/person/place_of_birth" ||
		  rel == "/people/person/place_of_death" ||
		  rel == "/location/us_state/capitol") {
	  //postObs(r) =  50.0
	  postObs(r) =  200.0
	} else {
	  //postObs(r) = 200.0
	  postObs(r) = 500.0
	}
	//postObs(r) = 10000.0
      }

      //Scale based on the entity frequency...
      //if(postObs(r) < 100) {
      if(postObs(r) < 0) {
	//postObs(r) *= 0.01 * (1.0 + math.min(data.fbData.entityFreq(e1), data.fbData.entityFreq(e2)))
	postObs(r) *= 0.01 * (1.0 + math.min(data.fbData.entityFreq(e1), data.fbData.entityFreq(e2)))
	//println(postObs(r))
      }
    }

    //println(e1 + "\t" + e2 + "\t" + (0 until postObs.length).map(i => i + ":" + data.relVocab(i) + postObs(i)).mkString("\t").filter(x => x != -5))
    postObs
  }

  def fbObsScoreNER(ep:EntityPair):DenseVector[Double] = {
    val postObs = DenseVector.zeros[Double](data.nRel)
    val e1  = data.entityVocab(ep.e1id)
    val e2  = data.entityVocab(ep.e2id)

    var allSenseRels = Set[String]()

    for(sense1 <- mid2name(e1).flatMap(x => mid2name(x))) {
      for(sense2 <- mid2name(e2).flatMap(x => mid2name(x))) {
	for(rel <- data.fbData.getRels(sense1, sense2)) {
	  allSenseRels += rel
	}
      }
    }

    for(r <- 0 until data.nRel) {
      if(r == data.relVocab("NA")) {
	//postObs(r) = 0.0
	postObs(r) = -0.0
      } else if(ep.obs(r) == 0.0) {
	//Simple missing data model
	val rel = data.relVocab(r)
	val values = data.fbData.getA2s(e1,rel);

	postObs(r) = -15.0
      } else {
	val rel = data.relVocab(r)
	//println(rel)
	if(rel == "person" ||
	   rel == "geo-loc") {
	  //postObs(r) =  400.0
	  postObs(r) =  10000.0
	} else {
	  //postObs(r) = 200.0
	  postObs(r) = 10000.0
	}
	//postObs(r) = 10000.0
      }

      //Scale based on the entity frequency...
      //if(postObs(r) < 100) {
      if(postObs(r) < 0) {
	//postObs(r) *= 0.01 * (1.0 + math.min(data.fbData.entityFreq(e1), data.fbData.entityFreq(e2)))
	postObs(r) *= 1.0 * (1.0 + math.min(data.fbData.entityFreq(e1), data.fbData.entityFreq(e2)))
	//println(postObs(r))
      }
    }
    //println(e1 + "\t" + e2 + "\t" + (0 until postObs.length).map(i => i + ":" + data.relVocab(i) + postObs(i)).mkString("\t").filter(x => x != -5))
    postObs
  }

  def fbObsScore(ep:EntityPair):DenseVector[Double] = {
    val postObs = DenseVector.zeros[Double](data.nRel)
    val e1  = data.entityVocab(ep.e1id)
    val e2  = data.entityVocab(ep.e2id)

    var allSenseRels = Set[String]()

    for(sense1 <- mid2name(e1).flatMap(x => mid2name(x))) {
      for(sense2 <- mid2name(e2).flatMap(x => mid2name(x))) {
	for(rel <- data.fbData.getRels(sense1, sense2)) {
	  allSenseRels += rel
	}
      }
    }

    /*
    val dataRels = (0 until ep.obs.length).filter(i => ep.obs(i) != 0).map(i => data.relVocab(i))
    if(dataRels.length != allSenseRels.toList.length) {
      println(mid2name(e1).mkString + "\t" + mid2name(e1).flatMap(x => mid2name(x)))
      println(mid2name(e2).mkString + "\t" + mid2name(e2).flatMap(x => mid2name(x)))
      println("allSenseRels=\t" + allSenseRels)
      println("dataRels=\t" + (0 until ep.obs.length).filter(i => ep.obs(i) != 0).map(i => data.relVocab(i)).mkString("\t"))
    }
    */
    
    for(r <- 0 until data.nRel) {
      if(r == data.relVocab("NA")) {
	postObs(r) = 0.0
      } else if(ep.obs(r) == 0.0) {
	//Simple missing data model
	val rel = data.relVocab(r)

	/*
	val values = if(rel == "/location/location/contains") {
	  data.fbData.getA1s(e2,rel)
	} else {
	  data.fbData.getA2s(e1,rel)
	}
	*/
	val values = data.fbData.getA2s(e1,rel);


	//TODO: this whole thing may need some debugging...
	if(allSenseRels.contains(rel)) {
	  //Q: Is there another sense in which this is true?
	  //postObs(r) = -1.0
	  //postObs(r) = 10000.0
	  postObs(r) = 10.0
	//} else if(values.filter(x => data.fbData.aContainsB(e2,x)).length > 0) {
	} else if(data.relVocab(r) != "/people/person/nationality" && 
		  (values.filter(x => data.fbData.aContainsB(e2,x)).length > 0 || values.filter(x => data.fbData.aContainsB(x,e2)).length > 0)) {
	  //Another value is present, and it is contained by e2
	  postObs(r) = 10.0
 	//} else if(values.filter(x => !data.fbData.aContainsB(e2,x)).length > 0) {
	} else if((data.relVocab(r) == "/people/person/nationality" && values.length > 0) || 
		  (values.filter(x => !data.fbData.aContainsB(e2,x)).length > 0 && values.filter(x => !data.fbData.aContainsB(x,e2)).length > 0)) {
	  //Q: Is there another value for this rel?
	  //postObs(r) = -10.0
          postObs(r) = -5.0
	} else {
	  postObs(r) = -5.0
	}
      } else {
	postObs(r) = 10000.0
      }

      //Scale based on the entity frequency...
      if(postObs(r) < 100) {
	postObs(r) *= 0.01 * (1.0 + math.min(data.fbData.entityFreq(e1), data.fbData.entityFreq(e2)))
	//println(postObs(r))
      }
    }

    //println(e1 + "\t" + e2 + "\t" + (0 until postObs.length).map(i => i + ":" + data.relVocab(i) + postObs(i)).mkString("\t").filter(x => x != -5))
    postObs
  }

  //Computes the score for each of the observation factors based on observed data in Freebase
  def computeObsScore(ep:EntityPair):DenseVector[Double] = {
    //Posterior distribution over observations
    val postObs     = DenseVector.zeros[Double](data.nRel)
    for(r <- 0 until data.nRel) {
      if(r == data.relVocab("NA")) {
	//postObs(r) = -4.0
	//postObs(r) = -2.0
	postObs(r) = 0.0
      } else if(ep.obs(r) == 0.0) {
	var s = 0.0
        s += phiMid(ep.e1id)
        s += phiMid(ep.e2id)
	s += phiMid(data.entityVocab.size + r)
	s += phiMid(phiMit.length-1)
        if(s > -5.0) {   //Don't let phiMid grow > 0
          s = -5.0
        }
	postObs(r) = s
	//postObs(r) = -5.0
      } else {
        /*
         * NOTE: this should work for finding the MAP result, but need to add Phi for all possible MIT cases to get the correct score....
         */
	var s = 0.0
       	s -= phiMit(data.entityVocab.size + r)
	s -= phiMit(phiMid.length-1)
       	s -= phiMit(data.entityVocab.size + r)
	s -= phiMit(phiMid.length-1)
        if(s < 5.0) {   //Don't let phiMit grow > -5
          s = 5.0
        }
	postObs(r) = s
	//postObs(r) = 100.0
      }
    }
    postObs
  }

  def inferHiddenBranchAndBound(ep:EntityPair):Tuple2[EntityPair,Double] = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHiddenBranchAndBound")
    }

    val postZ = DenseMatrix.zeros[Double](ep.features.length, data.nRel)
    for(i <- 0 until ep.features.length) {
      postZ(i,::) := (theta * ep.features(i)).toDense
    }
    val postObs = simpleObsScore(ep)
    val (iHidden1rs, score1rs) = inferHiddenLocalSearch(ep, 1)
    val result = inferHiddenBranchAndBound(ep, postZ, postObs, iHidden1rs, score1rs, new Array[Double](postZ.numCols), List(), 0.0)

    if(Constants.TIMING) {
      Utils.Timer.stop("inferHiddenBranchAndBound")
    }

    result
  }

  def inferHiddenBranchAndBound(ep:EntityPair, postZ:DenseMatrix[Double], postObs:DenseVector[Double], epbest:EntityPair, scorebest:Double, rPartial:Array[Double], zPartial:List[Int], sPartial:Double):Tuple2[EntityPair,Double] = {
    var epBest    = epbest
    var scoreBest = scorebest
    for(rel <- 0 until postZ.numCols) {
      val newZ = rel :: zPartial
      val newSpartial = sPartial + postZ(newZ.length-1, rel)
      var newScore    = newSpartial
      
      //Update rPartial
      val newRpartial = rPartial.clone
      newRpartial(rel) = 1.0

      //Add max scores for rest of z's (note: this is an upper bound / admissible heuristic)
      for(i <- newZ.length until postZ.numRows) {
	newScore += postZ(i,::).max
      }

      //Observation factors
      for(rel <- 0 until postZ.numCols) {
	if(newRpartial(rel) == 1.0) {
	  newScore += postObs(rel)
	} else if(postObs(rel) > 0.0) {
	  //Find the bess possible way of changing one of the remaining z's (note: it is possible we could use the same z to satisfy 2 different relations, but hey this is an upper bound!)
	  var maxValue = Double.NegativeInfinity
	  for(i <- newZ.length until postZ.numRows) {
	    val possibleValue = postObs(rel) + postZ(i,rel) - postZ(i,::).max
	    if(possibleValue > maxValue) {
	      maxValue = possibleValue
	    }
	  }
	  if(maxValue > 0.0) {
	    newScore += maxValue
	  }
	}
      }

      if(newZ.length == postZ.numRows && newScore > scoreBest) {
	//Found a better full assignment
	epBest    = new EntityPair(ep.e1id, ep.e2id, ep.features, DenseVector(newRpartial).t, DenseVector(newZ.toArray), null, ep.obs)
	scoreBest = newScore
      } else if(newScore > scoreBest) {
	val results = inferHiddenBranchAndBound(ep, postZ, postObs, epBest, scoreBest, newRpartial, newZ, newSpartial)
	epBest    = results._1
	scoreBest = results._2
      }
    }
    return (epBest, scoreBest)
  }

  def inferHiddenLocalSearch(ep:EntityPair, nRandomRestarts:Int):Tuple2[EntityPair,Double] = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHiddenLocalSearch")
    }

    //println("N:" + ep.features.length)

    val postZ  = DenseMatrix.zeros[Double](ep.features.length, data.nRel)
    for(i <- 0 until ep.features.length) {
      postZ(i,::) := (theta * ep.features(i)).toDense
      //Normalize (note: this isn't necessary, except for analasys purposes and generating P/R on training data...)
      val logExpSum = MathUtils.LogExpSum(postZ(i,::).toArray)
      postZ(i,::) -= logExpSum
    }
    ep.postZ = postZ

    var bestZ:DenseVector[Int]      = null
    var bestRel:DenseVectorRow[Double] = null
    var bestScore                   = Double.NegativeInfinity

    //val postObs = simpleObsScore(ep)
    //val postObs = simpleObsScoreKbp(ep)
    //val postObs = fbObsScore(ep)
    //val postObs = fbObsScore2(ep)
    val postObs = fbObsScore3(ep)
    //val postObs = simpleObsScoreNER(ep)
    //val postObs = fbObsScoreNER(ep)

    for(n <- 0 until nRandomRestarts) {
      val z       = DenseVector.zeros[Int](postZ.numRows)
      val rel     = DenseVector.zeros[Double](postZ.numCols).t
      val rCounts = DenseVector.zeros[Int](postZ.numCols)
      var score = 0.0

      //Random initialization
      for(i <- 0 until z.length) {
	//z(i) = scala.util.Random.nextInt(postObs.length)
	z(i) = MathUtils.Sample(scalala.library.Library.exp(postZ(i,::)).toArray)
	score += postZ(i,z(i))
	rCounts(z(i)) += 1
	rel(z(i)) = 1.0
      }
      for(r <- 0 until rCounts.length) {
	if(rCounts(r) > 0) {
	  score += postObs(r)
	}
      }

      var changed = false
      do {
	//Recompute Deltas
	if(Constants.TIMING) {
	  Utils.Timer.start("re-computing deltas")
	}

	//First search operator (change one variable)
	val deltas = DenseMatrix.zeros[Double](postZ.numRows, postZ.numCols)
//Compute this in parallel over rows?
//	for(i <- (0 until postZ.numRows)) {
//	  for(r <- 0 until postZ.numCols) {		
	for(r <- (0 until postZ.numCols).par) {		//Compute this in parallel over columns?
	  for(i <- (0 until postZ.numRows)) {
	    if(r != z(i)) {
	      deltas(i,r) = postZ(i,r) - postZ(i,z(i))
	      if(rCounts(r) == 0) {
		//This will be the first instance of r to be extracted...
		deltas(i,r) += postObs(r)
	      }
	      if(rCounts(z(i)) == 1) {
		//z(i) is the last instance of r remaining...
		deltas(i,r) -= postObs(z(i))
	      }
	    } else {
	      deltas(i,r) = 0.0
	    }
	  }
	}

	//Second search operator (switch all instances of relation r to NA)
	//val deltasNA = DenseVector.zeros[Double](postZ.numCols)
	val deltasAggregate = DenseMatrix.zeros[Double](postZ.numCols, postZ.numCols)
	for(r1 <- (0 until postZ.numCols).par) {
//	for(r1 <- (0 until postZ.numCols)) {
	  for(r2 <- 0 until postZ.numCols) {
	    if(rCounts(r1) > 0 && r1 != r2) {
	      for(i <- 0 until postZ.numRows) {
		if(z(i) == r1) {
		  deltasAggregate(r1,r2) += postZ(i,r2) - postZ(i,r1)
		}
	      }
	      deltasAggregate(r1,r2) -= postObs(r1)
	      if(rCounts(r2) == 0) {
		deltasAggregate(r1,r2) += postObs(r2)
	      }
	    }
	  }
	}

	if(Constants.TIMING) {
	  Utils.Timer.stop("re-computing deltas")
	}

	changed = false

	val (i, newRel)    = deltas.argmax
	val oldRel         = z(i)
	val delta          = deltas(i,newRel)
	val deltaAggregate = deltasAggregate.max

	//Check which search operator provides the greatest score delta
	if(deltaAggregate > delta && deltaAggregate > 0) {
	  //Change all instances of the max deltaNA relation to NA 
	  score += deltaAggregate

	  val (r1, r2)  = deltasAggregate.argmax
	  for(i <- 0 until z.length) {
	    if(z(i) == r1) {
	      z(i) = r2
	      rCounts(r2) += 1
	    }
	  }
	  rCounts(r1) = 0
	  rel(r1) = 0.0
	  rel(r2) = 1.0
	  changed = true
	} else if(oldRel != newRel && delta > 0) {
	  score += delta
	  z(i) = newRel
	  rCounts(newRel) += 1
	  rel(newRel) = 1.0
	  rCounts(oldRel) -= 1
	  if(rCounts(oldRel) == 0.0) {
	    rel(oldRel) = 0
	  }
	  changed = true
	}
      } while(changed)

      if(score > bestScore) {
	bestScore = score
	bestZ     = z
	bestRel   = rel
      }
    }


    if(Constants.DEBUG) {
      println("constrained score=\t"    + bestScore)
      println("constrained result.z=\t" + bestZ.toList.map((r) => data.relVocab(r)))
      println("constrained rel=\t"      + (0 until bestRel.length).filter((r) => bestRel(r) == 1.0).map((r) => data.relVocab(r)))
      println("constrained obs=\t"      + (0 until ep.obs.length).filter((r) => ep.obs(r) == 1.0).map((r) => data.relVocab(r)))
    }

    val result = new EntityPair(ep.e1id, ep.e2id, ep.features, bestRel, bestZ, null, ep.obs)
    
    if(Constants.TIMING) {
      Utils.Timer.stop("inferHiddenLocalSearch")
    }

    (result, bestScore)
  }

  def inferHiddenAstar(ep:EntityPair):Tuple2[EntityPair,Double] = {
    inferHiddenAstar(ep, -1)
  }

  def inferHiddenAstar(ep:EntityPair, beamSize:Int):Tuple2[EntityPair,Double] = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHiddenAstar")
    }

    val postZ  = DenseMatrix.zeros[Double](ep.features.length, data.nRel)

    for(i <- 0 until ep.features.length) {
      postZ(i,::) := (theta * ep.features(i)).toDense
    }

    //Posterior distribution over observations
    val postObs = simpleObsScore(ep)

    val rPartial = new Array[Double](data.nRel)
    var sPartial = 0.0
    val bs = new BeamSearch(new HiddenVariablesHypothesisTwoSided(postZ, postObs, Nil, rPartial, ep.obs.toArray, sPartial, sPartial), beamSize);

    while(bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].z.length < ep.features.length) {
      bs.UpdateQ
    }

    val rel    = DenseVector.zeros[Double](data.nRel).t
    for(r <- bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].z.toArray) {
      rel(r) = 1.0
    }

    val score     = bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].score

    if(Constants.DEBUG) {
      val z         = bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].z
      val rPartial  = bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].rPartial
      println("constrained score=\t"  + score)
      println("constrained result.z=\t" + z.toList.map((r) => data.relVocab(r)))
      println("constrained rel=\t"      + (0 until rel.length).filter((r) => rel(r) == 1.0).map((r) => data.relVocab(r)))
      println("constrained rPartial=\t" + (0 until rel.length).filter((r) => rPartial(r) == 1.0).map((r) => data.relVocab(r)))
      println("constrained obs=\t" + (0 until rel.length).filter((r) => ep.obs(r) == 1.0).map((r) => data.relVocab(r)))
    }

    val result = new EntityPair(ep.e1id, ep.e2id, ep.features, rel, DenseVector(bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].z.toArray), null, ep.obs)

    if(Constants.TIMING) {
      Utils.Timer.stop("inferHiddenAstar")
    }

    (result, score)
  }

  def inferHiddenMULTIR(ep:EntityPair):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHiddenMULTIR")
    }
    val z      = DenseVector.zeros[Int](ep.features.length)
    val zScore = DenseVector.zeros[Double](ep.features.length)
    val postZ  = DenseMatrix.zeros[Double](ep.features.length, data.nRel)

    //First pre-compute postZ
    for(i <- 0 until ep.features.length) {
      postZ(i,::) := (theta * ep.features(i)).toDense
    }

    val covered = DenseVector.zeros[Boolean](ep.features.length)     //Indicates whether each mention is already assigned...
    var nCovered = 0
    for(rel <- 0 until ep.rel.length) {
      if(ep.obs(rel) == 1.0 && nCovered < ep.features.length) {
	val scores = postZ(::,rel)
	scores(covered) := Double.NegativeInfinity
	val best   = scores.argmax
	z(best)      = rel
	zScore(best) = scores.max
	covered(best) = true
	nCovered += 1
      }
    }

    for(i <- 0 until ep.features.length) {
      if(!covered(i)) {
	//Whatever....
	for(rel <- 0 until ep.rel.length) {
	  if(ep.obs(rel) == 0 && rel != data.relVocab("NA")) {
	    postZ(i,rel) = Double.MinValue
	  }
	}

	z(i)      = postZ(i,::).argmax
	zScore(i) = postZ(i,::).max
      }
    }
    if(Constants.DEBUG) {
      println("constrained result.z=" + z.toList.map((r) => data.relVocab(r)))
      println("constrained obs=\t" + (0 until ep.rel.length).filter((r) => ep.obs(r) == 1.0).map((r) => data.relVocab(r)))
    }

    val result = new EntityPair(ep.e1id, ep.e2id, ep.features, ep.rel, z, zScore)

    if(Constants.TIMING) {
      Utils.Timer.stop("inferHiddenMULTIR")
    }
    result
  }

  /*
   * Greedy search for best overall assignment to z, aggregate rel and obs
   * (1) find best assignment to z
   * (2) compute rel (deterministically)
   * (3) predict max observation value for each fact 
   */
  def inferAll(ep:EntityPair):EntityPair = {
    inferAll(ep, false)
  }

  def inferAll(ep:EntityPair, useAverage:Boolean):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferAll")
    }
    val z      = DenseVector.zeros[Int](ep.features.length)
    //val postZ  = new Array[SparseVector[Double]](ep.features.length)
    val postZ  = DenseMatrix.zeros[Double](ep.features.length, data.nRel)
    val zScore = DenseVector.zeros[Double](ep.features.length)
    val rel    = DenseVector.zeros[Double](data.nRel).t

    for(i <- 0 until ep.features.length) {
      if(useAverage) {
	postZ(i,::) := (theta_average * ep.features(i)).toDense
      } else {
	postZ(i,::) := (theta * ep.features(i)).toDense
      }

      //Normalize?
      //val logExpSum = MathUtils.LogExpSum(postZ(i,::).toArray)
      //postZ(i,::) -= logExpSum
      
      z(i) = postZ(i,::).argmax
      zScore(i) = postZ(i,::).max

      //println(scalala.library.Library.exp(postZ(i,::)))
      //println(z(i))
      //println(math.exp(zScore(i)))

      //Set the aggregate variables
      rel(z(i)) = 1.0
    }

    val postObs = DenseVector.zeros[Double](data.nRel)
    val newObs  = DenseVector.zeros[Double](data.nRel)

    //NOTE: this doesn't really do anything now...
    for(r <- 0 until data.nRel) {
      if(rel(r) == 1.0) {
	var s = 0.0
	s -= phiMid(ep.e1id)
	s -= phiMid(ep.e2id)
	s -= phiMid(data.entityVocab.size + r)
	s -= phiMid(phiMid.length-1)	//Bias feature
	postObs(r) = s
      } else {
        var s = 0.0
	s += phiMit(ep.e1id)
	s += phiMit(ep.e2id)
	s += phiMit(data.entityVocab.size + r)
	s += phiMit(phiMit.length-1)	//Bias feature
	postObs(r) = s        
      }

      //if(rel(r) == 1.0 && postObs(r) > 0.0) {
      if(postObs(r) > 0) {
	newObs(r) = 1.0
      }
    }

    if(Constants.DEBUG) {
      val rels  = rel
      println("unconstrained result.z=" + z.toList.map((r) => data.relVocab(r)))
      println("unconstrained rel=" + (0 until rels.length).filter((r) => rels(r) == 1.0).map((r) => data.relVocab(r)))
      println("unconstrained obs=" + (0 until newObs.length).filter((r) => newObs(r) == 1.0).map((r) => data.relVocab(r)))
      println("unconstrained postObs=" + postObs.toList)
    }

    //TODO: get rid of zScore, replace with postZ...
    val result = new EntityPair(ep.e1id, ep.e2id, ep.features, rel, z, zScore, newObs)
    result.postObs = postObs

    if(Constants.TIMING) {
      Utils.Timer.stop("inferAll")
    }
    result
  }
}
