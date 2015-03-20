package dnmar;

import scalala.scalar._;
import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._;

import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStream

import cc.factorie.protobuf.DocumentProtos.Relation
import cc.factorie.protobuf.DocumentProtos.Relation.RelationMentionRef

import scala.util.Random

import math.abs

import java.io._

abstract class Parameters(val data:EntityPairData) {
  val nRel  = data.nRel
  val nFeat = data.nFeat

  /*********************************************************************
   * THETA
   *********************************************************************
   */
  val theta         = DenseMatrix.zeros[Double](nRel,nFeat+1)
				//Innitialize bias feature for "NA"
  //theta(data.relVocab("NA"), nFeat) = -10.0
  //theta(data.relVocab("NA"), nFeat) = -100.0
  val theta_sum     = DenseMatrix.zeros[Double](nRel,nFeat+1)

  var theta_average = DenseMatrix.zeros[Double](nRel,nFeat+1)

  var nUpdates = 1.0

  def computeThetaAverage {
    if(Constants.TIMING) {
      Utils.Timer.start("computeThetaAverage")
    }

    theta_average = theta - (theta_sum / nUpdates)

    if(Constants.TIMING) {
      Utils.Timer.stop("computeThetaAverage")
    }
  }

  def resetTheta {
    theta         := 0.0
    theta_sum     := 0.0
    theta_average := 0.0
    nUpdates      = 0.0
  }

  def printTheta {
    if(Constants.TIMING) {
      Utils.Timer.start("printTheta")
    }

    val thetaSorted = theta.argsort.reverse
    println("**** THETA ******")
    for(i <- 0 until 10) {
      val (r,f) = thetaSorted(i)
      println(data.relVocab(r) + "\t" + data.featureVocab(f) + "\t" + theta(r,f))
    }

    if(Constants.TIMING) {
      Utils.Timer.stop("printTheta")
    }
  }

  def dumpTheta(outFile:String) {
    if(Constants.TIMING) {
      Utils.Timer.start("dumpTheta")
    }
    
    val fw = new FileWriter(outFile)

    var thetaSorted = theta.argsort.reverse.filter((rf) => rf._1 != data.relVocab("NA"))
    thetaSorted = thetaSorted.sortBy((rf) => -math.abs(theta(rf._1,rf._2)))
    //for(i <- 0 until thetaSorted.length) {
    for(i <- 0 until 10000) {
      val (r,f) = thetaSorted(i)
      fw.write(data.relVocab(r) + "\t" + data.featureVocab(f) + "\t" + theta(r,f) + "\n")
    }

    fw.close()

    if(Constants.TIMING) {
      Utils.Timer.stop("dumpTheta")
    }
  }

  def updateTheta(iAll:EntityPair, iHidden:EntityPair) {
    if(Constants.TIMING) {
      Utils.Timer.start("updateTheta")
    }

    //Update le weights
    for(m <- 0 until iAll.features.length) {
      if(iAll.z(m) != iHidden.z(m)) {
	theta(iHidden.z(m),::)     :+= iHidden.features(m)
	theta(iAll.z(m),   ::)     :-= iAll.features(m)

	theta_sum(iHidden.z(m),::) :+= (nUpdates :* iHidden.features(m))
	theta_sum(iAll.z(m),   ::) :-= (nUpdates :* iAll.features(m))
      }
    }

    nUpdates += 1.0
    //Compute conditional likelihood?

    if(Constants.TIMING) {
      Utils.Timer.stop("updateTheta")
    }
 }

  /*********************************************************************
   * PHI
   *********************************************************************
   */
  val phiMid = SparseVector.zeros[Double](data.entityVocab.size + data.relVocab.size + 1)	//Add space for bias feature...
  val phiMit = SparseVector.zeros[Double](data.entityVocab.size + data.relVocab.size + 1)	//Add space for bias feature...
  //val phiMid = DenseVector.rand(data.entityVocab.size + data.relVocab.size + 1)	//Observation parameters (just 3 things for now - e1, e2, rel)

  //Innitialize bias features
  phiMid(phiMid.length-1) =   -5.0
  //phiMit(phiMid.length-1) = -100.0
  phiMit(phiMid.length-1) = -1000.0

  //phiMid(phiMid.length-1) =  -100.0
  //phiMit(phiMid.length-1) =  -200.0

  //phiMid(phiMid.length-1) =     -50.0
  //phiMit(phiMid.length-1) =  -1000.0

  def resetPhi {
    phiMid := 0
    phiMit := 0
  }

  def updatePhi(iAll:EntityPair, iHidden:EntityPair) { 
    if(Constants.TIMING) {
      Utils.Timer.start("updatePhi")
    }

    //Update le weights
    val e1id = iHidden.e1id
    val e2id = iHidden.e2id

    //TODO: have some doubts here...
    for(r <- 0 until iAll.rel.length) {
      var maxConfidence = Double.NegativeInfinity
      val scores = iAll.zScore(iAll.z :== r)
      if(scores.length > 0) {
	maxConfidence = scores.max
	//println("maxConfidence=" + maxConfidence)
      }
      var minConfidence = iAll.zScore.min
//      if(iAll.rel(r) == 0.0) {
//	println("minConfidence=" + minConfidence)
//      }

      if(r != data.relVocab("NA")) {
	if(maxConfidence > 10.0) {
          //if(iHidden.obs(r) == 0.0 && iHidden.rel(r) == 1.0) {
	  if(iHidden.obs(r) == 0.0 && iAll.rel(r) == 1.0) {
	    phiMid(iHidden.e1id)              += 1.0
	    phiMid(iHidden.e2id)              += 1.0
	    phiMid(data.entityVocab.size + r) += 1.0
	    phiMid(phiMid.length-1)           += 1.0	
          }
          if(iAll.obs(r) == 0.0 && iAll.rel(r) == 1.0) {
	    phiMid(iHidden.e1id)              -= 1.0
	    phiMid(iHidden.e2id)              -= 1.0
	    phiMid(data.entityVocab.size + r) -= 1.0
	    phiMid(phiMid.length-1)           -= 1.0	
          }
	}

	//TODO: minConfidence (aka confidence this relation wasn't extracted?)
	//Needs some more thought on how to do this...
	//Confidence that all scores are N/A...?

        //if(iHidden.obs(r) == 1.0 && iHidden.rel(r) == 0.0) {
	if(minConfidence > 10.0 && iAll.rel(r) == 0.0) {
	  if(iHidden.obs(r) == 1.0 && iAll.rel(r) == 0.0) {
	    phiMit(iHidden.e1id)              += 1.0
	    phiMit(iHidden.e2id)              += 1.0
	    phiMit(data.entityVocab.size + r) += 1.0
	    phiMit(phiMit.length-1)           += 1.0
          }
          if(iAll.obs(r) == 1.0 && iAll.rel(r) == 0.0) {
	    phiMit(iHidden.e1id)              -= 1.0
	    phiMit(iHidden.e2id)              -= 1.0
	    phiMit(data.entityVocab.size + r) -= 1.0
	    phiMit(phiMit.length-1)           -= 1.0	
          }
	}
      }
    }    

    if(Constants.TIMING) {
      Utils.Timer.stop("updatePhi")
    }
  }

  def printPhi {
    println("phiMid************************")
    println("bias\t" + phiMid(phiMid.length-1))
    //for(i <- (0 until phiMid.length).toList.sortBy((j) => -phiMid(j)).slice(0,10)) {
    for(i <- (0 until phiMid.length).toList.sortBy((j) => -math.abs(phiMid(j))).slice(0,10)) {
      if(i < data.entityVocab.size) {
	println(data.entityVocab(i) + "\t" + phiMid(i))
      } else if(i < data.entityVocab.size + data.relVocab.size) {
	println(data.relVocab(i - data.entityVocab.size) + "\t" + phiMid(i))
      }
    }
    println("phiMit************************")
    println("bias\t" + phiMit(phiMit.length-1))
    //for(i <- (0 until phiMit.length).toList.sortBy((j) => -phiMit(j)).slice(0,10)) {
    for(i <- (0 until phiMit.length).toList.sortBy((j) => -math.abs(phiMit(j))).slice(0,10)) {
      if(i < data.entityVocab.size) {
	println(data.entityVocab(i) + "\t" + phiMit(i))
      } else if(i < data.entityVocab.size + data.relVocab.size) {
	println(data.relVocab(i - data.entityVocab.size) + "\t" + phiMit(i))
      }
    }
  }

  def dumpPredictions(outFile:String) {
    val fw = new FileWriter(outFile)
    
    val mid2name = new FreebaseUtils.Mid2Name("../data/mid2name")

    for(i <- 0 until data.data.length) {
      val e12 = data.data(i)
      val relations = (0 until e12.rel.length).filter((r) => e12.rel(r) == 1.0).map((r) => data.relVocab(r)).mkString(",")
      for(j <- 0 until e12.z.length) {
	val postZ = e12.postZ(j,::)
	postZ(data.relVocab("NA")) = Double.NegativeInfinity
	val mapZnonNA = postZ.argmax
	fw.write(Array(mid2name(data.entityVocab(e12.e1id)), mid2name(data.entityVocab(e12.e2id)),
		       data.entityVocab(e12.e1id), data.entityVocab(e12.e2id), relations,
		       data.relVocab(e12.z(j)),
		       data.relVocab(mapZnonNA), postZ(mapZnonNA),
		       e12.sentences(j)).mkString("\t") + "\n")
      }
    }

    fw.close()
  }
  

  /*********************************************************************
   * Inference (Must be in implementation class)
   *********************************************************************
   */
  
  // def inferHidden(ep:EntityPair):EntityPair

  def inferAll(ep:EntityPair):EntityPair
  def inferAll(ep:EntityPair, useAverage:Boolean):EntityPair
  def train(iterations:Int)
  def train(iterations:Int, fw:FileWriter)

  var trainSimple = false

  var updatePhi   = true
  var updateTheta = true
}
