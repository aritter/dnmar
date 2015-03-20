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

import scala.util.Random

import java.io._

class MultiR(data:EntityPairData) extends Parameters(data) {
  //Randomly permute the training data
  //val training = Random.shuffle((0 until data.data.length).toList).filter((e12) => true)

  def train(nIter:Int) = {
    train(nIter, null)
  }

  def train(nIter:Int, outFile:FileWriter) = { 
    //Randomly permute the training data
    val training = Random.shuffle((0 until data.data.length).toList).filter((e12) => true)
    for(i <- 0 until nIter) {
      //println("iteration " + i)
      for(e12 <- training) {
	//Run le inference
	val iAll    = inferAll(data.data(e12))
	val iHidden = inferHidden(data.data(e12))
	data.data(e12).z = iHidden.z
	updateTheta(iAll, iHidden)
      }
    }
  }

  def inferHidden(ep:EntityPair):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHidden")
    }
    val z      = DenseVector.zeros[Int](ep.features.length)
    val zScore = DenseVector.zeros[Double](ep.features.length)
    val postZ  = DenseMatrix.zeros[Double](ep.features.length, data.nRel)

    //First pre-compute postZ
    for(i <- 0 until ep.features.length) {
      postZ(i,::) := (theta * ep.features(i)).toDense
      //Normalize (note: this isn't necessary, except for analasys purposes and generating P/R on training data...)
      val logExpSum = MathUtils.LogExpSum(postZ(i,::).toArray)
      postZ(i,::) -= logExpSum
    }
    ep.postZ = postZ

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
      Utils.Timer.stop("inferHidden")
    }
    result
  }

  def inferAll(ep:EntityPair):EntityPair = {
    inferAll(ep, false)
  }

  def inferAll(ep:EntityPair, useAverage:Boolean):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferAll")
    }
    val z      = DenseVector.zeros[Int](ep.features.length)
    val postZ  = new Array[SparseVector[Double]](ep.features.length)
    val zScore = DenseVector.zeros[Double](ep.features.length)
    val rel    = DenseVector.zeros[Double](data.nRel).t

    for(i <- 0 until ep.features.length) {
      if(useAverage) {
	postZ(i) = theta_average * ep.features(i)
      } else {
	postZ(i) = theta * ep.features(i)
      }

      //Normalize?
      //val logExpSum = MathUtils.LogExpSum(postZ(i).toArray)
      //postZ(i) -= logExpSum      

      z(i) = postZ(i).argmax
      zScore(i) = postZ(i).max

      //Set the aggregate variables
      rel(z(i)) = 1.0
    }
    if(Constants.DEBUG) {
      println("unconstrained result.z=" + z.toList.map((r) => data.relVocab(r)))
    }

    //TODO: get rid of zScore, replace with postZ...
    val result = new EntityPair(ep.e1id, ep.e2id, ep.features, rel, z, zScore)

    if(Constants.TIMING) {
      Utils.Timer.stop("inferAll")
    }
    result
  }
}
