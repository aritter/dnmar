package dnmar;

import scala.util.Random;

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

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import scala.collection.mutable.ListBuffer

import java.io._

import java.util.HashSet;

object Eval {
  //case class Prediction(val score:Double, val correct:Boolean) 
  class Prediction(val score:Double, val correct:Boolean, val rel:String, val annotated_sentence:String) {
    def this(score:Double, correct:Boolean) = this(score, correct, null, null)
  }

  var useObsPredictions = false
  var useAveragedParameters = false

  def HumanEval(param:Parameters, test:EntityPairData, annotatedFile:String, rel:Int) {
    HumanEval(param, test, annotatedFile, rel, null)
  }

  def HumanEval(param:Parameters, test:EntityPairData, annotatedFile:String) {
    HumanEval(param, test, annotatedFile, -1, null)
  }

  def HumanEval(param:Parameters, test:EntityPairData, annotatedFile:String, outFile:String) {
    HumanEval(param, test, annotatedFile, -1, outFile)
  }

  /*
  def KbpEval(param:Parameters, testDir:String, slotAssesmentFile:String, outDir:String) {
    Utils.Timer.start("KbpEval")

    var nidOffset = 0

    val candidates = new HashSet[String]

    val queryIds = new HashSet[String]

    /*
    for(line <- scala.io.Source.fromInputStream(new FileInputStream(slotAssesmentFile))(io.Codec("iso-8859-1")).getLines()) {    
      val fields = line.trim.split("\t")
      //queryIds.add(fields(1))
      //candidates.add(fields(1) + ":" + fields(8))
    }
    */

    val responseFile = outDir + "/responseFile"
    val prOutFile = new FileWriter(outDir + "/prOut")

    var predictions = List[Tuple6[String,String,String,String,String,Double]]()

    //var t = 1.0
    //while(t >= 0.0) {

    val fw = new FileWriter(responseFile)
    for(file <- new java.io.File(testDir).listFiles.sortBy({x => x.getName})) {
    //for(file <- new java.io.File(testDir).listFiles) {
      val qid   = file.getName
      var nid:String   = null
      var nstr:String  = null
      var etyp:String  = null
      var vtyp:String  = null
      var valu:String  = null
      var docid:String = null
      var feats:String = null

      println(qid)

      for(line <- scala.io.Source.fromFile(file).getLines()) {
	if(line.startsWith("NIL")) {
	  nid = line.trim
	  nidOffset = 0
	  
	  if(feats != null) {
	    queryIds.add(qid)
	    candidates.add(qid + ":" + valu)

	    //val features = SparseVector.zeros[Double](param.data.featureVocab.size + 1)
	    val features = SparseVector.zeros[Double](param.nFeat + 1)
	    //param.data.featureVocab.lock
	    feats.split("\t").map(x => param.data.featureVocab(x)).filter(x => x >= 0).foreach(
	      {x =>
		features(x) = 1.0
	     })

	    var postZ:DenseVector[Double] = null
	    if(useAveragedParameters) {
	      postZ = (param.theta_average * features).toDense
	    } else {
	      postZ = (param.theta * features).toDense
	    }
	    val logExpSum = MathUtils.LogExpSum(postZ.toArray)
	    postZ -= logExpSum
	    val pNA = postZ(param.data.relVocab("NA"))
	    postZ(param.data.relVocab("NA")) = Double.NegativeInfinity

	    var predicted = postZ.argmax
	    //println(feats.split("\t").mkString(","))
	    //println(feats.split("\t").length)
	    //println(postZ)
	    //println(features.sum)
	    //println(exp(postZ(predicted)))
	    //fw.write(qid + "\t" + param.data.relVocab(predicted) + "\t" + "DNMAR" + "\t" + docid + "\t" + valu + "\n")
	    predictions ::= (qid, param.data.relVocab(predicted), "DNMAR", docid, valu, postZ(predicted))

	    feats = null
	  }
	} else if(nidOffset == 1) {
	  nstr = line.trim
	} else if(nidOffset == 2) {
	  etyp = line.trim
	} else if(nidOffset == 3) {
	  vtyp = line.trim
	} else if(nidOffset == 4) {
	  valu = line.trim
	} else if(nidOffset == 8) {
	  docid = line.trim
	} else if(nidOffset > 10 && line.startsWith("arg")) {
	  feats = line.trim
	}
	nidOffset += 1
      }
    }

    var t = 1.0
    //while(t >= 0.0) {
    //for(nPredictions <- 1000 to 1) {
    var nPredictions = 1000
    while(nPredictions > 0) {
      println("nPredictions =" + nPredictions)

      val rfName = responseFile + "." + nPredictions
      val fw = new FileWriter(rfName)
      
      val sorted = predictions.sortBy({x => -x._6})
      //for(i <- 0 until (t * sorted.length.toDouble).toInt) {
      for(i <- 0 until nPredictions) {
	fw.write(sorted(i)._1 + "\t" + sorted(i)._2 + "\t" + sorted(i)._3 + "\t" + sorted(i)._4 + "\t" + sorted(i)._5 + "\n")
      }

      fw.close

      val os = new PrintStream("test_file")

      //NOTE: uses anydoc scoring -> as best I can tell this is what mihai's code does too (not mentioned in properties file - see defaults in "Constants.java").
      val pair = edu.stanford.nlp.kbp.slotfilling.SFScore.score(os, rfName, slotAssesmentFile, true, candidates, queryIds)
      
      //println(pair.first + "\t" + pair.second)
      prOutFile.write(pair.first + "\t" + pair.second + "\t" + nPredictions + "\n")

      // these increments are identical to those in MultiR.generatePRCurve() 
      /*
      if(t > 1.0) t -= 1.0;
      else if(t > 0.99)   t  -= 0.0001;
      else if(t > 0.95)   t  -= 0.001;
      else t -= 0.01;
      */
      if(nPredictions < 100) nPredictions -= 1
      else nPredictions -= 10
    }
    prOutFile.close

    Utils.Timer.stop("KbpEval")
  }
  */

  def HumanEval(param:Parameters, test:EntityPairData, annotatedFile:String, rel:Int, outFile:String) {
    if(Constants.TIMING) {
      Utils.Timer.start("HumanEval")
    }

    /*
     ************************************************************************************************************************
     * NOTE: it appears MultiR uses the aggregate-level scores to generate P/R curves rather than the per-sentence scores
     * Hypothesis: this could lead to higher recal....
     *
     * Comment From MultiR (FullInference.java):
       It's important to ignore the _NO_RELATION_ type here, soneed to start at 1!
       final value is avg of maxes
     ************************************************************************************************************************
     */

    /*
     * Read in the features and labels
     */
    val features            = new ListBuffer[SparseVectorCol[Double]]
    val aggregateScores     = new ListBuffer[Double]
    val labels              = new ListBuffer[Int]
    val tf                  = new ListBuffer[Boolean]
    val sentences           = new ListBuffer[String]
    val sentences_annotated = new ListBuffer[String]
    for(line <- scala.io.Source.fromFile(annotatedFile).getLines()) {
      //var Array(e1id_str, e2id_str, i_dont_know_what_this_is, relation_str, is_mention_str, sentence) = line.trim.split("\t")
      var Array(e1id_str, e2id_str, i_dont_know_what_this_is, relation_str, is_mention_str, sentence_annotated, e1str, e2str, sentence) = line.trim.split("\t")
      if(sentence(0) == '"') {
	sentence = sentence.substring(1,sentence.length-1)	//strip quotes
      }

      //if(is_mention_str != "n" && (rel == -1 || test.relVocab(relation_str) == rel)) {
      //if(rel == -1 || test.relVocab(relation_str) == rel && (relation_str != "/location/administrative_division/country") && (is_mention_str != "")) {
      //if(rel == -1 || test.relVocab(relation_str) == rel && (relation_str != "/location/administrative_division/country")) {
      if(relation_str != "/location/administrative_division/country") {
	test.entityVocab.lock      
	val e1id = test.entityVocab(e1id_str)
	val e2id = test.entityVocab(e2id_str)

	/*****************************************************************************************************************
	 * OK, we can't assume a relation isn't true if just because the sentence is labeled with a different relation
	 *****************************************************************************************************************
	 */

	//Treat errors as "NA"? (doesn't penalize recall for missing them, but any predictions will hurt precision...)
//	if(is_mention_str != "y" && is_mention_str != "indirect") {
//	  relation_str = "NA"
//	}

	val ep    = test.data.filter((ep) => ep.e1id == e1id && ep.e2id == e2id)(0)
	//TODO: no need to find the actual test sentence if we'er using the aggregate scores?
	//OK, now let's find the sentence in the test data, so we can get it's features
	val index = ep.sentences.indexOf(sentence)
	if(index >= 0) {
	  features            += ep.features(index)
	  labels              += test.relVocab(relation_str)
	  sentences           += sentence
	  tf                  += is_mention_str == "y" || is_mention_str == "indirect"
	  sentences_annotated += sentence_annotated
	} else {
	  if(Constants.DEBUG) {
	    println("Threw out an annotated example...")
	    println(e1id_str + "\t" + e1id)
	    println(e2id_str + "\t" + e2id)
	    println(sentence)
	    println(ep.sentences.toList)
	    println(ep.sentences.indexOf(sentence))
	  }
	}
      }
    }

    var sortedPredictions = List[Prediction]()

    var maxRecall = 0.0
    for(i <- 0 until features.length) {
      var postZ:DenseVector[Double] = null
      if(useAveragedParameters) {
	postZ = (param.theta_average * features(i)).toDense
      } else {
	postZ = (param.theta * features(i)).toDense
      }
      val logExpSum = MathUtils.LogExpSum(postZ.toArray)
      postZ -= logExpSum
      val pNA = postZ(test.relVocab("NA"))
      //postZ(test.relVocab("NA")) = Double.NegativeInfinity

      //println(param.data.relVocab(predicted) + "\t" + sentences(i))

      if(tf(i) && labels(i) != test.relVocab("NA") && (rel == -1 || labels(i) == rel)) {
	maxRecall += 1.0
      }
      
      //if(predicted != test.relVocab("NA")) {
      //NOTE: not just making max prediction (as done in original MultiR paper...)
      var predicted = postZ.argmax
      for(predicted <- 0 until test.nRel) {
	if((rel == -1 || predicted == rel) && predicted != test.relVocab("NA")) {
	  if(predicted == labels(i)) {
	    if(tf(i)) {
	      //True Positive
	      sortedPredictions ::= new Prediction(postZ(predicted), true, test.relVocab(predicted), sentences_annotated(i))
	    } else {
	      //False Positive
	      sortedPredictions ::= new Prediction(postZ(predicted), false, test.relVocab(predicted), sentences_annotated(i))
	    }
	  }
	}
      }
    }

    if(maxRecall > 0) {
      if(outFile != null) {
	println("dumping to " + outFile)
	DumpPR(sortedPredictions, maxRecall, outFile)
      }
      PrintPR(sortedPredictions, maxRecall)
    }

    if(Constants.TIMING) {
      Utils.Timer.stop("HumanEval")
    }
  }

/*
  def HumanEvalMiml(miml:JointBayesRelationExtractor, test:EntityPairData, annotatedFile:String, outFile:String) {
    HumanEvalMiml(miml, test, annotatedFile, -1, outFile)
  }

  def HumanEvalMiml(miml:JointBayesRelationExtractor, test:EntityPairData, annotatedFile:String, rel:Int, outFile:String) {
    if(Constants.TIMING) {
      Utils.Timer.start("HumanEvalMiml")
    }

    /*
     * Read in the features and labels
     */
    val features            = new ListBuffer[SparseVectorCol[Double]]
    val aggregateScores     = new ListBuffer[Double]
    val labels              = new ListBuffer[Int]
    val tf                  = new ListBuffer[Boolean]
    val sentences           = new ListBuffer[String]
    val sentences_annotated = new ListBuffer[String]
    for(line <- scala.io.Source.fromFile(annotatedFile).getLines()) {
      var Array(e1id_str, e2id_str, i_dont_know_what_this_is, relation_str, is_mention_str, sentence_annotated, e1str, e2str, sentence) = line.trim.split("\t")
      if(sentence(0) == '"') {
	sentence = sentence.substring(1,sentence.length-1)	//strip quotes
      }

      if(relation_str != "/location/administrative_division/country") {
	test.entityVocab.lock      
	val e1id = test.entityVocab(e1id_str)
	val e2id = test.entityVocab(e2id_str)

	/*****************************************************************************************************************
	 * OK, we can't assume a relation isn't true if just because the sentence is labeled with a different relation
	 *****************************************************************************************************************
	 */

	val ep    = test.data.filter((ep) => ep.e1id == e1id && ep.e2id == e2id)(0)
	//TODO: no need to find the actual test sentence if we'er using the aggregate scores?
	//OK, now let's find the sentence in the test data, so we can get it's features
	val index = ep.sentences.indexOf(sentence)
	if(index >= 0) {
	  features            += ep.features(index)
	  labels              += test.relVocab(relation_str)
	  sentences           += sentence
	  tf                  += is_mention_str == "y" || is_mention_str == "indirect"
	  sentences_annotated += sentence_annotated
	} else {
	  if(Constants.DEBUG) {
	    println("Threw out an annotated example...")
	    println(e1id_str + "\t" + e1id)
	    println(e2id_str + "\t" + e2id)
	    println(sentence)
	    println(ep.sentences.toList)
	    println(ep.sentences.indexOf(sentence))
	  }
	}
      }
    }

    var sortedPredictions = List[Prediction]()

    var maxRecall = 0.0
    for(i <- 0 until features.length) {
      //println(param.data.relVocab(predicted) + "\t" + sentences(i))
      val stringFeatures = Utils.bin2int(features(i).toArray).map((f) => test.featureVocab(f))
      //println(stringFeatures)
      val predictions = JointBayesRelationExtractor.sortPredictions(miml.classifyLocally(asList(stringFeatures)))

      if(tf(i) && labels(i) != test.relVocab("NA") && (rel == -1 || labels(i) == rel)) {
	maxRecall += 1.0
      }
      
      //if(predicted != test.relVocab("NA")) {
      //NOTE: not just making max prediction (as done in original MultiR paper...)
      for(j <- 0 until predictions.length) {
	val predicted = test.relVocab(predictions.get(j).first)
	val confidence = predictions.get(j).second

	//println(test.relVocab(predicted) + "\t" + confidence + "\t" + test.relVocab(labels(i)))

	if((rel == -1 || predicted == rel) && predicted != test.relVocab("NA")) {
	  if(predicted == labels(i)) {
	    if(tf(i)) {
	      //True Positive
	      sortedPredictions ::= new Prediction(confidence, true, test.relVocab(predicted), sentences_annotated(i))
	    } else {
	      //False Positive
	      sortedPredictions ::= new Prediction(confidence, false, test.relVocab(predicted), sentences_annotated(i))
	    }
	  }
	}
      }
    }

    if(maxRecall > 0) {
      if(outFile != null) {
	println("dumping to " + outFile)
	DumpPR(sortedPredictions, maxRecall, outFile)
      }
      PrintPR(sortedPredictions, maxRecall)
    }

    if(Constants.TIMING) {
      Utils.Timer.stop("HumanEvalMiml")
    }
  }
 */

  def HumanEvalNer(param:Parameters, inDir:String, outFile:String) {
    HumanEvalNer(param, inDir, -1, outFile)
  }

  def HumanEvalNer(param:Parameters, inDir:String, rel:Int, outFile:String) {
    val fGold     = new BufferedReader(new InputStreamReader(new FileInputStream(inDir + "/gold")))
    val fFeatures = new BufferedReader(new InputStreamReader(new FileInputStream(inDir + "/test.features")))
    var lineNum = 0

    var sortedPredictions = List[Prediction]()
    var maxRecall = 0.0

    for(line <- scala.io.Source.fromFile(inDir + "/entities").getLines()) {
      val entity      = param.data.entityVocab(line.trim)
      val gold        = param.data.relVocab(fGold.readLine.trim)
      val featuresStr = fFeatures.readLine.trim

      /*
      val relations = DenseVector.zeros[Double](param.data.relVocab.size)
      rels.foreach( 
	{r => 
	  relations(r) = 1.0
       })
       */

      val features = SparseVector.zeros[Double](param.data.featureVocab.size + 1)
      features(param.data.featureVocab.size) = 1.0		//Bias feature
      featuresStr.split(" ").map(x => param.data.featureVocab(x)).filter(x => x >= 0).foreach(
	{x =>
	  features(x) = 1.0
       })

      var postZ:DenseVector[Double] = null
      if(useAveragedParameters) {
	postZ = (param.theta_average * features).toDense
      } else {
	postZ = (param.theta * features).toDense
      }
      val logExpSum = MathUtils.LogExpSum(postZ.toArray)
      postZ -= logExpSum
      val pNA = postZ(param.data.relVocab("NA"))
      postZ(param.data.relVocab("NA")) = Double.NegativeInfinity

      if(gold != param.data.relVocab("NA") && (rel == -1 || gold == rel)) {
	maxRecall += 1.0
      }

      var predicted = postZ.argmax
      for(predicted <- 0 until param.data.nRel) {
	if((rel == -1 || predicted == rel) && predicted != param.data.relVocab("NA")) {
	  if(predicted == gold) {
	      //True Positive
	      sortedPredictions ::= new Prediction(postZ(predicted), true, param.data.relVocab(predicted), featuresStr)
	  } else {
	      //False Positive
	    sortedPredictions ::= new Prediction(postZ(predicted), false, param.data.relVocab(predicted), featuresStr)
	  }
	}
      }
      lineNum += 1
    }
    
    if(maxRecall > 0) {
      if(outFile != null) {
	println("dumping to " + outFile)
	DumpPR(sortedPredictions, maxRecall, outFile)
      }
      PrintPR(sortedPredictions, maxRecall)
    }
  }

  def AggregateEval(param:Parameters, test:EntityPairData) {
    AggregateEval(param, test, -1, null)
  }

  def AggregateEval(param:Parameters, test:EntityPairData, rel:Int) {
    AggregateEval(param, test, rel, null)
  }

  def AggregateEval(param:Parameters, test:EntityPairData, outFile:String) {
    AggregateEval(param, test, -1, outFile)
  }

  def AggregateEval(param:Parameters, test:EntityPairData, rel:Int, outFile:String) {
    if(Constants.TIMING) {
      Utils.Timer.start("AggregateEval")
    }

    var totalRelations = 0.0	//For computing fn

    var sortedPredictions = List[Prediction]()
    for(ep <- Random.shuffle(test.data.toList)) { 
      val predicted = param.inferAll(ep, useAveragedParameters)

//      if(Constants.DEBUG) {
//	println("predicted:\t" + Utils.bin2int(predicted.rel.toArray).map((r) => test.relVocab(r)))
//	println("observed:\t"  + Utils.bin2int(ep.rel.toArray).map((r) => test.relVocab(r)))
//      }
      for(r <- 0 until test.nRel) {
	if(test.relVocab(r) != "NA" && (rel == -1 || r == rel)) {
	  if(ep.rel(r) == 1.0 && (rel == -1 || rel == r)) {
	    totalRelations += 1.0
	  }

	  val prediction = if(useObsPredictions) { 
	    predicted.obs(r)
	  } else {
	    predicted.rel(r)
	  }

	  if(ep.rel(r) == 1.0 && prediction == 1.0) { 
	    sortedPredictions ::= new Prediction(predicted.zScore(predicted.z :== r).max, true)
	    //sortedPredictions ::= Prediction(predicted.zScore(predicted.z :== r).max, true)
	  }
	  else if(ep.rel(r) == 0.0 && prediction == 1.0) {
	    sortedPredictions ::= new Prediction(predicted.zScore(predicted.z :== r).max, false)
	    //sortedPredictions ::= Prediction(predicted.zScore(predicted.z :== r).max, false)
	  }
	}
      }
    }
    
    if(outFile != null) {
      DumpPR(sortedPredictions, totalRelations, outFile)
    }
    
    PrintPR(sortedPredictions, totalRelations)

    if(Constants.TIMING) {
      Utils.Timer.stop("AggregateEval")
    }
  }

  def DumpPR(sortedPredictions:List[Prediction], maxResults:Double, outFile:String) {
    var tp, fp, fn = 0.0

    val fw = new FileWriter(outFile)

    for(prediction <- sortedPredictions.sortBy(-_.score)) {
      if(prediction.correct) {
	tp += 1.0
      } else {
	fp += 1.0
      }

      fn = maxResults - tp
      val p = tp / (tp + fp)
      val r = tp / (tp + fn)
      //val f = 2 * p * r / (p + r)

      fw.write(p + "\t" + r + "\t" + prediction.rel + "\t" + prediction.correct + "\t" + prediction.annotated_sentence + "\t" + prediction.score + "\n")
    }

    fw.close()
  }    

  def PrintPR(sortedPredictions:List[Prediction], maxResults:Double) {
    var tp, fp, fn = 0.0

    var maxF,  maxFp, maxFr = 0.0
    var maxP,  maxPr, maxPf = 0.0
    var maxRp, maxR,  maxRf = 0.0
    for(prediction <- sortedPredictions.sortBy(-_.score)) {
      if(prediction.correct) {
	tp += 1.0
      } else {
	fp += 1.0
      }

      fn = maxResults - tp
      val p = tp / (tp + fp)
      val r = tp / (tp + fn)
      val f = 2 * p * r / (p + r)

      if(f > maxF) {
	maxF  = f
	maxFp = p
	maxFr = r
      }

      if(r > 0.05 && p > maxP) {
	maxP  = p
	maxPr = r
	maxPf = f
      }

      if(r > maxR && p > 0.5) {
	maxR  = r
	maxRp = p
	maxRf = f
      }
    }
    println("N:" + sortedPredictions.length)
    println("P:" + maxFp + "\tR:" + maxFr + "\tF:" + maxF)
    println("P:" + maxP  + "\tR:" + maxPr + "\tF:" + maxPf)
    println("P:" + maxRp + "\tR:" + maxR  + "\tF:" + maxRf)
  }
}
