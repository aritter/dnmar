package dnmar;

import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStream

import cc.factorie.protobuf.DocumentProtos.Relation
import cc.factorie.protobuf.DocumentProtos.Relation.RelationMentionRef

import sys.process._

import org.clapper.argot._

import java.io._

object Constants {
  var DEBUG = false
  var TIMING = true
}

object Main {
  import ArgotConverters._
  
  val parser = new ArgotParser(
    "DNMAR",
    preUsage=Some("DNMAR: Version 0.1. Copyright (c) " +
                  "2012, Alan Ritter.")
  )

  val train = parser.option[ProtobufData](List("trainProto"), "n", "Training data (in google protobuf format).") { 
    println("Loading train")
    (sValue, opt) => new ProtobufData(sValue,null,null,null,true)
  }

  val nerTrain = parser.option[NerData](List("trainNER"), "n", "Training data (in NER file format).") { 
    println("Loading train")
    (sValue, opt) => new NerData(sValue,null,null,null)
  }

  val kbpTrain = parser.option[KbpData](List("trainKBP"), "n", "Training data (in KBP file format).") { 
    println("Loading train")
    (sValue, opt) => new KbpData(sValue,null,null,null)
  }

  val test  = parser.option[ProtobufData](List("testProto"), "n", "Test data (in google protobuf format).") {
    println("Loading test")
    (sValue, opt) => new ProtobufData(sValue, 
				      train.value.getOrElse(null).entityVocab, 
				      train.value.getOrElse(null).relVocab.lock, 
				      train.value.getOrElse(null).featureVocab.lock,
				      true)
  }

  val nerTest  = parser.option[NerData](List("testNER"), "n", "Test data (in NER format).") {
    println("Loading test")
    (sValue, opt) => new NerData(sValue, 
				 nerTrain.value.getOrElse(null).entityVocab, 
				 nerTrain.value.getOrElse(null).relVocab.lock, 
				 nerTrain.value.getOrElse(null).featureVocab.lock)
  }

  val kbpTest  = parser.option[KbpData](List("testKBP"), "n", "Test data (in KBP format).") {
    println("Loading test")
    (sValue, opt) => new KbpData(sValue, 
				 kbpTrain.value.getOrElse(null).entityVocab, 
				 kbpTrain.value.getOrElse(null).relVocab.lock, 
				 kbpTrain.value.getOrElse(null).featureVocab.lock)
  }

  val outDir = parser.option[String](List("outDir"), "n", "output directory") {
    (sValue, opt) => sValue
  }

  val filterFB = parser.option[String](List("filterFB"), "n", "Filter Freebase") {
    (sValue, opt) => sValue
  }

  val algorithm = parser.option[String](List("algorithm"), "n", "algorithm") {
    (sValue, opt) => sValue
  }

  val nIter = parser.option[String](List("nIter"), "n", "iterations") {
    (sValue, opt) => sValue
  }

  val outCompareInfer = parser.option[String](List("outCompareInfer"), "n", "output file for comparing inference methods") {
    (sValue, opt) => sValue
  }

  def main(args: Array[String]) {
    try { 
      parser.parse(args)
    }
    catch { 
      case e: ArgotUsageException => println(e.message)
    }

    var multir:MultiR = null
    var dnmar:DNMAR   = null

    if(train.value.getOrElse(null) != null) {
      multir = new MultiR(train.value.getOrElse(null))
      dnmar  = new DNMAR(train.value.getOrElse(null))
    } else if(nerTrain.value.getOrElse(null) != null) {
      multir = new MultiR(nerTrain.value.getOrElse(null))
      dnmar  = new DNMAR(nerTrain.value.getOrElse(null))
    } else if(kbpTrain.value.getOrElse(null) != null) {
      multir = new MultiR(kbpTrain.value.getOrElse(null))
      dnmar  = new DNMAR(kbpTrain.value.getOrElse(null))      
    }

    if(filterFB.value.getOrElse(null) != null) {
      FreebaseUtils.filterFreebase(filterFB.value.getOrElse(null), filterFB.value.getOrElse(null) + ".filtered", train.value.getOrElse(null).entityVocab, train.value.getOrElse(null).relVocab)
    }

    if(outDir.value.getOrElse(null) != null) {
      println(outDir.value.getOrElse(null))
      ("mkdir -p " + outDir.value.getOrElse(null)).!
      //("mkdir -p " + outDir.value.getOrElse(null) + "/parameters").!
      ("mkdir -p " + outDir.value.getOrElse(null) + "/predictions").!      
    }

    if(algorithm.value.getOrElse(null) == "MultiR") {
      println("evaluating MultiR")
      if(nIter.value.getOrElse(null) != null) {
	EvalIterations(multir, nIter.value.getOrElse(null).toInt)
      } else {
	EvalIterations(multir, 50)
      }
      //println("DUMPING THETA")
      //multir.dumpTheta(outDir.value.getOrElse(null) + "/parameters/theta")
      if(train.value.getOrElse(null) != null) {
	//Only for binary relation data (for now)
	multir.dumpPredictions(outDir.value.getOrElse(null) + "/predictions/preds")
      }
    } else if(algorithm.value.getOrElse(null) == "DNMAR") {
      println("evaluating DNMAR")
      if(nIter.value.getOrElse(null) != null) {
	EvalIterations(dnmar, nIter.value.getOrElse(null).toInt)
      } else {
	EvalIterations(dnmar, 1)
      }
      //println("DUMPING THETA")
      //dnmar.dumpTheta(outDir.value.getOrElse(null) + "/parameters/theta")
      if(train.value.getOrElse(null) != null) {
	//Only for binary relation data (for now)
	dnmar.dumpPredictions(outDir.value.getOrElse(null) + "/predictions/preds")
      }
    } else if(algorithm.value.getOrElse(null) == "MIML") {
      println("(not) loading MIML model")
      //val props = StanfordStringUtils.argsToProperties(List("-props", "/home/aritter/dlvm/data/mimlre-2012-11-27/config/multir/multir_mimlre.properties").toArray)
      //val miml = JointBayesRelationExtractor.load("/home/aritter/dlvm/data/mimlre-2012-11-27/corpora/multir/multir_JOINT_BAYES_T5_E15_NF5_Fall_M0_Istable_Ytrue_EPOCH14.ser", props)
      //val outFile = outDir.value.getOrElse(null) + "/out"
      //Eval.HumanEvalMiml(miml, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt", outFile)
    }
    
  }

  def EvalIterations(dnmar:Parameters, nIter:Int) {
    var nrel:Int       = -1
    var relVocab:Vocab = null
    if(train.value.getOrElse(null) != null) {
      nrel     = train.value.getOrElse(null).relVocab.size
      relVocab = train.value.getOrElse(null).relVocab
    } else if(nerTrain.value.getOrElse(null) != null) {
      nrel     = nerTrain.value.getOrElse(null).relVocab.size
      relVocab = nerTrain.value.getOrElse(null).relVocab
    } else if(kbpTrain.value.getOrElse(null) != null) {
      nrel     = kbpTrain.value.getOrElse(null).relVocab.size
      relVocab = kbpTrain.value.getOrElse(null).relVocab      
    }

    var fw:FileWriter = null
    if(outCompareInfer.value.getOrElse(null) != null) {
      println(outCompareInfer.value.getOrElse(null))
      fw = new FileWriter(outCompareInfer.value.getOrElse(null))
      fw.write(List("score1rs", "time1rs", "score10rs", "time10rs", "score20rs", "time20rs", "score1kBeam", "time1kBeam", "scoreBNB", "timeBNB", "scoreExact", "timeExact", "nVars").reduceLeft(_ + "\t" + _) + "\n")
    }

    //for(i <- 0 to nIter+1) {
    for(i <- 0 to nIter) {
      var outFile:String = null

      println("*********************************************")
      println("iteration " + i)
      println("*********************************************")

      dnmar.train(1, fw)

      /*
      println("rel predictions:")
      Eval.useObsPredictions = false
      Eval.AggregateEval(dnmar, test.value.getOrElse(null))

      println("*********************************************")
      println("* rel predictions (training):")
      println("*********************************************")
      Eval.useObsPredictions = false
      Eval.AggregateEval(dnmar, train.value.getOrElse(null))

      println("*********************************************")
      println("* Human annotated evaluation")
      println("*********************************************")
      Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt")
      */

      if(i == nIter) {
	println("*********************************************")
	println("* averaged parameters")
	println("*********************************************")
	dnmar.computeThetaAverage
	Eval.useAveragedParameters = true

	Eval.useObsPredictions = false

	if(i == nIter) {
	  outFile = outDir.value.getOrElse(null)
	  if(outFile != null) {
	    outFile += "/aggregate"
	  }
	}
	if(test.value.getOrElse(null) != null) {
	  Eval.AggregateEval(dnmar, test.value.getOrElse(null), outFile)
	} else if(nerTest.value.getOrElse(null) != null) {
	  Eval.AggregateEval(dnmar, nerTest.value.getOrElse(null), outFile)
	} else if(kbpTest.value.getOrElse(null) != null) {
	  Eval.AggregateEval(dnmar, kbpTest.value.getOrElse(null), outDir.value.getOrElse(null))
	}

	println("Human annotated evaluation (averaged)")
	if(i == nIter) {
	  outFile = outDir.value.getOrElse(null)
	  if(outFile != null) {
	    outFile += "/sentential"
	  }
	}
	if(nerTrain.value.getOrElse(null) != null) {
	  Eval.HumanEvalNer(dnmar, "/home/aritter/dlvm/data/ner/annotated", outFile)
	} else if(train.value.getOrElse(null) != null) {
	  Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt", outFile)
	} else if(kbpTrain.value.getOrElse(null) != null) {
	  //Eval.KbpEval(dnmar, "/home/aritter/dlvm/data/mimlre-2012-11-27/corpora/kbp/test", "/home/aritter/dlvm/data/mimlre-2012-11-27/resources/kbp/test_combined/TAC_KBP_Regular-Slot_Assessments", outDir.value.getOrElse(null))
	}
	for(r <- 0 until nrel) {
	  if(i == nIter) {
	    outFile = outDir.value.getOrElse(null)
	    if(outFile != null) {
	      outFile += ("/sentential_" + relVocab(r).replace("/", "_"))
	    }
	  }
	  println(relVocab(r))
	  if(nerTrain.value.getOrElse(null) != null) {
	    Eval.HumanEvalNer(dnmar, "/home/aritter/dlvm/data/ner/annotated", r, outFile)
	  } else if(train.value.getOrElse(null) != null) {
	    Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential-byrelation.txt", r, outFile)
	  }
	}
	Eval.useAveragedParameters = false
      }

      if(Constants.TIMING) {
	Utils.Timer.print
	Utils.Timer.reset
      }
    }
  }
  
  if(Constants.TIMING) {
    Utils.Timer.print
  }
}
