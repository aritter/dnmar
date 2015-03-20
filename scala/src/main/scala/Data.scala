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
import scala.io._
import java.io._


import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

import cc.factorie.protobuf.DocumentProtos.Relation
import cc.factorie.protobuf.DocumentProtos.Relation.RelationMentionRef

/**************************************************************************
 * EntityPair
 * Stores all observed and hidden variables associated with a pair
 * of entities (e1id,e2id)
 **************************************************************************
 */
class EntityPair(val e1id:Int, val e2id:Int, val features:Array[SparseVectorCol[Double]], val rel:DenseVectorRow[Double], var z:DenseVector[Int], val zScore:DenseVector[Double], val obs:DenseVector[Double], val sentences:Array[String]) {
  def this(e1id:Int, e2id:Int, features:Array[SparseVectorCol[Double]], rel:DenseVectorRow[Double], z:DenseVector[Int], zScore:DenseVector[Double], obs:DenseVector[Double]) = this(e1id, e2id, features, rel, z, zScore, obs, null)
  def this(e1id:Int, e2id:Int, features:Array[SparseVectorCol[Double]], rel:DenseVectorRow[Double]) = this(e1id, e2id, features, rel, null, null, rel.toDense, null)
  def this(e1id:Int, e2id:Int, features:Array[SparseVectorCol[Double]], rel:DenseVectorRow[Double], sentences:Array[String]) = this(e1id, e2id, features, rel, null, null, rel.toDense, sentences)
  def this(e1id:Int, e2id:Int, features:Array[SparseVectorCol[Double]], rel:DenseVectorRow[Double], z:DenseVector[Int], zScore:DenseVector[Double]) = this(e1id, e2id, features, rel, z, zScore, rel.toDense, null)
  //val obs = rel.toDense							
  var postObs:DenseVector[Double] = null
  var postZ:DenseMatrix[Double] = null
}

abstract class EntityPairData {
  val data:Array[EntityPair]
  val nRel:Int
  val nFeat:Int

  val entityVocab:Vocab
  val relVocab:Vocab
  var featureVocab:Vocab

  val fbData:FreebaseUtils.FreebaseData
}

class KbpData(inDir:String, evoc:Vocab, rvoc:Vocab, fvoc:Vocab) extends EntityPairData {
  def this(inDir:String) = this(inDir, null, null, null)

  var newVocab = true
  val entityVocab  = if(evoc != null) { evoc } else { new Vocab }
  val relVocab     = if(rvoc != null) { rvoc } else { new Vocab }
  var featureVocab = if(fvoc != null) { newVocab=false; fvoc } else { new Vocab }

  val fbData = new FreebaseUtils.FreebaseData("../data/freebase-datadump-quadruples.tsv.bz2.filtered")

  val entityPair2features = new HashMap[Tuple2[Int,Int],List[Array[Int]]]
  val entityPair2labels   = new HashMap[Tuple2[Int,Int],DenseVectorRow[Double]]

  //Limit on the number of lines for debugging and to reduce memory usage
  //val MAXLINES = 100
  //val MAXLINES = 3000000
  val MAXLINES = 5000000
  //val MAXLINES = 20000000

  //First pass: figure out vocabuarly sizes
  var nLines = 0
  for(file <- new java.io.File(inDir).listFiles) {
    //if((file.getName contains "kb_part") && nLines < 5000000) {
    //if((file.getName contains "kb_part") && nLines < 2000000) {
    //if((file.getName contains "kb_part") && nLines < 100000) {
    if((file.getName contains "kb_part") && nLines < MAXLINES) {
      for(line <- scala.io.Source.fromFile(file).getLines()) {
	nLines += 1
	if(nLines % 10000 == 0) {
	  println("pass 1 nLines =" + nLines)
	}
	val fields = line.trim.split("""\s+""")
	if(fields.length > 5) {
	  val Array(junk_eid, entType, neType, slotValue, concatenatedLabel) = fields.slice(0,5)
	  //println(concatenatedLabel)
	  val rels = concatenatedLabel.split("""\|""").filter(_ != "_NR").map(x => relVocab(x)) ++ Array(relVocab("NA"))
	  val features = fields.slice(5,fields.length).map(x => featureVocab(x))
	  val e1 = entityVocab(junk_eid)
	  val e2 = entityVocab(slotValue)
	}
      }
    }
  }

  //Only ues features with minimum count
  featureVocab = featureVocab.getMinCountVocab

  nLines = 0
  for(file <- new java.io.File(inDir).listFiles) {
    //if((file.getName contains "kb_part") && nLines < 5000000) {
    //if((file.getName contains "kb_part") && nLines < 2000000) {
    //if((file.getName contains "kb_part") && nLines < 100000) {
    if((file.getName contains "kb_part") && nLines < MAXLINES) {
      for(line <- scala.io.Source.fromFile(file).getLines()) {
	nLines += 1
	if(nLines % 10000 == 0) {
	  println("pass 2 nLines =" + nLines)
	}
	val fields = line.trim.split("""\s+""")
	if(fields.length > 5) {
	  val Array(junk_eid, entType, neType, slotValue, concatenatedLabel) = fields.slice(0,5)
	  //println(concatenatedLabel)
	  val rels = concatenatedLabel.split("""\|""").filter(_ != "_NR").map(x => relVocab(x)) ++ Array(relVocab("NA"))
	  val features = fields.slice(5,fields.length).map(x => featureVocab(x))
	  val e1 = entityVocab(junk_eid)
	  val e2 = entityVocab(slotValue)

	  if(!entityPair2features.contains((e1,e2))) {
	    entityPair2features += (e1,e2) -> List()
	  }
	  entityPair2features((e1,e2)) ::= features
	  val relations = DenseVector.zeros[Double](relVocab.size)
	  rels.foreach(
	    {r =>
	      relations(r) = 1.0
	   })
	  //TODO: need to debug? make sure relations are the same for each pair ...?
	  entityPair2labels((e1,e2)) = relations.t
	}
      }
    }
  }

  val data = new Array[EntityPair](entityPair2features.size)
  //Second pass, create data structures
  
  var nEntityPairs = 0
  for(((e1, e2), features) <- entityPair2features) {
    val featureVectors = new Array[SparseVectorCol[Double]](features.length)
    for(i <- 0 until features.length) {
      featureVectors(i) = SparseVector.zeros[Double](featureVocab.size + 1)
      featureVectors(i)(featureVocab.size) = 1.0	//Bias feature
      features(i).filter(x => x >= 0).foreach(
	{x =>
	  featureVectors(i)(x) = 1.0
       })
    }
    val relations = entityPair2labels((e1,e2))
    data(nEntityPairs) = new EntityPair(e1, e2, featureVectors, relations)
    nEntityPairs += 1
  }

  val nEntities = entityVocab.size
  val nFeat        = featureVocab.size
  val nRel         = relVocab.size

  println("nRel:" + nRel)
  println("feature vocab size: " + featureVocab.size)
  println("# entity pairs:     " + nEntityPairs)
  println("# relations:        " + relVocab.size)

}

class NerData(inDir:String, evoc:Vocab, rvoc:Vocab, fvoc:Vocab) extends EntityPairData {
  def this(inDir:String) = this(inDir, null, null, null)

  var newVocab = true
  val entityVocab  = if(evoc != null) { evoc } else { new Vocab }
  val relVocab     = if(rvoc != null) { rvoc } else { new Vocab }
  var featureVocab = if(fvoc != null) { newVocab=false; fvoc } else { new Vocab }

  val fbData = new FreebaseUtils.FreebaseData("../data/freebase-datadump-quadruples.tsv.bz2.filtered")

  //First pass: figure out vocabuarly sizes
  var nEntities = 0
  for(line <- scala.io.Source.fromFile(inDir + "/labels2").getLines()) {
    val rels = line.trim.split(",").filter(_ != "NONE").map(x => relVocab(x)) ++ Array(relVocab("NA"))
  }
  for(line <- scala.io.Source.fromFile(inDir + "/features2").getLines()) {
    val mentions = line.trim.split("\t")
    for(mention <- mentions) {
      val features = mention.split(" ").map(x => featureVocab(x))
    }
  }
  for(line <- scala.io.Source.fromFile(inDir + "/entities2").getLines()) {
    val entity = entityVocab(line.trim)
    nEntities += 1
  }

  val nEntityPairs = nEntities
  val nFeat        = featureVocab.size
  val nRel         = relVocab.size

  println("nRel:" + nRel)
  println("feature vocab size: " + featureVocab.size)
  println("# entity pairs:     " + nEntityPairs)
  println("# relations:        " + relVocab.size)

  //Second pass: read in the data
  val data = new Array[EntityPair](nEntities)
  
  val fLabels   = new BufferedReader(new InputStreamReader(new FileInputStream(inDir + "/labels2")))
  val fFeatures = new BufferedReader(new InputStreamReader(new FileInputStream(inDir + "/features2")))
  var lineNum = 0
  for(line <- scala.io.Source.fromFile(inDir + "/entities2").getLines()) {
    val entity   = entityVocab(line.trim)
    val mentions = fFeatures.readLine.trim.split("\t")
    val rels     = fLabels.readLine.trim.split(",").filter(_ != "NONE").map(x => relVocab(x)) ++ Array(relVocab("NA"))

    val relations = DenseVector.zeros[Double](relVocab.size)
    rels.foreach( 
      {r => 
	relations(r) = 1.0
     })

    val features = new Array[SparseVectorCol[Double]](mentions.length)
    for(i <- 0 until mentions.length) {
      val mention = mentions(i)
      features(i) = SparseVector.zeros[Double](featureVocab.size + 1)
      features(i)(featureVocab.size) = 1.0		//Bias feature
      mention.split(" ").map(x => featureVocab(x)).filter(x => x >= 0).foreach(
	{x =>
	  features(i)(x) = 1.0
       })
    }

    data(lineNum) = new EntityPair(entity, entity, features, relations.t)
    lineNum += 1
  }  
}

//Class to read and manage data from google protobuf file format
class ProtobufData(inFile:String, evoc:Vocab, rvoc:Vocab, fvoc:Vocab, readSentences:Boolean) extends EntityPairData { 
  def this(inFile:String, readSentences:Boolean) = this(inFile, null, null, null, readSentences)
  def this(inFile:String) = this(inFile, null, null, null, false)

  var newVocab = true
  val entityVocab  = if(evoc != null) { evoc } else { new Vocab }
  val relVocab     = if(rvoc != null) { rvoc } else { new Vocab }
  var featureVocab = if(fvoc != null) { newVocab=false; fvoc } else { new Vocab }

  var is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(inFile)))
  var r = Relation.parseDelimitedFrom(is);
  var nEntityPairs = 0

  val fbData = new FreebaseUtils.FreebaseData("../data/freebase-datadump-quadruples.tsv.bz2.filtered")

  //First pass: figure out vocabulary sizes
  while(r != null) {
    val e1 = entityVocab(r.getSourceGuid)
    val e2 = entityVocab(r.getDestGuid)

    if(newVocab) {
      var relStrings:List[String] = (fbData.getRels(entityVocab(e1), entityVocab(e2)) ++ r.getRelType.split(",")).filter(x => x != "NA").toSet.toList
      if(relStrings.length == 0) {
	relStrings = List("NA")
      }
      for(rel <- relStrings) {
      //for(rel <- r.getRelType.split(",")) {
      //for(rel <- fbData.getRels(entityVocab(e1), entityVocab(e2))) {
      //for(rel <- fbData.getRels(entityVocab(e1), entityVocab(e2)) ++ r.getRelType.split(",")) {
	relVocab(rel)
      }
      for(i <- 0 until r.getMentionCount) {
	val m = r.getMention(i)
	for(j <- 0 until m.getFeatureCount) {
	  featureVocab(m.getFeature(j))
	}
      }
    }

    nEntityPairs += 1
    r = Relation.parseDelimitedFrom(is)
  }

  val nRel  = relVocab.size
  val nFeat = featureVocab.size

  println("nRel:" + nRel)

  val data = new Array[EntityPair](nEntityPairs)

  //Second pass: Populate the data structures
  is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(inFile)))
  r = Relation.parseDelimitedFrom(is);
  nEntityPairs = 0
  while(r != null) {
    //println(r.getRelType())

    val e1 = entityVocab(r.getSourceGuid)
    val e2 = entityVocab(r.getDestGuid)

    val relations = DenseVector.zeros[Double](relVocab.size)

    var relStrings = (fbData.getRels(entityVocab(e1), entityVocab(e2)) ++ r.getRelType.split(",")).filter(x => x != "NA").toSet.toList
    if(relStrings.length == 0) {
      relStrings = List("NA")
    }
    for(rel <- relStrings) {    
    //for(rel <- r.getRelType.split(",")) {
    //for(rel <- fbData.getRels(entityVocab(e1), entityVocab(e2))) {
    //for(rel <- fbData.getRels(entityVocab(e1), entityVocab(e2)) ++ r.getRelType.split(",")) {
      val r = relVocab(rel)
      if(r >= 0) {
	relations(r) = 1.0
      }
    }

    val mentions           = new Array[SparseVectorCol[Double]](r.getMentionCount)
    val sentences          = new Array[String](r.getMentionCount)
    val annotatedSentences = new Array[String](r.getMentionCount)
    //val mentions = new Array[DenseVectorCol[Double]](r.getMentionCount)
    for(i <- 0 until r.getMentionCount) {
      var nFeatures = 0.0
      mentions(i) = SparseVector.zeros[Double](featureVocab.size + 1)
      mentions(i)(featureVocab.size) = 1.0	//Bias feature
      val m = r.getMention(i)
      sentences(i)          = m.getSentence
      //println(m.getSentence)
      for(j <- 0 until m.getFeatureCount) {
	val f = featureVocab(m.getFeature(j))
	if(f >= 0) {
	  mentions(i)(f) = 1.0
	  nFeatures += 1.0
	}
      }
    }

    if(readSentences) {
      data(nEntityPairs) = new EntityPair(e1, e2, mentions, relations.t, sentences)
    } else {
      data(nEntityPairs) = new EntityPair(e1, e2, mentions, relations.t)
    }

//    if(nEntityPairs % 1000 == 0) {
//      println(nEntityPairs)
//    }

    nEntityPairs += 1
    r = Relation.parseDelimitedFrom(is)
  }

  println("feature vocab size: " + featureVocab.size)
  println("# entity pairs:     " + nEntityPairs)
  println("# relations:        " + relVocab.size)
}
