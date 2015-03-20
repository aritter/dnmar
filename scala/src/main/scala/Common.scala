package dnmar;

import scala.collection.mutable.HashMap
import math._
import scala.io._
import java.io._
import java.io.FileWriter
import scala.util.Random

object Utils {
  def deepCopy[A](a: A)(implicit m: reflect.Manifest[A]): A =
    util.Marshal.load[A](util.Marshal.dump(a))

  def count[A](xs:List[A]): HashMap[A,Int] = {
    val result = new HashMap[A,Int]()
    for(x <- xs) {
      result(x) = result.getOrElse(x, 0) + 1
    }
    return result
  }

  def bin2int(b:Array[Double]):List[Int] = {
    var result = List[Int]()
    for(i <- 0 until b.length) {
      if(b(i) == 1.0) {
	result ::= i
      }
    }
    return result.reverse
  }

  object Timer {
    var begin = new HashMap[String,Long]
    var sum   = new HashMap[String,Long]

    def reset {
      begin = new HashMap[String,Long]
      sum   = new HashMap[String,Long]
    }

    def reset(s:String):Double = {
      val time = stop(s)
      begin.remove(s)
      sum.remove(s)
      time
    }

    def start(s:String) = {
      if(Constants.DEBUG) {
	println("start " + s)
      }
      begin(s) = System.currentTimeMillis
    }

    def stop(s:String):Double = {
      if(Constants.DEBUG) {
	println("stop " + s)
      }
      val end = System.currentTimeMillis
      sum(s) = sum.getOrElse(s, 0L) + (end - begin(s))
      sum(s) / 1000.0
    }

    def print {
      for((s,t) <- sum.toList.sortBy(_._2).reverse) {
	println(s + "\t" + t / 1000.0 + " s")
      }
    }
  }
}

object StringUtils {
  def chomp(str:String) : String = {
    str.substring(0, str.lastIndexOf("\n"))
  }

  /** 
   * stripWS
   * Strips leading/trailing whitespace
   */ 
  def stripWS(str:String) : String = {
    str.replaceFirst("""^\s+""", "").replaceFirst("""[\s\n]+$""", "")
  }
}

object FreebaseUtils {
  class Mid2Name(mapFile:String, lower:Boolean) {
    def this(mapFile:String) = this(mapFile, false)

    val m2n = new HashMap[String,List[String]]
    val n2m = new HashMap[String,List[String]]

    for(line <- scala.io.Source.fromFile(mapFile).getLines()) {
      var Array(mid, rel, lang, name) = line.trim.split("\t")

      if(lower) {
	name = name.toLowerCase
      }

      if(!m2n.contains(mid)) {
	m2n += mid -> List()
      }
      m2n(mid) ::= name

      if(!n2m.contains(name)) {
	n2m += name -> List()
      }
      n2m(name) ::= mid
    }

    def apply(str:String):List[String] = {
      var s = str
      if(lower) {
	s = str.toLowerCase
      }

      if(s(0) == '/') {
	if(!m2n.contains(s)) {
	  return List[String]()
	} else {
	  return m2n(s)
	}
      } else {
	if(!n2m.contains(s)) {
	  return List[String]()
	} else {
	  return n2m(s)
	}
      }
    }
  }

  class FreebaseData(quadruplesFile:String) {
    val a1Rel       = new HashMap[String,List[String]]
    val a2Rel       = new HashMap[String,List[String]]
    val a1a2        = new HashMap[String,List[String]]
    val contains    = new HashMap[String,List[String]]
    val containedBy = new HashMap[String,List[String]]
    val entityCount = new HashMap[String,Double]
    
    for(line <- scala.io.Source.fromFile(quadruplesFile).getLines()) {
      var fields = line.trim.split("\t")
      if(fields.length == 3) {
	if(fields(1) == "/location/location/contains") {
	  if(!contains.contains(fields(0))) {
	    contains += fields(0) -> List[String]()
	  }
	  if(!containedBy.contains(fields(2))) {
	    containedBy += fields(2) -> List[String]()
	  }
	  contains(fields(0))    ::= fields(2)
	  containedBy(fields(2)) ::= fields(0)
	}

	val a1r = fields(0) + "\t" + fields(1)
	if(!a1Rel.contains(a1r)) {
	  a1Rel += a1r -> List[String]()
	}
	val a2r = fields(2) + "\t" + fields(1)
	if(!a2Rel.contains(a2r)) {
	  a2Rel += a2r -> List[String]()
	}
	a2Rel(a2r) ::= fields(2)
	val a12 = fields(0) + "\t" + fields(2)
	if(!a1a2.contains(a12)) {
	  a1a2 += a12 -> List[String]()
	}
	a1a2(a12) ::= fields(1)

	if(!entityCount.contains(fields(0))) {
	  entityCount += fields(0) -> 0.0
	}
	entityCount(fields(0)) += 1.0
	if(!entityCount.contains(fields(2))) {
	  entityCount += fields(2) -> 0.0
	}
	entityCount(fields(2)) += 1.0
      }
    }

    def aContainsB(a:String, b:String):Boolean = {
      if(!contains.contains(a)) {
	return false
      }
      val aContains = contains(a)
      if(aContains.contains(b)) {
	return true
      } else {
	for(c <- aContains) {
	  if(aContainsB(c,b)) {
	    return true
	  }
	}
	return false
      }
    }

    def entityFreq(e:String):Double = {
      if(!entityCount.contains(e)) {
	return 0.0
      } else {
	return entityCount(e)
      }
    }

    def aContainedByB(a:String, b:String):Boolean = {
      if(!containedBy.contains(a)) {
	return false
      }
      val aContainedBy = containedBy(a)
      if(aContainedBy.contains(b)) {
	return true
      } else {
	for(c <- aContainedBy) {
	  if(aContainedByB(c,b)) {
	    return true
	  }
	}
	return false
      }
    }

    def getRels(a1:String, a2:String):List[String] = {
      val key = a1 + "\t" + a2
      if(!a1a2.contains(key)) {
	return List("NA")
      }
      return a1a2(key)
    }
    
    def getA2s(a1:String, rel:String):List[String] = {
      val key = a1 + "\t" + rel
      if(!a1Rel.contains(key)) {
	return List[String]()
      }
      return a1Rel(key)
    }

    def getA1s(a2:String, rel:String):List[String] = {
      val key = a2 + "\t" + rel
      if(!a2Rel.contains(key)) {
	return List[String]()
      }
      return a2Rel(key)
    }
  }
  
  def guid2mid(guid:String) : String = {
    var GUID = guid
    //From Freebase wiki:
    /*
     * 1. Take the GUID
     * 2. Strip off the leading "#9202a8c04000641f8"
     * 3. Take what's left and interpret it as a hex number
     * 4. Express that number in base 32 using the character set 0123456789bcdfghjklmnpqrstvwxyz_ (ie the digits, the letters excluding the vowels and underscore)
     * Prepend /m/0 to what you've got.
     */
    val characters =List('0','1','2','3','4','5','6','7','8','9','b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','y','z','_').toArray

    GUID = GUID.replace("/guid/", "")

    var result = List[Char]()
    var number = Integer.parseInt(GUID.slice(17,GUID.length), 16)
    //println(GUID)
    //println(GUID.slice(17,GUID.length))
    while(number > 0) {
      result ::= characters(number & 31)
      number >>= 5
    }
    
    return "/m/0" + result.mkString("")
  }

  /************************************************************************************
   * Filter freebase quadruple dump to generate a file only containing relevant
   * entities (acording to the GUID vocab).  This is basically just for efficiency...
   ************************************************************************************
   */
  def filterFreebase(sourceFile:String, targetFile:String, guidVocab:Vocab, relVocab:Vocab) {
    //val buffered = new BufferedReader(new InputStreamReader(new org.apache.tools.bzip2.CBZip2InputStream(new BufferedInputStream(new FileInputStream(sourceFile)))))
    //Need to do this hack for some wierd reasons (googled...)

    /*
    val old2new = HashMap[String,String](
      "/business/person/company"->"/business/employment_tenure/company",
      "/people/person/place_lived"->"/people/person/places_lived",
      "/business/company/founders"->"/organization/organization/founders",
      "/business/company/locations"->"/organization/organization/locations",
      "/business/company/place_founded"->"/organization/organization/place_founded",
      "/business/company/advisors"->"/organization/organization/advisors"
    )
    */

    val new2old = HashMap[String,String](
      "/business/employment_tenure/company"->"/business/person/company",
      "/people/person/places_lived"->"/people/person/place_lived",
      "/organization/organization/founders"->"/business/company/founders",
      "/organization/organization/locations"->"/business/company/locations",
      "/organization/organization/place_founded"->"/business/company/place_founded",
      "/organization/organization/advisors"->"/business/company/advisors"
    )
    
    val readTwoBytes = new FileInputStream(sourceFile)
    readTwoBytes.read()
    readTwoBytes.read()
    val buffered = new BufferedReader(new InputStreamReader(new org.apache.tools.bzip2.CBZip2InputStream(readTwoBytes)))
    val fw = new FileWriter(targetFile)
    
    val locked = guidVocab.locked
    guidVocab.locked = true
    relVocab.locked = true
    var line = buffered.readLine()
    while(line != null) {
      //println(line)
      var fields = line.trim.split("\t")
      if(fields.length >= 3) {
	//if(old2new.contains(fields(1))) {
	if(new2old.contains(fields(1))) {
	  //fields(1) = old2new(fields(1))
	  fields(1) = new2old(fields(1))
	}
	try {
	  if(relVocab(fields(1)) >= 0 && (guidVocab(fields(0)) >= 0 || guidVocab(fields(2)) >= 0 || fields.length > 3 && guidVocab(fields(3)) >= 0)) {
	    fw.write(fields.mkString("\t") + "\n")
	  }
	} catch {
	  case e:Exception => println("skipping:" + line + "\n")
	}
      }
      line = buffered.readLine()
    }
    guidVocab.locked = locked
    relVocab.locked = locked
    fw.close()
  }
}

object MathUtils {
  val rnd = new Random

  def ArgMax(d:Array[Double]):Int = {
    var result = 0
    var max = d(0)
    for(i <- 1 to d.length-1) {
      if(d(i) > max) {
	result = i
	max = d(i)
      }
    }
    return(result)
  }

  //Sample from a discrete distribution
  def Sample(d:Array[Double]):Int = {
    var sum = 0.0
    val target = rnd.nextDouble * d.sum.toDouble
    
    for(i <- 0 to d.length-1) {
      sum += d(i)
      if(sum > target) {
	return(i)
      }
    }
    0
  }

  //Not the most efficient...
  def Mode[T](d:Array[T]) {
    var maxCount = 0
    var maxVal = d(0)

    for(i <- 0 until d.length) {
      var count = 0
      for(j <- (i+1) until d.length) {
	if(d(j) == d(i)) {
	  count += 1
	}
      }
      if(count > maxCount) {
	maxCount = count
	maxVal = d(i)
      }
    }
  }

  //NOTE: seems to work...  might be a couple boundary cases.
  def LogNormalize(d:Array[Double]) {
    //Log Exp Sum
    val max = d.max
    val logSum = max + log(d.map(x => exp(x - max)).sum)
    //Normalize
    for(i <- 0 to d.length-1) {
      d(i) -= logSum
    }
  }

  def LogExpSum(d:Array[Double]):Double = {
    val max = d.max
    return(max + log(d.map(x => exp(x - max)).sum))
  }

  def Normalize(d:Array[Double]):Array[Double] = {
    val sum = d.sum
    for(i <- 0 to d.length-1) {
      d(i) /= sum
    }
    d
  }

  def LogFactorial(n:Double):Double = {
    var result = 0.0
    for(i <- 2 to n.toInt) {
      result += log(i)
    }
    return result
  }

  def LogFactorial_old(n:Double):Double = {
    if(n <= 1) {
      return 0.0;
    } else {
      return log(n) + LogFactorial(n-1);
    }
  }

  //Rising factorial function
  def LogRff(n:Double, alpha:Double):Double = {
    if(n <= 1) {
      return 0.0;
    } else {
      return log(alpha + n) + LogFactorial(n-1);
    }    
  }
}
