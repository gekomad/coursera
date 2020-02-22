package stackoverflow

import javax.swing.text.html.CSS

import org.apache.commons.codec.language.bm.Lang
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD

import annotation.tailrec
import scala.collection.immutable
import scala.reflect.ClassTag
import scala.util.{Random}

/** A raw stackoverflow posting, either a question or an answer */
case class Posting(postingType: Int, id: Int, acceptedAnswer: Option[Int], parentId: Option[QID], score: Int, tags: Option[String]) extends Serializable //TODO simple class?

/*

<postTypeId>,<id>,[<acceptedAnswer>],[<parentId>],<score>,[<tag>]
LOAD DATA LOCAL INFILE 'stackoverflow.csv' INTO TABLE stackoverflow CHARACTER SET UTF8 FIELDS TERMINATED BY ','

Resulting clusters:
  Score  Dominant language (%percent)  Questions
================================================
      0  Groovy            (100.0%)         1631
      0  MATLAB            (100.0%)         3725
      1  C#                (100.0%)       361835
      1  Ruby              (100.0%)        54727
      1  CSS               (100.0%)       113598
      1  PHP               (100.0%)       315771
      1  Objective-C       (100.0%)        94745
      1  JavaScript        (100.0%)       365649
      1  Java              (100.0%)       383473
      2  Perl              (100.0%)        19229
      2  MATLAB            (100.0%)         7989
      2  Clojure           (100.0%)         3441
      2  Python            (100.0%)       174586
      2  C++               (100.0%)       181255
      2  Scala             (100.0%)        12423
      3  Groovy            (100.0%)         1390
      4  Haskell           (100.0%)        10362
      5  MATLAB            (100.0%)         2774
      9  Perl              (100.0%)         4716
     14  Clojure           (100.0%)          595
     25  Scala             (100.0%)          728
     36  Groovy            (100.0%)           32
     53  Haskell           (100.0%)          202
     66  Clojure           (100.0%)           57
     78  Perl              (100.0%)           56
     79  C#                (100.0%)         2585
     85  Ruby              (100.0%)          648
     97  Objective-C       (100.0%)          784
    130  Scala             (100.0%)           47
    139  PHP               (100.0%)          475
    172  CSS               (100.0%)          358
    212  C++               (100.0%)          264
    227  Python            (100.0%)          400
    249  Java              (100.0%)          483
    377  JavaScript        (100.0%)          431
    443  C#                (100.0%)          147
    503  Objective-C       (100.0%)           73
    546  Ruby              (100.0%)           34
    766  CSS               (100.0%)           26
    887  PHP               (100.0%)           13
   1130  Haskell           (100.0%)            2
   1269  Python            (100.0%)           19
   1290  C++               (100.0%)            9
   1895  JavaScript        (100.0%)           33
  10271  Java              (100.0%)            2


1,27398936,,,0,PHP
1,28903923,,,0,PHP
2,28904080,,28903923,0,

<postTypeId>:     Type of the post. Type 1 = question, type 2 = answer.

<id>:             Unique id of the post (regardless of type).

<acceptedAnswer>: Id of the accepted answer post. This
                  information is optional, so maybe be missing
                  indicated by an empty string.

<parentId>:       For an answer: id of the corresponding
                  question. For a question:missing, indicated
                  by an empty string.

<score>:          The StackOverflow score (based on user
                  votes).

<tag>:            The tag indicates the programming language
                  that the post is about, in case it's a
                  question, or missing in case it's an answer.

*/

/** The main class */
object StackOverflow extends StackOverflow {

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("StackOverflow")
  @transient lazy val sc: SparkContext = new SparkContext(conf)


  def main(args: Array[String]): Unit = {

    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")

    val raw = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val answerHighScore = scoredPostings(grouped) //.sample(true, 0.1, 0)

    val vectors: RDD[(LangIndex, HighScore)] = vectorPostings(answerHighScore)

    assert(vectors.count() == 2121822, "Incorrect number of vectors: " + vectors.count())

    //crea 45 cluster
    val sampleVector: Array[(LangIndex, HighScore)] = sampleVectors(vectors)

    val means: Array[(Int, Int)] = kmeans(sampleVector, vectors)
    val results: Array[(String, Double, Int, HighScore)] = clusterResults(means, vectors)
    printResults(results)
  }
}

/** The parsing and kmeans methods */
class StackOverflow extends Serializable {

  /** Languages */
  val langs =
    List(
      "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
      "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

  /** K-means parameter: How "far apart" languages should be for the kmeans algorithm? */
  def langSpread = 50000

  assert(langSpread > 0, "If langSpread is zero we can't recover the language from the input data!")

  /** K-means parameter: Number of clusters */
  def kmeansKernels = 45

  /** K-means parameter: Convergence criteria */
  def kmeansEta: Double = 20.0D

  /** K-means parameter: Maximum iterations */
  def kmeansMaxIterations = 120


  //
  //
  // Parsing utilities:
  //
  //

  /** Load postings from the given file */
  def rawPostings(lines: RDD[String]): RDD[Posting] =
    lines.map(line => {
      val arr = line.split(",")
      Posting(postingType = arr(0).toInt,
        id = arr(1).toInt,
        acceptedAnswer = if (arr(2) == "") None else Some(arr(2).toInt),
        parentId = if (arr(3) == "") None else Some(arr(3).toInt),
        score = arr(4).toInt,
        tags = if (arr.length >= 6) Some(arr(5).intern()) else None)
    })

  def groupedPostings(postings: RDD[Posting]): RDD[(QID, Iterable[(Question, Answer)])] = {
    val questions = postings.filter(_.postingType == 1).map(x => (x.id, x))
    val answers = for {
      posting <- postings.filter(_.postingType == 2)
      parentId <- posting.parentId
    } yield (parentId, posting)

    val o: RDD[(HighScore, (Question, Question))] = questions.join(answers)
    val p: RDD[(HighScore, Iterable[(Question, Question)])] = o.groupByKey
    p
  }


  /** Compute the maximum score for each posting
    * select id,max(score) from stackoverflow where postTypeId=1 group by id
    * */


  def scoredPostings(grouped: RDD[(Int, Iterable[(Posting, Posting)])]): RDD[(Posting, Int)] = {

    def answerHighScore(as: Array[Posting]): Int = {
      var highScore = 0
      var i = 0
      while (i < as.length) {
        val score = as(i).score
        if (score > highScore)
          highScore = score
        i += 1
      }
      highScore
    }

    grouped.map { case (_, iterable) =>
      (iterable.head._1, answerHighScore(iterable.map(_._2).toArray))      
    }
  }

  def vectorPostings(scored: RDD[(Posting, Int)]): RDD[(Int, Int)] = {
    /** Return optional index of first language that occurs in `tags`. */
    def firstLangInTag(tag: Option[String], ls: List[String]): Option[Int] = {
      if (tag.isEmpty) None
      else if (ls.isEmpty) None
      else if (tag.get == ls.head) Some(0) // index: 0
      else {
        val tmp = firstLangInTag(tag, ls.tail)
        tmp match {
          case None => None
          case Some(i) => Some(i + 1) // index i in ls.tail => index i+1
        }
      }
    }

    val output = scored.map(score => (firstLangInTag(score._1.tags, langs).getOrElse(0) * langSpread, score._2))
    output.cache()
    output
  }

  /** Sample the vectors */
  def sampleVectors(vectors: RDD[(LangIndex, HighScore)]): Array[(Int, Int)] = {

    assert(kmeansKernels % langs.length == 0, "kmeansKernels should be a multiple of the number of languages studied.")
    val perLang = kmeansKernels / langs.length

    // http://en.wikipedia.org/wiki/Reservoir_sampling
    def reservoirSampling(lang: Int, iter: Iterator[Int], size: Int): Array[Int] = {
      val res = new Array[Int](size)
      val rnd = new util.Random(lang)

      for (i <- 0 until size) {
        assert(iter.hasNext, s"iterator must have at least $size elements")
        res(i) = iter.next
      }

      var i = size.toLong
      while (iter.hasNext) {
        val elt = iter.next
        val j = math.abs(rnd.nextLong) % i
        if (j < size)
          res(j.toInt) = elt
        i += 1
      }

      res
    }

    val res: Array[(LangIndex, HighScore)] =
      if (langSpread < 500)
      // sample the space regardless of the language
        vectors.takeSample(false, kmeansKernels, 42)
      else {
        // sample the space uniformly from each language partition
        val x = vectors.groupByKey
        x.flatMap({
          case (lang, vectors) => reservoirSampling(lang, vectors.toIterator, perLang).map((lang, _))
        }).collect()
      }
    assert(res.length == kmeansKernels, res.length)
    res
  }


  //
  //
  //  Kmeans method:
  //
  //

  /** Main kmeans computation */
  @tailrec final def kmeans(means: Array[(Int, Int)], vectors: RDD[(Int, Int)], iter: Int = 1, debug: Boolean = true): Array[(Int, Int)] = {
    //find closest centroid 2121822 (10,(1500000,0))
    val closest: RDD[(Int, (Int, Int))] = vectors.map(vector => (findClosest(vector, means), vector))

    //group by centroid 42 ( < 45) alcuni vengono raggruppati nello stesso means
    val updatedMeans: Map[Int, (Int, Int)] = closest.groupByKey().mapValues(averageVectors).collect().toMap

    // 45 ricrea i means prendendoli da updatedMeans se non esiste prende il vecchio means
    val newMeans: Array[(Int, Int)] = means.indices.map { centroid =>
      updatedMeans.getOrElse(centroid, means(centroid))
    }.toArray

    // TODO: Fill in the newMeans array
    val distance = euclideanDistance(means, newMeans)

    if (debug) {
      println(
        s"""Iteration: $iter
           |  * current distance: $distance
           |  * desired distance: $kmeansEta
           |  * means:""".stripMargin)
      for (idx <- 0 until kmeansKernels)
        println(f"   ${means(idx).toString}%20s ==> ${newMeans(idx).toString}%20s  " +
          f"  distance: ${euclideanDistance(means(idx), newMeans(idx))}%8.0f")
    }

    if (converged(distance))
      newMeans
    else if (iter < kmeansMaxIterations)
      kmeans(newMeans, vectors, iter + 1, debug)
    else {
      if (debug) {
        println("Reached max iterations!")
      }
      newMeans
    }
  }


  //
  //
  //  Kmeans utilities:
  //
  //

  /** Decide whether the kmeans clustering converged */
  def converged(distance: Double) =
    distance < kmeansEta


  /** Return the euclidean distance between two points */
  def euclideanDistance(v1: (Int, Int), v2: (Int, Int)): Double = {
    val part1 = (v1._1 - v2._1).toDouble * (v1._1 - v2._1)
    val part2 = (v1._2 - v2._2).toDouble * (v1._2 - v2._2)
    part1 + part2
  }

  /** Return the euclidean distance between two points */
  def euclideanDistance(a1: Array[(Int, Int)], a2: Array[(Int, Int)]): Double = {
    assert(a1.length == a2.length)
    var sum = 0d
    var idx = 0
    while (idx < a1.length) {
      sum += euclideanDistance(a1(idx), a2(idx))
      idx += 1
    }
    sum
  }

  /** Return the closest point */
  def findClosest(p: (Int, Int), centers: Array[(Int, Int)]): Int = {
    var bestIndex = 0
    var closest = Double.PositiveInfinity
    for (i <- 0 until centers.length) {
      val tempDist = euclideanDistance(p, centers(i))
      if (tempDist < closest) {
        closest = tempDist
        bestIndex = i
      }
    }
    bestIndex
  }


  /** Average the vectors */
  def averageVectors(ps: Iterable[(Int, Int)]): (Int, Int) = {
    val iter = ps.iterator
    var count = 0
    var comp1: Long = 0
    var comp2: Long = 0
    while (iter.hasNext) {
      val item = iter.next
      comp1 += item._1
      comp2 += item._2
      count += 1
    }
    ((comp1 / count).toInt, (comp2 / count).toInt)
  }

  private[this] def medianCalculation(xs: List[Int]): Int = {
    val xsSorted = xs.sortWith(_ < _)
    val xsLen = xs.length
    if (xsLen % 2 != 0)
      xsSorted.drop(xsLen / 2).head
    else {
      val xsStart = xsLen / 2 - 1
      val xsSortedDropped = xsSorted.drop(xsStart)
      (xsSortedDropped.head + xsSortedDropped.tail.head) / 2
    }
  }


  def clusterResults(means: Array[(LangIndex, HighScore)], vectors: RDD[(LangIndex, HighScore)]): Array[(String, Double, Int, Int)] = {
    val closest: RDD[(ArrayIndex, (LangIndex, HighScore))] = vectors.map(p => (findClosest(p, means), p))
    val closestGrouped: RDD[(ArrayIndex, Iterable[(LangIndex, HighScore)])] = closest.groupByKey()

    val median: RDD[(HighScore, (String, Double, HighScore, HighScore))] = closestGrouped.mapValues { vs =>

      val langIndex = vs.groupBy(_._1).maxBy(_._2.size)._1

      val langLabel: String = langs(langIndex / langSpread) // most common language in the cluster

      val clusterSize: Int = vs.size
      val langPercent = 100 * vs.count(_._1 == langIndex) / clusterSize.toDouble
      val medianScore = medianCalculation(vs.map(_._2).toList)

      (langLabel, langPercent, clusterSize, medianScore)
    }

     median.collect().map(_._2).sortBy(_._4)
    
  }

  def printResults(results: Array[(String, Double, Int, Int)]): Unit = {
    println("Resulting clusters:")
    println("  Score  Dominant language (%percent)  Questions")
    println("================================================")
    for ((lang, percent, size, score) <- results)
      println(f"${score}%7d  ${lang}%-17s (${percent}%-5.1f%%)      ${size}%7d")
  }
}
