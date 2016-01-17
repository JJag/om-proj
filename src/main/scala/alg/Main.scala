package alg

import java.io.File

import scala.io.Source
import scala.util.Random

object Main {

  type Mfcc = Vector[Float]

  def main(args: Array[String]) {

    val dataset: Seq[(String, Mfcc)] = new File("C:/Users/Jakub/Desktop/WOK").
      listFiles.toSeq.
      map(_.getAbsolutePath).
      flatMap(loadSet)
    val shuffledDataset = Random.shuffle(dataset)
    //    val trainingSet: Map[String, Seq[Mfcc]] = loadSet("C:/Users/Jakub/Desktop/train.csv")
    //    val testSet: Map[String, Seq[Mfcc]] = loadSet("C:/Users/Jakub/Desktop/test.csv")
    val (trainingList, testList) = dataset.splitAt((0.8 * dataset.length).toInt)

    val trainingSet: Map[String, Seq[Mfcc]] = trainingList.groupBy(_._1).mapValues(_.map(_._2))
    val testSet = testList.groupBy(_._1).mapValues(_.map(_._2))



    println("Wczytano dane")
    println(trainingSet.size)
    val codebooks: Map[String, Seq[Mfcc]] = trainingSet.map { case (k, features) =>
      println("mama")
      (k, Lbg(512, features))
    }
    println("wtf")
    val classifier = new CodebookClassifier(codebooks)

    val classified = testSet.mapValues(_.map(classifier.classify))
    val n: Int = classified.map { case (k, v) => v.length }.sum
    val correct: Int = classified.map { case (k, v) => v.count(_ == k) }.sum

    val accuracy = correct.toFloat / n

    println("Acc = " + accuracy)

  }

  def loadSet(filepath: String): Seq[(String, Mfcc)] = {
    val linesItr: Iterator[String] = Source.fromFile(filepath).getLines
    linesItr.map { line =>
      val words = line.split(",")
      val label = words.last
      val mfcc = words.init.map(_.toFloat).toVector
      (label, mfcc)
    }.toList
  }
}
