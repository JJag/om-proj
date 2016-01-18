package main

import java.io.File

import classifier.CodebookClassifier
import util.CollectionUtils.WithMostCommonElem
import vq.Lbg

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
    val testSet: Map[String, Seq[Mfcc]] = testList.groupBy(_._1).mapValues(_.map(_._2))

    println("Wczytano dane")
    println(s"trainingSet.size = ${trainingSet.size}")


    val lbg = new Lbg(512)
    val codebooks: Map[String, Seq[Mfcc]] = trainingSet.map { case (k, features) =>
      println("mama")
      (k, lbg.quantize(features))
    }
    println("wtf")

    val classifier = new CodebookClassifier(codebooks)
    val classified: Map[String, Seq[String]] = testSet.mapValues(_.map(classifier.classify))
    val N = 50
    val classifiedGrouped = testSet.mapValues(_.grouped(N))
    val classifedVoted = classifiedGrouped.mapValues(_.map(_.mostCommonElem))
    val n: Int = classifedVoted.map { case (k, v) => v.length }.sum
    val correct: Int = classifedVoted.map { case (k, v) => v.count(_ == k) }.sum
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
