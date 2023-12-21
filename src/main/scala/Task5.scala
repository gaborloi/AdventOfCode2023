package org.practice.advent

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

object Task5 {

  case class Range(start: Long, length: Long) {
    def intersection(r2: Range): Option[Range] = {
      val newStart = math.max(start, r2.start)
      val newEnd = math.min(start + length, r2.start + r2.length)
      val newLength = newEnd - newStart
      newLength match {
        case l if l > 0 => Some(Range(newStart, newLength))
        case _ => None
      }
    }

    def upperBound(): Long = start + length
    def shift(diff: Long): Range = Range(start + diff, length)
  }
  case class DataMappingItem(destinationRangeStart: Long, sourceRangeStart: Long, length: Long) {
    def sourceRange(): Range = Range(sourceRangeStart, length)
//    def destinationRange(): Range = Range(destinationRangeStart, length)

    def shiftValue(): Long = destinationRangeStart - sourceRangeStart

    def reverse(): DataMappingItem = DataMappingItem(sourceRangeStart, destinationRangeStart, length)
  }

  class DataMapping(rawList: List[DataMappingItem]) {
    val mappingList: List[DataMappingItem] = enrich(rawList)
    def transform(rangeList: List[Range]): List[Range] = rangeList.flatMap {
      r => mappingList.map( mi => mi.sourceRange().intersection(r).map(_.shift(mi.shiftValue())))
    }.flatten

    def reverse(): DataMapping = new DataMapping(
      mappingList.map(_.reverse()).reverse
    )

    private def enrich(mappingList: List[DataMappingItem]): List[DataMappingItem] = {
      val sortedMappingList = mappingList.sortBy {
        _.sourceRangeStart
      }
      val enrichedList = sortedMappingList.foldLeft(List[DataMappingItem]()) { (dmList, dm) =>
        val boundary = if (dmList.isEmpty) 0L else dmList.last.sourceRange().upperBound()
        if (dm.sourceRangeStart != boundary) {
          dmList ++ List(DataMappingItem(boundary, boundary, dm.sourceRangeStart - boundary), dm)
        } else dmList ++ List(dm)
      }
      val finalBound = enrichedList.last.sourceRange().upperBound()
      enrichedList :+ DataMappingItem(finalBound, finalBound, Long.MaxValue / 10)//-finalBound-1) //TODO
    }
  }

  case class DataMappingChain(mappingChain: List[DataMapping]) {
    def reverse(): List[DataMapping] = mappingChain.map(_.reverse()).reverse

    def transform(inputRanges: List[Range]): List[Range] = mappingChain.foldLeft(inputRanges) {
      (nextRange, dataMapping) =>
        dataMapping.transform(nextRange)
    }
  }
  def parseFile1(file: BufferedSource): Long = {
    var seed = List[Range]()
    val mappings = ListBuffer[DataMapping]()
    var currentList = ListBuffer[DataMappingItem]()
    file.getLines() foreach {
      case line@seeds if line.startsWith("seeds") => seed = parseSeeds(seeds)
      case "" => if (currentList.nonEmpty) mappings.addOne(new DataMapping(currentList.toList))
      case line if line(0).isLetter => currentList = ListBuffer[DataMappingItem]()
      case line@digits if line(0).isDigit => currentList.addOne(parseMapping(digits))
    }
    mappings.addOne(new DataMapping(currentList.toList))

    val locations = findLocation(DataMappingChain(mappings.toList), seed)
    locations.map(_.start).min
  }

  def findLocation(chain: DataMappingChain, seeds: List[Range]): List[Range] = chain.transform(seeds)

  def parseMapping(input: String): DataMappingItem = {
    val values = input.split(" ")
    DataMappingItem(values(0).toLong, values(1).toLong, values(2).toLong)
  }
  //TODO: order seeds?
  def parseSeeds(input: String): List[Range] = input.substring(7).split(" ").map(s => Range(s.toLong, 1L)).toList

  def parseFile2(file: BufferedSource): Long = {
    var seed = List[Range]()
    val mappings = ListBuffer[DataMapping]()
    var currentList = ListBuffer[DataMappingItem]()
    file.getLines() foreach {
      case line@seeds if line.startsWith("seeds") => seed = parseSeedsV2(seeds)
      case "" => if (currentList.nonEmpty) mappings.addOne(new DataMapping(currentList.toList))
      case line if line(0).isLetter => currentList = ListBuffer[DataMappingItem]()
      case line@digits if line(0).isDigit => currentList.addOne(parseMapping(digits))
    }
    mappings.addOne(new DataMapping(currentList.toList))

    backwardMapping(DataMappingChain(mappings.toList), seed)
  }

  def parseSeedsV2(input: String): List[Range] = {
    val seedInput = input.substring(7).split(" ").map(s => s.toLong).toList
    val starts = seedInput.zipWithIndex.filter{ s => (s._2 % 2) == 0 }.map(_._1)
    val lengths = seedInput.zipWithIndex.filter{ s => (s._2 % 2) == 1 }.map(_._1)

    starts.zip(lengths).map { t => Range(t._1, t._2) }.sortBy( _.start )
  }
  def backwardMapping(chain: DataMappingChain, seeds: List[Range]): Long = {
    val preppedChain = chain.reverse()
    preppedChain.head.mappingList foreach  { dm =>
      val seedRange = findRanges(0, List(dm.sourceRange()), preppedChain)
      val availableSeeds = checkSeeds(seedRange, seeds)
      availableSeeds match {
        case Some(r) => return findLocation(chain, List(r)).head.start
        case None => ""
      }
    }
    -1
  }

  @tailrec
  def findRanges(levelIdx: Int, destinationRange: List[Range], allDataMappings: List[DataMapping]): List[Range] = {
    val sourceDataMappings = destinationRange flatMap { destRange =>
      allDataMappings(levelIdx).transform(List(destRange))
    }

    if (levelIdx + 1 < allDataMappings.length) {
      findRanges(levelIdx + 1, sourceDataMappings, allDataMappings)
    } else sourceDataMappings
  }

  def checkSeeds(seedsToTest: List[Range], actualSeeds: List[Range]): Option[Range] = {
    seedsToTest foreach { t =>
      actualSeeds foreach { a =>
        t.intersection(a) match {
          case Some(r) => return Some(r)
          case None => ""
        }
      }
    }
    None
  }
}