package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.collection.mutable
import scala.language.implicitConversions


object Task20 {

  type ModuleMap = Map[String, Module]
  case class MessageHandler(queue: mutable.Queue[Message]) {
    var highCounter: Int = 0
    var lowCounter: Int = 0

    def sendNextMessage(modules: ModuleMap): ModuleMap = {
      val message = queue.dequeue()
      if (message.receiver == "EXIT") {
        queue.dropWhile(_ => true)
        return modules
      }
      if (message.highPulse) highCounter += 1 else lowCounter += 1
      if (!modules.keySet.contains(message.receiver)) modules else {
        val updModule = modules(message.receiver).receivePulse(message)
        val modulesUpd = modules.updated(message.receiver, updModule)
        queue ++= updModule.sendPulse(modulesUpd)
        modulesUpd
      }
    }

    @tailrec
    final def transmit(modules: ModuleMap): ModuleMap = if(queue.nonEmpty) {
      transmit(sendNextMessage(modules))
    } else modules
    def pushTheButton(modules: ModuleMap): ModuleMap = {
      queue+= Message("button", "broadcaster", highPulse = false)
      transmit(modules)
    }

  }

  case class Message(sender: String, receiver: String, highPulse: Boolean)

  abstract class Module(val name: String, val destinations: List[String]) {
    def receivePulse(m: Message): Module

    def sendPulse(modules: Map[String, Module]): List[Message]
  }

  case class FlipFlop(
    ffname: String, ffdest: List[String], isOn: Boolean = false, latestPulse: Boolean = false
  ) extends Module(ffname, ffdest) {
    def receivePulse(m: Message): FlipFlop = {
      if (m.highPulse) FlipFlop(name, destinations, isOn, m.highPulse) else {
        FlipFlop(name, destinations, !isOn, m.highPulse)
      }
    }

    def sendPulse(modules: ModuleMap): List[Message] = {
      if (latestPulse) List() else destinations.map { receiver => Message(name, receiver, isOn)}
    }
  }

  case class Conjunction(cName: String, cDest: List[String], inputs: Map[String, Boolean]) extends Module(cName, cDest) {
    def receivePulse(m: Message): Conjunction = Conjunction(name, destinations, inputs.updated(m.sender, m.highPulse))

    def sendPulse(modules: ModuleMap): List[Message] = {
      val pulse = !inputs.values.toList.forall( b => b)
      destinations.map { receiver => Message(name, receiver, pulse) }
    }
  }

  case class Broadcaster(
    dest: List[String], lastPulse: Boolean = false
  ) extends Module("broadcaster", dest) {
    def receivePulse(m: Message): Broadcaster = Broadcaster(destinations, m.highPulse)

    def sendPulse(modules: ModuleMap): List[Message] = destinations.map {
      receiver => Message(name, receiver, lastPulse)
    }
  }

  case class Terminator(dest: List[String], val terminate: Boolean = false) extends Module("rx", dest) {
    def receivePulse(m: Message): Terminator = Terminator(destinations, !m.highPulse)

    def sendPulse(modules: ModuleMap): List[Message] =
      if(terminate) List(Message(name, "EXIT", highPulse = false)) else List()
  }

  def parseline(line: String): Module = {
    val parsed = line.split(" -> ")
    val name = parsed.head.drop(1)
    val destinations = parsed(1).replace(" ", "").split(",").toList
    line.head match {
      case '%' => FlipFlop(name, destinations)
      case '&' => Conjunction(name, destinations, Map())
      case 'b' => Broadcaster(destinations)
    }
  }

  def enrichConjunction(conjunction: Module, modules: List[Module]): Map[String, Boolean] = {
    modules.filter(m => m.destinations.contains(conjunction.name)).map(_.name -> false).toMap
  }
  @tailrec
  final def findTerminator(mh: MessageHandler, ms: ModuleMap, counter: Int = 0): Int = {
    val currentCount = counter + 1
    val moduleMap = mh.pushTheButton(ms)
    if (moduleMap("rx") match { case t: Terminator => t.terminate }) return currentCount
    if (currentCount > 1000000000) return -currentCount
    findTerminator(mh, moduleMap, currentCount)
  }

  @tailrec
  final def findRelevant(currents: Set[String], ms: ModuleMap, relevant: Set[String]): Set[String] = {
    val affectingRoots = currents.foldLeft(Set[String]()) { (affectingRoots, current) =>
      ms.filter { case (_, v) => v.destinations.contains(current) }.keySet.union(affectingRoots)
    }
    val newItems = affectingRoots.diff(relevant)
    if (newItems.isEmpty) return relevant
    findRelevant(newItems, ms, relevant.union(newItems))
  }

  def calcFile1(file: BufferedSource): Int = {
    val rawModules = file.getLines().map(parseline).toList
    val conjunctionsInputEnrich = rawModules.filter(_.isInstanceOf[Conjunction]).map { c =>
      Conjunction(c.name, c.destinations, enrichConjunction(c, rawModules))
    }
    val modules = (rawModules.filterNot(_.isInstanceOf[Conjunction]) ++ conjunctionsInputEnrich).map (m => m.name -> m).toMap
    val mh = MessageHandler(mutable.Queue[Message]())

    (1 to 1000).foldLeft(modules) { (ms, _) => mh.pushTheButton(ms) }
    mh.lowCounter * mh.highCounter
  }

  def calcFile2(file: BufferedSource): Long = {
    val rawModules = file.getLines().map(parseline).toList
    val conjunctionsInputEnrich = rawModules.filter(_.isInstanceOf[Conjunction]).map { c =>
      Conjunction(c.name, c.destinations, enrichConjunction(c, rawModules))
    }
    val modules = (
      rawModules.filterNot(_.isInstanceOf[Conjunction]) ++
        conjunctionsInputEnrich :+ Terminator(List())).map (m => m.name -> m).toMap
    val mh = MessageHandler(mutable.Queue[Message]())

//    val relevantSet = findRelevant(Set("rx"), modules, Set())
//    println(modules.size)
//    println(relevantSet.size)
//    println(modules.keySet.diff(relevantSet))
    1
    //    findTerminator(mh, modules)
  }
}