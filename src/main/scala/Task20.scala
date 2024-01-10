package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.collection.mutable
import scala.language.implicitConversions


object Task20 {

  type ModuleMap = Map[String, Module]
  case class MessageHandler(queue: mutable.Queue[Message], modules: ModuleMap) {
    var highCounter: Int = 0
    var lowCounter: Int = 0

    def sendNextMessage(): Unit = {
      val message = queue.dequeue()
      if (message.receiver == "EXIT") {
        queue.dropWhile(_ => true)
      } else {
        if (message.highPulse) highCounter += 1 else lowCounter += 1
        if (!modules.keySet.contains(message.receiver)) modules else {
          val currentModule = modules(message.receiver)
          currentModule.receivePulse(message)
          queue ++= currentModule.sendPulse(modules)
        }
      }
    }

    @tailrec
    final def transmit(): Unit = if(queue.nonEmpty) {
      sendNextMessage()
      transmit()
    }

    def pushTheButton(): Unit = {
      queue+= Message("button", "broadcaster", highPulse = false)
      transmit()
    }
  }

  case class Message(sender: String, receiver: String, highPulse: Boolean)

  abstract class Module(val name: String, val destinations: List[String]) {

    var period: Int
    def receivePulse(m: Message): Unit

    def sendPulse(modules: Map[String, Module]): List[Message]
  }

  case class FlipFlop(
    ffname: String, ffdest: List[String], var isOn: Boolean = false, var latestPulse: Boolean = false
  ) extends Module(ffname, ffdest) {

    var period: Int = 2
    def receivePulse(m: Message): Unit = {
      latestPulse =  m.highPulse
      if (!m.highPulse) isOn = !isOn
    }

    def sendPulse(modules: ModuleMap): List[Message] = {
      if (latestPulse) List() else destinations.map { receiver => Message(name, receiver, isOn)}
    }
  }

  case class Conjunction(cName: String, cDest: List[String], inputs: mutable.Map[String, Boolean]) extends Module(cName, cDest) {

    var period: Int = math.pow(2, inputs.size).toInt
    def receivePulse(m: Message): Unit = {
      inputs(m.sender) = m.highPulse
//      println(cName, inputs)
    }

    def sendPulse(modules: ModuleMap): List[Message] = {
      val pulse = !inputs.values.toList.forall( b => b)
      destinations.map { receiver => Message(name, receiver, pulse) }
    }
  }

  case class Broadcaster(
    dest: List[String], var lastPulse: Boolean = false
  ) extends Module("broadcaster", dest) {

    var period: Int = 1
    def receivePulse(m: Message): Unit = {
      lastPulse = m.highPulse
    }

    def sendPulse(modules: ModuleMap): List[Message] = destinations.map {
      receiver => Message(name, receiver, lastPulse)
    }
  }

  case class Terminator(dest: List[String], var terminate: Boolean = false) extends Module("rx", dest) {

    var period: Int = 1
    def receivePulse(m: Message): Unit = {
      terminate = !m.highPulse
    }

    def sendPulse(modules: ModuleMap): List[Message] =
      if(terminate) List(Message(name, "EXIT", highPulse = false)) else List()
  }

  def parseline(line: String): Module = {
    val parsed = line.split(" -> ")
    val name = parsed.head.drop(1)
    val destinations = parsed(1).replace(" ", "").split(",").toList
    line.head match {
      case '%' => FlipFlop(name, destinations)
      case '&' => Conjunction(name, destinations, mutable.Map[String, Boolean]())
      case 'b' => Broadcaster(destinations)
    }
  }

  def enrichConjunction(conjunction: Conjunction, modules: List[Module]): Unit = {
    modules.filter(m => m.destinations.contains(conjunction.name)).foreach { m => conjunction.inputs(m.name) = false }
  }
  @tailrec
  final def findTerminator(mh: MessageHandler, counter: Int = 0): Int = {
    val currentCount = counter + 1
    mh.pushTheButton()
    if (mh.modules("rx") match { case t: Terminator => t.terminate }) return currentCount
    if (currentCount > 3000000) return -currentCount
    findTerminator(mh, currentCount)
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
    rawModules.filter(_.isInstanceOf[Conjunction]) foreach { case c:Conjunction =>
      enrichConjunction(c, rawModules)
    }
    val modules = rawModules.map (m => m.name -> m).toMap
    val mh = MessageHandler(mutable.Queue[Message](), modules)

    for (_ <- 1 to 1000) {
      mh.pushTheButton()
    }
    mh.lowCounter * mh.highCounter
  }

  def affectsTo(moduleName: String, modules: ModuleMap, affectedSet: Set[String]): Set[String] = {
    val affected = modules(moduleName).destinations.toSet.filterNot( _ == "dn").diff(affectedSet)

    if (affected.isEmpty) {
      return affectedSet
    }
    affected.foldLeft(affectedSet.union(affected)) { (s, a) =>
      affectsTo(a, modules, s)
    }
  }

  @tailrec
 final def checkHash(toObserve: List[Module], previousHashes: List[Int], mh:MessageHandler, i: Int): (Int, Int) = {
    if (i > 10000) return (-1,-1)
    mh.pushTheButton()
    val thisHash = toObserve.hashCode()
    if (previousHashes.contains(thisHash))
      return (previousHashes.indexOf(thisHash),i)
    checkHash(toObserve, previousHashes :+ thisHash, mh, i + 1)
  }

  def calcFile2(file: BufferedSource): Long = {
    val rawModules = file.getLines().map(parseline).toList
    rawModules.filter(_.isInstanceOf[Conjunction]) foreach { case c:Conjunction =>
      enrichConjunction(c, rawModules)
    }
    val modules = (rawModules :+ Terminator(List())).map (m => m.name -> m).toMap
    val mh = MessageHandler(mutable.Queue[Message](), modules)

    val afset = modules("broadcaster").destinations.map { d =>
      affectsTo(d, modules, Set())
    }

    afset.foldLeft(1L) { (prod, af) =>
      val (_, res) = checkHash(af.map(modules(_)).toList, List(),mh, 0)
      prod * res.toLong
    }
  }
}