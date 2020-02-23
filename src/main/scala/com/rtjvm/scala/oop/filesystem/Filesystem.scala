package com.rtjvm.scala.oop.filesystem

import com.rtjvm.scala.oop.Files.Directory
import com.rtjvm.scala.oop.commands.Command

object Filesystem extends App {

  val root = Directory.ROOT
  // for some reason this is buggy.
  io.Source.stdin.getLines.foldLeft(State(root, root))((currentState, newLine) => {
    currentState.show
    Command.from(newLine).apply(currentState)
  })

}
