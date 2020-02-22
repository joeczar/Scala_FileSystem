package com.rtjvm.scala.oop.filesystem

import com.rtjvm.scala.oop.Files.Directory

class State(val root: Directory, val wd: Directory, val output: String) {
  def show(): Unit = {
    print(State.SHELL_TOKEN)
    print(output)
  }
  def setMessage(message: String): State = {
    State(root, wd, message + "\n")
  }
}
object State {
  val SHELL_TOKEN = "$: "

  def apply(root: Directory, wd: Directory, output: String = ""): State =
    new State(root, wd, output)
}