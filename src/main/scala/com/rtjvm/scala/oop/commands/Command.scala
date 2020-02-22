package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

trait Command {
  def apply(state: State): State

}
object Command {
  val MKDIR = "mkdir"
  val LS = "ls"
  val PWD = "pwd"
  val TOUCH = "touch"

  def emptyCommand: Command = (state: State) => state.setMessage("")
  def incompleteCommand(name: String): Command = (state: State) => state.setMessage(name + ": incomplete command")

  def from(input: String): Command = {

    val tokens = input.split(" ")

    if (input.isEmpty || tokens.isEmpty) emptyCommand
    else if (tokens(0) == MKDIR) {
      if (tokens.length < 2) incompleteCommand(MKDIR)
      else new Mkdir(tokens(1))
    } else if (tokens(0) == LS) new Ls
    else if (tokens(0) == PWD) new Pwd
    else if(tokens(0)== TOUCH) {
      if (tokens.length < 2) incompleteCommand(TOUCH)
      else new Touch(tokens(1))
    }
    else new UnknownCommand

  }


}
