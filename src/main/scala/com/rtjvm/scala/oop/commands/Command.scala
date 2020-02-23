package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

trait Command extends (State => State) {
  //def apply(state: State): State

}
object Command {
  val MKDIR = "mkdir"
  val LS = "ls"
  val PWD = "pwd"
  val TOUCH = "touch"
  val CD = "cd"
  val RM = "rm"
  val ECHO = "echo"
  val CAT = "cat"

  def emptyCommand: Command = (state: State) => state.setMessage("")
  def incompleteCommand(name: String): Command = (state: State) => state.setMessage(name + ": incomplete command")

  def from(input: String): Command = {

    val tokens = input.split(" ")
    // TODO change to PM
    if (input.isEmpty || tokens.isEmpty) emptyCommand
    else if (tokens(0) == MKDIR) {
      if (tokens.length < 2) incompleteCommand(MKDIR)
      else new Mkdir(tokens(1))
    } else if (tokens(0) == LS) new Ls
      else if (tokens(0) == PWD) new Pwd
      else if(tokens(0)== TOUCH) {
        if (tokens.length < 2) incompleteCommand(TOUCH)
        else new Touch(tokens(1))
    } else if (tokens(0) == CD) {
      if (tokens.length < 2) incompleteCommand(CD)
      else new Cd(tokens(1))
    } else if (tokens(0) == RM) {
      if (tokens.length < 2) incompleteCommand(RM)
      else new Rm(tokens(1))
    } else if (tokens(0) == ECHO) {
      if (tokens.length < 2) incompleteCommand(ECHO)
      else new Echo(tokens.tail.toList)
    } else if (tokens(0) == CAT) {
      if (tokens.length < 2) incompleteCommand(CAT)
      else new Cat(tokens(1))
    }
    else new UnknownCommand

  }


}
