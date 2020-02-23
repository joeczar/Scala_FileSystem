package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.Files.{Directory, File}
import com.rtjvm.scala.oop.filesystem.State

class Echo(args: List[String]) extends Command {
  override def apply(state: State): State = {
    // if no args => state
    // if args == 1 => print to console
    /*
      multiple args
        if > echo to file (may create file if none)
        if >> append to file
        else echo to console
     */

    if (args.isEmpty) state
    else if (args.length == 1) state.setMessage(args.head)
    else {
      val operator = args(args.length - 2)
      val filename = args.last
      val contents = createContent(args, args.length - 2)

      if(">>" == operator)
        doEcho(state, contents, filename, append = true)
      else if (">" == operator)
        doEcho(state, contents, filename, append = false)
      else state.setMessage(createContent(args, args.length))

    }
  }
  def getRootAfterEcho(currentDir: Directory, path: List[String], contents: String, append: Boolean): Directory = {
    if (path.isEmpty) currentDir
    else if (path.tail.isEmpty){
      val dirEntry = currentDir.findEntry(path.head)
      if (dirEntry == null)
        currentDir.addEntry(new File(currentDir.path, path.head, contents))
      else if (dirEntry.isDirectory) currentDir
      else
        if (append) currentDir.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
        else currentDir.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
  } else {
      val nextDir = currentDir.findEntry(path.head).asDirectory
      val newNextDir = getRootAfterEcho(nextDir, path.tail, contents, append)

      if (newNextDir == nextDir) currentDir
      else currentDir.replaceEntry(path.head, newNextDir)
    }//Find file to create or add content to

  }

  def doEcho(state: State, contents: String, filename: String, append: Boolean): State = {
    if (filename.contains(Directory.SEPARATOR))
      state.setMessage("Echo: filename must not contain separators")
    else {
      val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ filename, contents, append)
      if (newRoot == state.root)
        state.setMessage(s"$filename: no such file ¯\\_(ツ)_/¯")
      else
        State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }
  }
  // TOP INDEX NON_INCLUSIVE
  def createContent(args: List[String], topIndex: Int): String = {
    @scala.annotation.tailrec
    def ccHelper(currentIndex: Int, acc: String): String = {
      if (currentIndex >= topIndex) acc
      else ccHelper(currentIndex + 1, acc + " " + args(currentIndex))
    }
    ccHelper(0, "")
  }
}
