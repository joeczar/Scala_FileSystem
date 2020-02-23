package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.Files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(name: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)){
      state.setMessage(s"Entry: $name already exists")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(s"$name must not contain separators!")
    } else if (checkIllegal(name)) {
      state.setMessage(s"$name: illegal")
    } else {
      doCreateEntry(state, name)
    }
  }
  def checkIllegal(str: String): Boolean ={
    name.contains(".")
  }
  def doCreateEntry(state: State, name: String): State = {
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDirectory.addEntry(newEntry)
      else {

        val oldEntry = currentDirectory.findEntry(path.head).asDirectory

        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd = state.wd

    // 1. all directories in fullPath
    val allDirsInPath = wd.getAllFoldersInPath
    // 2. create new directory entry in the wd
    val newEntry: DirEntry = CreateSpecificEntry(state, name) //File.empty(wd.path, name)
    //val newDir = Directory.empty(wd.path, name)

    // 3. update whole directory structure starting from root
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)
    // 4. find new directory INSTANCE given  wd's  full path
    val newWD = newRoot.findDescendant(allDirsInPath).asDirectory

    State(newRoot, newWD)
  }
  def CreateSpecificEntry(state: State, entryName: String): DirEntry
}
