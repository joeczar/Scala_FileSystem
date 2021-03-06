package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.Files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

class Mkdir(name: String) extends CreateEntry(name) {

  override def CreateSpecificEntry(state: State, entryName: String): DirEntry =
    Directory.empty(state.wd.path, entryName)
}

