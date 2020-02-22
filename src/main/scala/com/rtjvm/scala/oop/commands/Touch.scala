package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.Files.{DirEntry, File}
import com.rtjvm.scala.oop.filesystem.State

class Touch(name: String) extends CreateEntry(name) {

  override def CreateSpecificEntry(state: State, entryName: String): DirEntry =
    File.empty(state.wd.path, name)
}
