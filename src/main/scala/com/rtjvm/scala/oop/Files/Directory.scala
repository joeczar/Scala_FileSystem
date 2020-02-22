package com.rtjvm.scala.oop.Files

import java.nio.file.FileSystemException

class Directory (override val parentPath: String, override val name: String, val contents: List[DirEntry])
  extends DirEntry (parentPath, name) {

  def hasEntry(name: String): Boolean =
    findEntry(name) != null

  def getAllFoldersInPath: List[String] = {
    // path /a/b/c/d = List("a", "b", "c", "d")
    path.substring(1).split(Directory.SEPARATOR).toList.filter(x => !x.isEmpty)
  }

  def findDescendant(path: List[String]): DirEntry = {
    if (path.isEmpty) this
    else findEntry(path.head).asDirectory.findDescendant(path.tail)
  }

  def addEntry(newEntry: DirEntry): Directory =
    new Directory(parentPath, name, contents :+ newEntry)

  def findEntry(entryName: String): DirEntry = {
    @scala.annotation.tailrec
    def findEntryHelper(name: String, contentList: List[DirEntry]): DirEntry = {
      if (contentList.isEmpty) null
      else if (contentList.head.name == name) contentList.head
      else findEntryHelper(name, contentList.tail)
    }
    findEntryHelper(entryName, contents)
  }

  def replaceEntry(entryName: String, newEntry: DirEntry): Directory =
    new Directory(parentPath, name, contents.filter(e => e.name != entryName) :+ newEntry)

  def asDirectory: Directory = this
  def asFile: File = throw new FileSystemException("A directory cannot be converted to a file.")
  def getType: String = "Directory"
}
object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = Directory.empty("", "")
  def empty(parentPath: String, name: String): Directory =
    new Directory(parentPath, name, List())
}
