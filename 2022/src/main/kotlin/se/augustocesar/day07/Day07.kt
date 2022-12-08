package se.augustocesar.day07

import se.augustocesar.Task

class Day07 : Task() {
    override fun partOne(): String {
        val fs = FileSystem.resolve(readInput())
        val res = fs.findFolders { it.size <= 100_000 }

        return res.sumOf { it.size }.toString()
    }

    override fun partTwo(): String {
        val totalStorage = 70_000_000
        val updateSize = 30_000_000
        val fs = FileSystem.resolve(readInput())

        val storageAvailable = totalStorage - fs.root.size
        val extraStorageNeeded = updateSize - storageAvailable

        val res = fs.findFolders { it.size >= extraStorageNeeded }

        return res.minOf { it.size }.toString()
    }
}

enum class EntryType { FILE, FOLDER }

class Entry(val name: String, val type: EntryType, val parent: Entry?, val children: ArrayList<Entry>?, var size: Int) {
    companion object {
        fun file(name: String, parent: Entry, size: Int): Entry {
            return Entry(name, EntryType.FILE, parent, null, size)
        }

        fun folder(name: String, parent: Entry?): Entry {
            return Entry(name, EntryType.FOLDER, parent, arrayListOf(), 0)
        }
    }
}

private class FileSystem(val root: Entry) {
    private var currentDir = root

    fun findFolders(base: Entry = root, fn: (Entry) -> Boolean): List<Entry> {
        val folders: ArrayList<Entry> = arrayListOf()

        if (fn(base)) {
            folders.add(base)
        }

        base.children!!.forEach { children ->
            if (children.type == EntryType.FOLDER) {
                folders.addAll(findFolders(children, fn))
            }
        }

        return folders
    }

    private fun addChild(entry: Entry) {
        currentDir.children!!.add(entry)

        var base: Entry? = currentDir
        while (base != null) {
            base.size += entry.size
            base = base.parent
        }
    }

    private fun addFile(name: String, size: Int) {
        addChild(Entry.file(name, currentDir, size))
    }

    private fun addFolder(name: String) {
        addChild(Entry.folder(name, currentDir))
    }

    private fun cd(path: String) {
        currentDir = if (path == "..") {
            currentDir.parent ?: throw RuntimeException("Can't 'cd ..' on root folder")
        } else {
            currentDir.children!!.find { it.name == path } ?: throw RuntimeException("Can't find folder")
        }
    }

    companion object {
        fun resolve(input: String): FileSystem {
            var readingDir = false

            val root = Entry("/", EntryType.FOLDER, null, arrayListOf(), 0)
            val fs = FileSystem(root)

            for ((i, line) in input.lines().withIndex()) {
                if (i == 0) {
                    continue
                }

                if (line.startsWith("$ cd")) {
                    val folderName = line.removePrefix("$ cd ")

                    fs.cd(folderName)

                    readingDir = false
                    continue
                }

                if (readingDir) {
                    if (line.startsWith("dir")) {
                        val folderName = line.removePrefix("dir ")

                        fs.addFolder(folderName)
                    } else {
                        val (size, fileName) = line.split(" ")
                        fs.addFile(fileName, size.toInt())
                    }

                    continue
                }

                if (line.startsWith("$ ls")) {
                    readingDir = true
                    continue
                }
            }

            return fs
        }
    }
}
