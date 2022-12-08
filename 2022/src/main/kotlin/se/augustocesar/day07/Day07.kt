package se.augustocesar.day07

import se.augustocesar.Task

class Day07 : Task() {
    override fun partOne(): String {
        var readingDir: Entry? = null
        var currentDir = Entry("/", EntryType.FOLDER, null, arrayListOf(), 0)
        var rootDir = currentDir

        val lines = readInput().lines()
        for (i in 1 until lines.size) {
            val line = lines[i]

            if (line.startsWith("$ cd")) {
                val folderName = line.removePrefix("$ cd ")

                val children = if (folderName == "..") {
                    currentDir.parent ?: throw RuntimeException("Wut")
                } else {
                    currentDir.children.find { it.name == folderName } ?: throw RuntimeException("Wut")
                }

                currentDir = children
                readingDir = null
                continue
            }

            if (readingDir != null) {
                if (line.startsWith("dir")) {
                    val dirName = line.removePrefix("dir ")
                    currentDir.children.add(Entry(dirName, EntryType.FOLDER, currentDir, arrayListOf(), 0))
                } else {
                    val (size, fileName) = line.split(" ")
                    currentDir.children.add(Entry(fileName, EntryType.FILE, currentDir, arrayListOf(), size.toInt()))
                }

                continue
            }

            if (line.startsWith("$ ls")) {
                readingDir = currentDir
                continue
            }
        }

        rootDir.calculateSize()
        val res = lookupDirs(rootDir, 100_000)

        return res.sumOf { it.size }.toString()
    }

    override fun partTwo(): String {
        return "-"
    }
}

enum class EntryType { FILE, FOLDER }

class Entry(val name: String, val type: EntryType, val parent: Entry?, val children: ArrayList<Entry>, var size: Int) {
    fun calculateSize(): Int {
        if (this.type == EntryType.FILE) {
            return size
        }

        val sizes: List<Int> = this.children.map {
            if(it.type == EntryType.FOLDER) {
                it.calculateSize()
            } else {
                it.size
            }
        }

        this.size = sizes.sum()
        return this.size
    }
}

private fun lookupDirs(entry: Entry, maxSize: Int): List<Entry> {
    val folders: ArrayList<Entry> = arrayListOf()

    if(entry.size <= maxSize) {
        folders.add(entry)
    }

    entry.children.forEach {children ->
        if(children.type == EntryType.FOLDER) {
            folders.addAll(lookupDirs(children, maxSize))
        }
    }

    return folders
}
