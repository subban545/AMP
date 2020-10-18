import scala.io.StdIn.readLine
class Crossword(var matrix: Array[Array[Char]], val ws: Array[String], val dimension: Int) {

  private def isSolved(mtx: Array[Array[Char]]): Boolean = {
    if (mtx == null)
      return false

    for (i <- 0 until dimension)
      for (j <- 0 until dimension)
        if (mtx(i)(j) == '-')
          return false
    true
  }

  def findSlots(word: String, matrix: Array[Array[Char]]): Array[Slot] = {
    val size = word.length
    var slots: Array[Slot] = Array()

    /*
      Find potential slots in the rows of the matrix
     */
    for (i <- 0 until dimension) {
      val row = matrix(i)
      var start = -1
      var end = dimension
      for (j <- 0 until dimension) {
        if (row(j) != '+' && start == -1)
          start = j
        else if ((row(j) == '+' || j == dimension - 1) && start != -1) {
          if (row(j) == '+')
            end = j
          else
            end = j + 1
          if (size == end - start)
            slots = slots :+ new Slot(start, end - 1, false, i)
          start = -1
        }
      }
    }

    /*
      Find potential slots in the columns of the matrix
     */
    for (i <- 0 until dimension) {
      var start = -1
      var end = dimension
      for (j <- 0 until dimension) {
        if (matrix(j)(i) != '+' && start == -1)
          start = j
        else if ((matrix(j)(i) == '+' || j == dimension - 1) && start != -1) {
          end = j
          if (matrix(j)(i) == '+')
            end = j
          else
            end = j + 1
          if (size == end - start)
            slots = slots :+ new Slot(start, end - 1, true, i)
          start = -1
        }
      }
    }
    slots
  }

  def fillSlot(word: String, pos: Slot, matrix: Array[Array[Char]]): Array[Array[Char]] = {
    if (pos.isVertical) {
      for (j <- pos.start to pos.end)
        if (matrix(j)(pos.columnOrRow) == '-' || word.charAt(j - pos.start) == matrix(j)(pos.columnOrRow))
          matrix(j)(pos.columnOrRow) = word.charAt(j - pos.start)
        else
        // the word cannot be placed in this slot
          return null
    } else {
      for (j <- pos.start to pos.end)
        if (matrix(pos.columnOrRow)(j) == '-' || word.charAt(j - pos.start) == matrix(pos.columnOrRow)(j))
          matrix(pos.columnOrRow)(j) = word.charAt(j - pos.start)
        else
        // the word cannot be placed in this slot
          return null
    }
    matrix
  }

  def solve(matrix: Array[Array[Char]], words: Array[String]): Array[Array[Char]] = {

    if (isSolved(matrix)) {
      this.matrix = matrix
      return matrix
    }

    if (words.length == 0)
      return null

    for (word <- words)
      for (pos <- findSlots(word, matrix)) {
        // Put word in the slot and remove it from the list of words
        var clonedMtx = matrix.map(_.clone)
        clonedMtx = fillSlot(word, pos, clonedMtx)
        if (clonedMtx != null) {
          val restOfWords = words.filter(_ != word)
          val mtx = solve(clonedMtx, restOfWords)
          if (isSolved(mtx))
            return mtx
        }
      }
    null
  }

  def printCrossword(): Unit = {
    for (i <- 0 until dimension) {
      for (j <- 0 until dimension)
        print(this.matrix(i)(j))
      println()
    }
  }

}

class Slot(val start: Int, val end: Int, val vertical: Boolean, val colmOrRow: Int) {
  var isVertical: Boolean = vertical
  var columnOrRow: Int = colmOrRow
  var startIndex: Int = start
  var endIndex: Int = end
}

object Solution {
  def main(args: Array[String]) {

    println("Enter Crossword Grid line by line:")
    var matrix = Array.ofDim[Char](10, 10)
    for (i <-0 until 10) {
      val line = readLine().trim
      matrix(i) = line.toCharArray
    }

    println("Enter the list of words separated by ';': (e.g., LONDON;DELHI;ICELAND;ANKARA)")
    val line = readLine().trim
    val words = line.split(";")
    words.foreach(println)

    val cw: Crossword = new Crossword(matrix, words, 10)
    println("___________ Crossword ___________")
    cw.printCrossword()
    cw.solve(cw.matrix, words)
    println("___________ Answer ___________")
    cw.printCrossword()
  }
}
