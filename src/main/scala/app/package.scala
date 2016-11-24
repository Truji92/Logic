

package object app {

  def timed[T](block: => T): (Long, T) = {
    val start = System.currentTimeMillis()
    val result = block
    val end = System.currentTimeMillis()
    (end-start, result)
  }

}
