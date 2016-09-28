package scalashop

import common._
import org.scalameter._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blur(src: Img, dst: Img, from: Int, to: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method

    val fromCol = clamp(from, 0, src.width)
    val endCol  = clamp(to,   0, src.width)

    for (x <- fromCol until endCol) {
      for (y <- 0 until src.height) {

        val rgba = boxBlurKernel(src, x, y, radius)

        dst(x, y) = rgba
      }
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {

    val partSize = src.width / numTasks // size arredonadado

    val resto = src.width % numTasks    // resto ou zero

    val tasks = (0 until numTasks).map { partIndex => task {

      val from = partIndex * partSize
      val to = from + partSize

      blur(src, dst, from, to, radius)
    }
    }


    if (resto > 0) {
      task {

        val from = partSize * (numTasks - 1)
        //val from = partSize * numTasks
        val to = src.width
        blur(src, dst, from, to, radius)
      }
    }

    tasks.foreach(_.join())
  }

}


















