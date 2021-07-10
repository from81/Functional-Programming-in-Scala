package scalashop

import java.util.concurrent.*
import scala.util.DynamicVariable

import org.scalameter.*

/** The value of every pixel is represented as a 32 bit integer. */
type RGBA = Int

/** Returns the red component. */
def red(c: RGBA): Int = (0xff000000 & c) >>> 24

/** Returns the green component. */
def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

/** Returns the blue component. */
def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

/** Returns the alpha component. */
def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

/** Used to create an RGBA value from separate components. */
def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
  (r << 24) | (g << 16) | (b << 8) | (a << 0)

/** Restricts the integer into the specified range. */
def clamp(v: Int, min: Int, max: Int): Int =
  if v < min then min
  else if v > max then max
  else v

/** Image is a two-dimensional matrix of pixel values. */
class Img(val width: Int, val height: Int, private val data: Array[RGBA]):
  def this(w: Int, h: Int) = this(w, h, new Array(w * h)) // when constructed without passing data parameter
  def apply(x: Int, y: Int): RGBA = data(y * width + x) // return pixel at img[x][y] (*data is 1 dimensional)
  def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  override def toString: String = (0 until width).map(x => this.apply(x, 0)).toString()

/** Computes the blurred RGBA value of a single pixel of the input image. */
def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
  // TODO implement using while loops
  def extractChannel(f: RGBA => Int): IndexedSeq[Int] =
    for {
      i <- y-radius to y+radius
      j <- x-radius to x+radius
      if i >= 0 && i < src.height
      if j >= 0 && j < src.width
    } yield clamp(f(src(j, i)), 0, 255)
    
  def getAverage(s: IndexedSeq[Int]): Int = s.sum / s.length
  
  rgba(
    getAverage(extractChannel(red)),
    getAverage(extractChannel(green)),
    getAverage(extractChannel(blue)),
    getAverage(extractChannel(alpha))
  )
}

val forkJoinPool = ForkJoinPool()

abstract class TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())

class DefaultTaskScheduler extends TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T] =
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    t

val scheduler =
  DynamicVariable[TaskScheduler](DefaultTaskScheduler())

def task[T](body: => T): ForkJoinTask[T] =
  scheduler.value.schedule(body)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
  scheduler.value.parallel(taskA, taskB)

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) =
  val ta = task { taskA }
  val tb = task { taskB }
  val tc = task { taskC }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)
