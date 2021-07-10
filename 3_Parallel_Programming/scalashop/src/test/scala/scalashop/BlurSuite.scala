package scalashop

import java.util.concurrent.*
import scala.collection.*

class BlurSuite extends munit.FunSuite:
  // Put tests here
  //  import common._
  trait TestSets:
    val img1 = Img(4, 3,
      Array(
        rgba(1, 0, 0, 0),
        rgba(2, 0, 0, 0),
        rgba(3, 0, 0, 0),
        rgba(4, 0, 0, 0),

        rgba(4, 0, 0, 0),
        rgba(5, 0, 0, 0),
        rgba(6, 0, 0, 0),
        rgba(7, 0, 0, 0),

        rgba(7, 0, 0, 0),
        rgba(8, 0, 0, 0),
        rgba(9, 0, 0, 0),
        rgba(10, 0, 0, 0)
      )
    )

    val img2 = Img(6, 3,
      Array(
        rgba(68, 202, 245, 106),
        rgba(68, 32, 105, 160),
        rgba(224, 71, 30, 55),
        rgba(190, 148, 36, 225),
        rgba(210, 23, 74, 206),
        rgba(20, 37, 188, 250),

        rgba(79, 98, 202, 8),
        rgba(132, 234, 83, 43),
        rgba(224, 214, 201, 226),
        rgba(103, 234, 230, 120),
        rgba(69, 173, 234, 114),
        rgba(34, 138, 33, 94),

        rgba(108, 90, 191, 2),
        rgba(34, 181, 155, 254),
        rgba(30, 157, 73, 80),
        rgba(101, 82, 120, 254),
        rgba(89, 188, 57, 128),
        rgba(176, 69, 96, 202)
      )
    )

  test("boxBlur 1") {
    new TestSets:
      assertEquals(red(boxBlurKernel(img1, 2, 1, 1)), 6)
  }

  test("boxBlur 2") {
    new TestSets:
      assertEquals(red(boxBlurKernel(img1, 2, 2, 1)), 7)
  }

  test("boxBlur 3") {
    new TestSets:
      assertEquals(red(boxBlurKernel(img1, 3, 2, 1)), 8)
  }

  test("VerticalBoxBlur 1") {
    new TestSets:
      val res = Img(img1.width, img1.height)
      VerticalBoxBlur.blur(img1, res, 0, 2, 1)

      assertEquals(red(res(0, 0)), 3)
      assertEquals(red(res(0, res.height - 1)), 6)
      assertEquals(red(res(1, 1)), 5)
  }

  test("VerticalBoxBlur.parBlur 1") {
    new TestSets:
      val res = Img(img2.width, img2.height)
      VerticalBoxBlur.parBlur(img2, res, 3, 1)

      assertEquals(red(res(0, 0)), 86)
      assertEquals(red(res(1, 0)), 132)
      assertEquals(red(res(0, 1)), 81)
      assertEquals(red(res(1, 1)), 107)
      assertEquals(red(res(0, 2)), 88)
      assertEquals(red(res(1, 2)), 101)
      assertEquals(red(res(5, 1)), 99)
  }

  test("VerticalBoxBlur.parBlur 2") {
    new TestSets:
      val res = Img(img2.width, img2.height)
      VerticalBoxBlur.parBlur(img2, res, 6, 1)

      assertEquals(red(res(0, 0)), 86)
      assertEquals(red(res(1, 0)), 132)
      assertEquals(red(res(0, 1)), 81)
      assertEquals(red(res(1, 1)), 107)
      assertEquals(red(res(0, 2)), 88)
      assertEquals(red(res(1, 2)), 101)
      assertEquals(red(res(5, 1)), 99)
  }

  test("HorizontalBoxBlur 1") {
    new TestSets:
      val res = Img(img1.width, img1.height)
      HorizontalBoxBlur.blur(img1, res, 0, 2, 1)

      assertEquals(red(res(0, 0)), 3)
      assertEquals(red(res(1, 1)), 5)
  }

  test("HorizontalBoxBlur.parBlur 1") {
    new TestSets:
      val res = Img(img2.width, img2.height)
      HorizontalBoxBlur.parBlur(img2, res, 3, 1)

      assertEquals(red(res(0, 0)), 86)
      assertEquals(red(res(1, 0)), 132)
      assertEquals(red(res(0, 1)), 81)
      assertEquals(red(res(1, 1)), 107)
      assertEquals(red(res(0, 2)), 88)
      assertEquals(red(res(1, 2)), 101)
      assertEquals(red(res(5, 1)), 99)
  }