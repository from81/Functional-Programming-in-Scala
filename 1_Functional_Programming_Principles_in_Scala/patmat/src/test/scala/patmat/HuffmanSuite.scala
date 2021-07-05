package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }

  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a','b','d'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("character frequency count") {
    assertEquals(
      times(List('a', 'b', 'a')).sorted,
      List(('a', 2), ('b', 1))
    )
    assertEquals(
      times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')).sorted,
      List(('e',1), ('l',3), ('h',1), ('r',1), ('w',1), ('o',2), ('d',1)).sorted
    )
  }

  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("check singleton") {
    assertEquals(singleton(List()), false)
    assertEquals(singleton(List(Leaf('t', 2))), true)
    assertEquals(singleton(List(Leaf('t', 2), Leaf('q', 3))), false)
  }

  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine into one tree node") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(
      until(singleton, combine)(leaflist),
      List(
        Fork(
          Fork(
            Leaf('e',1),
            Leaf('t',2),
            List('e', 't'),
            3
          ),
          Leaf('x',4),
          List('e', 't', 'x'),
          7
        )
      )
    )
  }

  test("create code tree") {
    assertEquals(
      createCodeTree(List('e', 't', 'x', 't', 'x', 'x', 'x')),
      Fork(
        Fork(
          Leaf('e',1),
          Leaf('t',2),
          List('e', 't'),
          3
        ),
        Leaf('x',4),
        List('e', 't', 'x'),
        7
      )
    )
  }

  test("decode frenchCode") {
    assertEquals(
      decodedSecret,
      List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
    )
  }

  test("decode") {
    new TestTrees:
      assertEquals(decode(t1, List(0, 1)), List('a', 'b'))
  }

  test("encode") {
    new TestTrees:
      assertEquals(encode(t2)("abda".toList), List(0, 0, 0, 1, 1, 0, 0)),
  }

  test("encode 2") {
    new TestTrees:
      assertEquals(encode(t1)("ab".toList), List(0, 1)),
  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

  test("code bits") {
    new TestTrees:
      assertEquals(
        codeBits(
          List(
            ('a', List(1,0,0)),
            ('b', List(1,0,1)),
            ('c', List(1,1,1)),
            ('d', List(0,0,1)),
          )
        )('d'),
        List(0,0,1)
      )
  }

  test("convert") {
    new TestTrees:
      assertEquals(
        convert(t2),
        List(
          ('a', List(0,0)),
          ('b', List(0,1)),
          ('d', List(1))
        )
      )
  }

  test("quickEncode") {
    new TestTrees:
      assertEquals(quickEncode(t2)("abda".toList), List(0, 0, 0, 1, 1, 0, 0)),
  }

  test("quickEncode 2") {
    new TestTrees:
      assertEquals(quickEncode(t1)("ab".toList), List(0, 1)),
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
