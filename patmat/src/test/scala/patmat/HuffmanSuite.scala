package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('e',3),Leaf('t',2),List('e', 't'),5)))
  }

  test("simple encode") {
    new TestTrees{ 
    	assert(List(0, 0, 0, 1, 1, 0, 0) === encode(t2)("abda".toList))
    }
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("abda".toList)) === "abda".toList)
    }
  }
  
  test("decode secret") {
    new TestTrees {
        assert(decodedSecret === "huffmanestcool".toList)
    }
  }
  
  test("create code tree") {
    new TestTrees {
    	assert(t1 === createCodeTree("ababb".toList))
    }
  }
  
  test("convert code table") {
    new TestTrees {
    	val codeTable = convert(t1)
    	assert(List(('a', List(0)), ('b', List(1))) === codeTable)
    	
    	val codeTable1 = convert(t2)
    	assert(List(0, 0) === codeBits(codeTable1)('a'))
    	assert(List(0, 1) === codeBits(codeTable1)('b'))
    	assert(List(1) === codeBits(codeTable1)('d'))
    }
  }
  
  test("finally quick encode") {
    new TestTrees {
    	assert(secret === quickEncode(frenchCode)("huffmanestcool".toList))
    }
  }
}
