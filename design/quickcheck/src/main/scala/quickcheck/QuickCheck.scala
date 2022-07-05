package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import java.{util => ju}
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(
        const(insert(v, empty)),
        for {
          h1 <- genHeap
          h2 <- genHeap
        } yield meld(h1, h2)
     )
    } yield h
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: Int) => 
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min3") = forAll { (a: Int, b: Int) =>
    val m = if a < b then a else b
    findMin(insert(a, insert(b, empty))) == m
  }

  property("delmin") = forAll { (a: Int) => {
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }}

  property("minofmelding") = forAll { (h1: H, h2: H) =>
    val mins = for h <- List(h1, h2) yield findMin(h)
    val h = meld(h1, h2)
    mins.min == findMin(h)
  }

  def toSorted(h: H): List[A] =
    if isEmpty(h)
    then Nil
    else
      val min = findMin(h)
      min :: toSorted(deleteMin(h))

  property("sortedmins") = forAll { (h: H) =>
    
    def isSorted(l: List[A]): Boolean = 
      def iter(prev: A, l: List[A]): Boolean = l match
        case Nil => true
        case curr :: tail => curr >= prev && iter(curr, tail)
      
      l match
        case Nil => true
        case curr :: tail => iter(curr, tail)

    (toSorted andThen isSorted)(h)
  }

  def buildHeap(l: List[A]): H =
    def iter(heap: H, l: List[A]): H =  l match
      case head :: tail => iter(insert(head, heap), tail)
      case Nil => heap
    iter(empty, l)

  property("rightsize") = forAll { (l: List[A]) => 
    toSorted(buildHeap(l)).length == l.length
  }

  property("melindsize") = forAll { (l1: List[A], l2: List[A]) =>
    toSorted(
      meld(buildHeap(l1), buildHeap(l2))
    ).size == l1.size + l2.size
  }

  property("addminandmeld") = forAll { (h1: H, h2: H, n: A) =>
    val m = List(findMin(h1), findMin(h2), n).min
    findMin(meld(h1, insert(n, h2))) == m
  }

  property("delminmeldputback") = forAll { (h: H) => {
    try
      val m = findMin(h)
      findMin(meld(h, insert(m, empty))) == m
    catch
      case ex: ju.NoSuchElementException => true
  }}

  // def compareHeaps(h1: H, h2: H): Boolean =
  //   if isEmpty(h1) || isEmpty(h2) then isEmpty(h1) && isEmpty(h2)
  //   else 
  //     val m1 = findMin(h1)
  //     val m2 = findMin(h2)
  //     val res = m1 == m2 && compareHeaps(deleteMin(h1), deleteMin(h2))
    
  //     res

  // property("stolen") = forAll { (h1: H, h2: H) =>
  //   val res = compareHeaps(h1, h1)

  //   res
  //   // val m1 = findMin(h1)
  //   // val m2 = findMin(h2)

  //   // compareHeaps(meld(insert(m2, h1), deleteMin(h2)), meld(insert(m1, h2), deleteMin(h1)))
  // }