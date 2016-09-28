package quickcheck

import org.scalacheck._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  import org.scalacheck.Arbitrary._
  import org.scalacheck.Gen._
  import org.scalacheck.Prop.forAll

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    w <- arbitrary[Int]
    h1 <- oneOf(const(empty), genHeap)
  } yield insert(w, insert(v, h1))


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("gen1") = forAll { (h: H) =>

    val m = if (isEmpty(h)) 0 else findMin(h)

    findMin(insert(m, h)) == m
  }


  property("ordered delete") = forAll { heap: H =>
    deleteOrdered(Int.MinValue, heap)
  }


  @tailrec
  private def deleteOrdered(min: Int, heap: H): Boolean = {

    if (isEmpty(heap)) {
      true
    } else {

      val hMin = findMin(heap)

      if (min > hMin) {
        false
      } else {
        deleteOrdered(hMin, deleteMin(heap))
      }

    }

  }

  def insertList(listInt: List[Int], heap: H): H = listInt match {
    case x :: xs =>
      insertList(xs, insert(x, heap))
    case Nil =>
      heap
  }


  property("ordered insert delete") = forAll { listInt: List[Int] =>
    val heap = insertList(listInt, empty)

    checkDelSequence(heap, Int.MinValue)
  }


  property("remove min, insert min") = forAll { h: H =>
    val f1Min: Int = findMin(h)
    val hMin: H = deleteMin(h)
    val backH: H = insert(f1Min, hMin)
    val f2Min = findMin(backH)
    f1Min == f2Min
  }


  def checkDelSequence(heap: H, min: Int): Boolean = {
    if (isEmpty(heap)) {
      true
    } else {
      val hMin = findMin(heap)
      if (hMin < min) {
        // shouldn't do!!
        false
      } else {
        checkDelSequence(deleteMin(heap), hMin) // ok! check next
      }
    }
  }


  property("delete min Sequence List") = forAll { listInt: List[Int] =>
    val heap = insertList(listInt, empty)

    checkDelSequence(heap, Int.MinValue)
  }


  property("delete min Sequence") = forAll { heap: H =>
    checkDelSequence(heap, Int.MinValue)
  }




  property("Min of Meld") = forAll { h: H =>

    //heap constant
    val fixedHeap: H = insert(-8, insert(1, empty))

    //original mins
    val fMin: Int = findMin(h)
    val rMin: Int = findMin(fixedHeap)

    //min of meld
    val mMin: Int = findMin(meld(h, fixedHeap))

    //min of mins
    val minOfMins: Int = findMin(insert(rMin, insert(fMin, empty)))

    minOfMins == mMin

  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("check min back") = forAll { a: Int =>
    val h = insert(a, empty)

    val shouldEmpty = deleteMin(h)

    isEmpty(shouldEmpty)
  }

  property("min AB list") = forAll { lInt: List[Int] =>
    val heap = insertList(lInt, empty)
    isEmpty(heap) || (findMin(heap) == lInt.min)
  }


  property("min AB list") = forAll { lInt: List[Int] =>
    val setInt: Set[Int] = lInt.toSet
    val heap = insertList(lInt.toSet.toList, empty)
    checkMinDel(setInt, heap)
  }


  def checkMinDel(sInt: Set[Int], h: H): Boolean = {
    if (isEmpty(h)) {
      sInt.isEmpty
    } else {
      val hMin = findMin(h)
      val sMin = sInt.min

      if (hMin == sMin) {
        checkMinDel(sInt - hMin, deleteMin(h))
      } else {
        false
      }
    }

  }


  property("min AB meld") = forAll { ab: (Int, Int) =>
    val (a, b) = ab
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    val minAB = findMin(meld(h1, h2))
    Math.min(a, b) == minAB
  }


  property("min AB insert") = forAll { ab: (Int, Int) =>
    val (a, b) = ab
    val h = insert(b, insert(a, empty))
    val minAB = findMin(h)
    Math.min(a, b) == minAB
  }

  property("Insert And Min") = forAll { h: H =>

    //heap constant
    val fixedHeap: H = insert(-8, insert(1, empty))

    //original mins
    val fMin: Int = findMin(h)
    val rMin: Int = findMin(fixedHeap)

    //min of meld
    val mMin: Int = findMin(meld(h, fixedHeap))

    //min of mins
    val minOfMins: Int = findMin(insert(rMin, insert(fMin, empty)))

    minOfMins == mMin

  }

}


