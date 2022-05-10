package funsets

/** This class is a test suite for the methods in object FunSets.
  *
  * To run this test suite, start "sbt" then run the "test" command.
  */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /** When writing tests, one would often like to re-use certain values for
    * multiple tests. For instance, we would like to create an Int-set and have
    * multiple test about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we
    * can store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes?
    * Then the test methods are not even executed, because creating an instance
    * of the test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val a = union(s1, s2)
    val b = union(s2, s3)

  /** This test is currently disabled (by using @Ignore) because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", remove the .ignore
    * annotation.
    */
  test("singleton set one contains one") {

    /** We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets:
      /** The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains the common elements of two sets") {
    new TestSets:
      val s = intersect(a, b)
      assert(!contains(s, 1))
      assert(contains(s, 2))
      assert(!contains(s, 3))
  }

  test(
    "diff contains the elements of the first set that are not in the second"
  ) {
    new TestSets:
      val s = diff(a, b)
      assert(contains(s, 1))
      assert(!contains(s, 2))
  }

  test("filter") {
    new TestSets:
      val s = filter(a, x => x > 1)
      assert(!contains(s, 1))
      assert(contains(s, 2))
  }

  test("forall")(
    new TestSets:
      val f = (x: Int) => x < 3
      assert(forall(a, f))
      assert(!forall(b, f))
  )

  test("exists") {
    new TestSets:
      assert(exists(a, x => x == 2))
      assert(!exists(b, x => x == 1))
  }

  test("map") {
    new TestSets:
      val s = map(a, x => x + 10)
      assert(!contains(s, 1))
      assert(!contains(s, 3))
      assert(contains(s, 12))
  }

  test("fast") {
    val bigSet = fromList(List.range(-1000, 1000))
    val mapped = map(bigSet, x => x / 2)
    assert(!contains(mapped, -501))
    assert(contains(mapped, -500))
  }

  test("even and 3") {
    val set = union(n => n%2 == 0, singletonSet(3))
    assert(!forall(set, n => n%2 == 0))
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
