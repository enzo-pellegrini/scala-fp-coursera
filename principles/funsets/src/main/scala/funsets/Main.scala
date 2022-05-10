package funsets

object Main extends App:
  import FunSets.*
  println(contains(singletonSet(1), 1))
  val s = fromList(List(1, 2, 3, 60, 70, 80, 100, -100, -200, -300))
  printSet(s)
  val bs = map(s, x => 2*x)
  printSet(bs)
  // val xs = for i <- (-1000 to 1000) if contains(s, i) yield i
  val bigSet = fromList(List.range(-1000, 1000))
  // printSet(bigSet)
  val mapped = map(bigSet, x => x/2)
  printSet(mapped)
  println(contains(mapped, -501))
  println(contains(mapped, -500))

  println("hello")
