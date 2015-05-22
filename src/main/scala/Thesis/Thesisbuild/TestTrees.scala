package Thesis.Thesisbuild
import stapl.core.pdp._

object TestTrees {

  import LittleReuse._
  
  val finder = new AttributeFinder
  finder += new FinderModule2
  val pdp = new PDP(tree2 , finder)
  
  def main(args: Array[String]) {
  println("test1 " + test1(pdp).decision)
  println("test2 " + test2(pdp).decision)
  println("test3 " + test3(pdp).decision)
  println("test4 " + test4(pdp).decision)
  println("test5 " + test5(pdp).decision)
  println("test6 " + test6(pdp).decision)
  println("test7 " + test7(pdp).decision)
  println("test8 " + test8(pdp).decision)
  println("test9 " + test9(pdp).decision)
  println("test10 " + test10(pdp).decision)
}
  
  def test1(pdp : PDP) = pdp.evaluate("test1", "view", "doc123")
  
  def test2(pdp : PDP) = pdp.evaluate("test2", "view", "doc123")
  
  def test3(pdp : PDP) = pdp.evaluate("test3", "view", "doc123")
  
  def test4(pdp : PDP) = pdp.evaluate("test4", "view", "doc123")
  
  def test5(pdp : PDP) = pdp.evaluate("test5", "view", "doc123")
  
  def test6(pdp : PDP) = pdp.evaluate("test6", "view", "doc123")
  
  def test7(pdp : PDP) = pdp.evaluate("test7", "view", "doc123")
  
  def test8(pdp : PDP) = pdp.evaluate("test8", "view", "doc123")
  
  def test9(pdp : PDP) = pdp.evaluate("test9", "view", "doc123")
  
  def test10(pdp : PDP) = pdp.evaluate("test10", "view", "doc123")
}