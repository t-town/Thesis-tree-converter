package Thesis.Thesisbuild
import stapl.core.pdp._

object TestTrees {

  import stapl.core._
  import suchReuse._
  
  val finder = new AttributeFinder
  finder += new FinderModule3
  val pdp = new PDP(tree3 , finder)
  
  val converter = new TreeConverter(tree3copy)
  converter.convertTree();
  val pdp2 = new PDP(converter.root, finder)
  
  def main(args: Array[String]) {
  println("test1 " + test1(pdp2).decision)
  println("test2 " + test2(pdp2).decision)
  println("test3 " + test3(pdp2).decision)
  println("test4 " + test4(pdp2).decision)
  println("test5 " + test5(pdp2).decision)
  println("test6 " + test6(pdp2).decision)
  println("test7 " + test7(pdp2).decision)
  println("test8 " + test8(pdp2).decision)
  println("test9 " + test9(pdp2).decision)
  println("test10 " + test10(pdp2).decision)
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