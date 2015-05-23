package Thesis.Thesisbuild

import stapl.core._
import stapl.templates.general.GeneralTemplates
import stapl.performance.Timer
import stapl.core.pdp.PDP
import org.joda.time.LocalDateTime
import stapl.core.pdp.AttributeFinder
import stapl.core.pdp.RequestCtx
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import samples.TestExample

object Experiment {
  
  import ExperimentPolicy._
  import TestExample._
  import java.io._
  
  val finder = new AttributeFinder
  finder += new MyAttributeFinderModule
  val pdp = new PDP(TestPol , finder)
  
  val converter = new TreeConverter(testPolcopy, Set.empty)
  converter.convertTree();
  val pdp2 = new PDP(converter.root, finder)
  
  def main(args: Array[String]) {
  val tNormal = new Timer()
  val tConvert = new Timer()
  
  var l1,l2,l3,l4,l5,l6,l7,l8,l9,l10=0
  var lnormal, lconvert: List[Double] = List.empty
  var devnormal, devconvert: List[Double] = List.empty
  
  //Measuring number of accessed attributes before conversion
    l1 = test1(pdp).employedAttributes.size
    l2 = test2(pdp).employedAttributes.size
    l3 = test3(pdp).employedAttributes.size
    l4 = test4(pdp).employedAttributes.size
    l5 = test5(pdp).employedAttributes.size
    l6 = test6(pdp).employedAttributes.size
    l7 = test7(pdp).employedAttributes.size
    l8 = test8(pdp).employedAttributes.size
    l9 = test9(pdp).employedAttributes.size
   l10 = test10(pdp).employedAttributes.size

  //Measuring evaluation time before conversion
  for(i <- 1 to 100){
    tNormal.time(test1(pdp))
    tNormal.time(test2(pdp))
    tNormal.time(test3(pdp))
    tNormal.time(test4(pdp))
    tNormal.time(test5(pdp))
    tNormal.time(test6(pdp))
    tNormal.time(test7(pdp))
    tNormal.time(test8(pdp))
    tNormal.time(test9(pdp))
    tNormal.time(test10(pdp))
    lnormal ::= tNormal.mean
    devnormal ::= tNormal.stdDev
    tNormal.reset
  }
  
  //write results before conversion
  val file = new File("normalAccess.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write("test1 " + l1.toString + "\n")
  bw.write("test2 " + l2.toString + "\n")
  bw.write("test3 " + l3.toString + "\n")
  bw.write("test4 " + l4.toString + "\n")
  bw.write("test5 " + l5.toString + "\n")
  bw.write("test6 " + l6.toString + "\n")
  bw.write("test7 " + l7.toString + "\n")
  bw.write("test8 " + l8.toString + "\n")
  bw.write("test9 " + l9.toString + "\n")
  bw.write("test10 " + l10.toString + "\n")
  bw.close()
  
  val file1 = new File("normalTime.txt")
  val bw1 = new BufferedWriter(new FileWriter(file1))
  bw1.write("time standard deviation \n")
  for(t <- 1 to lnormal.size)
	  bw1.write(lnormal(t-1) + " " + devnormal(t-1) + "\n")
  bw1.close()
  
  //Measuring number of accessed attributes after conversion
    l1 = test1(pdp2).employedAttributes.size
    l2 = test2(pdp2).employedAttributes.size
    l3 = test3(pdp2).employedAttributes.size
    l4 = test4(pdp2).employedAttributes.size
    l5 = test5(pdp2).employedAttributes.size
    l6 = test6(pdp2).employedAttributes.size
    l7 = test7(pdp2).employedAttributes.size
    l8 = test8(pdp2).employedAttributes.size
    l9 = test9(pdp2).employedAttributes.size
   l10 = test10(pdp2).employedAttributes.size
   
    //Measuring evaluation time after conversion
  for(i <- 1 to 100){
    tConvert.time(test1(pdp2))
    tConvert.time(test2(pdp2))
    tConvert.time(test3(pdp2))
    tConvert.time(test4(pdp2))
    tConvert.time(test5(pdp2))
    tConvert.time(test6(pdp2))
    tConvert.time(test7(pdp2))
    tConvert.time(test8(pdp2))
    tConvert.time(test9(pdp2))
    tConvert.time(test10(pdp2))
    lconvert ::= tConvert.mean
    devconvert ::= tConvert.stdDev
    tConvert.reset
  }
  
   //write results before conversion
  val file2 = new File("convertAccess.txt")
  val bw2 = new BufferedWriter(new FileWriter(file2))
  bw2.write("test1 " + l1.toString + "\n")
  bw2.write("test2 " + l2.toString + "\n")
  bw2.write("test3 " + l3.toString + "\n")
  bw2.write("test4 " + l4.toString + "\n")
  bw2.write("test5 " + l5.toString + "\n")
  bw2.write("test6 " + l6.toString + "\n")
  bw2.write("test7 " + l7.toString + "\n")
  bw2.write("test8 " + l8.toString + "\n")
  bw2.write("test9 " + l9.toString + "\n")
  bw2.write("test10 " + l10.toString + "\n")
  bw2.close()
  
  val file3 = new File("convertTime.txt")
  val bw3 = new BufferedWriter(new FileWriter(file3))
  bw3.write("time standard deviation \n")
  for(t <- 1 to lconvert.size)
	  bw3.write(lconvert(t-1) + " " + devconvert(t-1) + "\n")
  bw3.close()
  
  val file4 = new File("accessTime.txt")
  val bw4 = new BufferedWriter(new FileWriter(file4))
  for(t <- 1 to lconvert.size)
	  bw4.write(lnormal(t-1) + " " + lconvert(t-1) + "\n")
  bw4.close()
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

object Experiment2 {
  
  import LittleReuse._
  
  val finder = new AttributeFinder
  finder += new FinderModule2
  val pdp = new PDP(tree2 , finder)
  
  val converter = new TreeConverter(tree2Copy, Set.empty)
  converter.convertTree();
  val pdp2 = new PDP(converter.root, finder)
  
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
  
  def main(args: Array[String]) {
  val tNormal = new Timer()
  val tConvert = new Timer()
  
  var l1,l2,l3,l4,l5,l6,l7,l8,l9,l10=0
  var lnormal, lconvert: List[Double] = List.empty
  var devnormal, devconvert: List[Double] = List.empty
  
  //Measuring number of accessed attributes before conversion
    l1 = test1(pdp).employedAttributes.size
    l2 = test2(pdp).employedAttributes.size
    l3 = test3(pdp).employedAttributes.size
    l4 = test4(pdp).employedAttributes.size
    l5 = test5(pdp).employedAttributes.size
    l6 = test6(pdp).employedAttributes.size
    l7 = test7(pdp).employedAttributes.size
    l8 = test8(pdp).employedAttributes.size
    l9 = test9(pdp).employedAttributes.size
   l10 = test10(pdp).employedAttributes.size

  //Measuring evaluation time before conversion
  for(i <- 1 to 100){
    tNormal.time(test1(pdp))
    tNormal.time(test2(pdp))
    tNormal.time(test3(pdp))
    tNormal.time(test4(pdp))
    tNormal.time(test5(pdp))
    tNormal.time(test6(pdp))
    tNormal.time(test7(pdp))
    tNormal.time(test8(pdp))
    tNormal.time(test9(pdp))
    tNormal.time(test10(pdp))
    lnormal ::= tNormal.mean
    devnormal ::= tNormal.stdDev
    tNormal.reset
  }
  
  //write results before conversion
  val file = new File("normalAccessTest2.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write("test1 " + l1.toString + "\n")
  bw.write("test2 " + l2.toString + "\n")
  bw.write("test3 " + l3.toString + "\n")
  bw.write("test4 " + l4.toString + "\n")
  bw.write("test5 " + l5.toString + "\n")
  bw.write("test6 " + l6.toString + "\n")
  bw.write("test7 " + l7.toString + "\n")
  bw.write("test8 " + l8.toString + "\n")
  bw.write("test9 " + l9.toString + "\n")
  bw.write("test10 " + l10.toString + "\n")
  bw.close()
  
  val file1 = new File("normalTimeTest2.txt")
  val bw1 = new BufferedWriter(new FileWriter(file1))
  bw1.write("time standard deviation \n")
  for(t <- 1 to lnormal.size)
	  bw1.write(lnormal(t-1) + " " + devnormal(t-1) + "\n")
  bw1.close()
  
  //Measuring number of accessed attributes after conversion
    l1 = test1(pdp2).employedAttributes.size
    l2 = test2(pdp2).employedAttributes.size
    l3 = test3(pdp2).employedAttributes.size
    l4 = test4(pdp2).employedAttributes.size
    l5 = test5(pdp2).employedAttributes.size
    l6 = test6(pdp2).employedAttributes.size
    l7 = test7(pdp2).employedAttributes.size
    l8 = test8(pdp2).employedAttributes.size
    l9 = test9(pdp2).employedAttributes.size
   l10 = test10(pdp2).employedAttributes.size
   
    //Measuring evaluation time after conversion
  for(i <- 1 to 100){
    tConvert.time(test1(pdp2))
    tConvert.time(test2(pdp2))
    tConvert.time(test3(pdp2))
    tConvert.time(test4(pdp2))
    tConvert.time(test5(pdp2))
    tConvert.time(test6(pdp2))
    tConvert.time(test7(pdp2))
    tConvert.time(test8(pdp2))
    tConvert.time(test9(pdp2))
    tConvert.time(test10(pdp2))
    lconvert ::= tConvert.mean
    devconvert ::= tConvert.stdDev
    tConvert.reset
  }
  
   //write results after conversion
  val file2 = new File("convertAccessTest2.txt")
  val bw2 = new BufferedWriter(new FileWriter(file2))
  bw2.write("test1 " + l1.toString + "\n")
  bw2.write("test2 " + l2.toString + "\n")
  bw2.write("test3 " + l3.toString + "\n")
  bw2.write("test4 " + l4.toString + "\n")
  bw2.write("test5 " + l5.toString + "\n")
  bw2.write("test6 " + l6.toString + "\n")
  bw2.write("test7 " + l7.toString + "\n")
  bw2.write("test8 " + l8.toString + "\n")
  bw2.write("test9 " + l9.toString + "\n")
  bw2.write("test10 " + l10.toString + "\n")
  bw2.close()
  
  val file3 = new File("convertTimeTest2.txt")
  val bw3 = new BufferedWriter(new FileWriter(file3))
  bw3.write("time standard deviation \n")
  for(t <- 1 to lconvert.size)
	  bw3.write(lconvert(t-1) + " " + devconvert(t-1) + "\n")
  bw3.close()
  
  val file4 = new File("accessTimeTest2.txt")
  val bw4 = new BufferedWriter(new FileWriter(file4))
  for(t <- 1 to lconvert.size)
	  bw4.write(lnormal(t-1) + " " + lconvert(t-1) + "\n")
  bw4.close()
  }
}

object Experiment3 {
  
  import suchReuse._
  
  val finder = new AttributeFinder
  finder += new FinderModule3
  val pdp = new PDP(tree3 , finder)
  
  val converter = new TreeConverter(tree3copy, Set.empty)
  converter.convertTree();
  val pdp2 = new PDP(converter.root, finder)
  
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
  
  def main(args: Array[String]) {
  val tNormal = new Timer()
  val tConvert = new Timer()
  
  var l1,l2,l3,l4,l5,l6,l7,l8,l9,l10=0
  var lnormal, lconvert: List[Double] = List.empty
  var devnormal, devconvert: List[Double] = List.empty
  
  //Measuring number of accessed attributes before conversion
    l1 = test1(pdp).employedAttributes.size
    l2 = test2(pdp).employedAttributes.size
    l3 = test3(pdp).employedAttributes.size
    l4 = test4(pdp).employedAttributes.size
    l5 = test5(pdp).employedAttributes.size
    l6 = test6(pdp).employedAttributes.size
    l7 = test7(pdp).employedAttributes.size
    l8 = test8(pdp).employedAttributes.size
    l9 = test9(pdp).employedAttributes.size
   l10 = test10(pdp).employedAttributes.size

  //Measuring evaluation time before conversion
  for(i <- 1 to 100){
    tNormal.time(test1(pdp))
    tNormal.time(test2(pdp))
    tNormal.time(test3(pdp))
    tNormal.time(test4(pdp))
    tNormal.time(test5(pdp))
    tNormal.time(test6(pdp))
    tNormal.time(test7(pdp))
    tNormal.time(test8(pdp))
    tNormal.time(test9(pdp))
    tNormal.time(test10(pdp))
    lnormal ::= tNormal.mean
    devnormal ::= tNormal.stdDev
    tNormal.reset
  }
  
  //write results before conversion
  val file = new File("normalAccessTest3.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write("test1 " + l1.toString + "\n")
  bw.write("test2 " + l2.toString + "\n")
  bw.write("test3 " + l3.toString + "\n")
  bw.write("test4 " + l4.toString + "\n")
  bw.write("test5 " + l5.toString + "\n")
  bw.write("test6 " + l6.toString + "\n")
  bw.write("test7 " + l7.toString + "\n")
  bw.write("test8 " + l8.toString + "\n")
  bw.write("test9 " + l9.toString + "\n")
  bw.write("test10 " + l10.toString + "\n")
  bw.close()
  
  val file1 = new File("normalTimeTest3.txt")
  val bw1 = new BufferedWriter(new FileWriter(file1))
  bw1.write("time standard deviation \n")
  for(t <- 1 to lnormal.size)
	  bw1.write(lnormal(t-1) + " " + devnormal(t-1) + "\n")
  bw1.close()
  
  //Measuring number of accessed attributes after conversion
    l1 = test1(pdp2).employedAttributes.size
    l2 = test2(pdp2).employedAttributes.size
    l3 = test3(pdp2).employedAttributes.size
    l4 = test4(pdp2).employedAttributes.size
    l5 = test5(pdp2).employedAttributes.size
    l6 = test6(pdp2).employedAttributes.size
    l7 = test7(pdp2).employedAttributes.size
    l8 = test8(pdp2).employedAttributes.size
    l9 = test9(pdp2).employedAttributes.size
   l10 = test10(pdp2).employedAttributes.size
   
    //Measuring evaluation time after conversion
  for(i <- 1 to 100){
    tConvert.time(test1(pdp2))
    tConvert.time(test2(pdp2))
    tConvert.time(test3(pdp2))
    tConvert.time(test4(pdp2))
    tConvert.time(test5(pdp2))
    tConvert.time(test6(pdp2))
    tConvert.time(test7(pdp2))
    tConvert.time(test8(pdp2))
    tConvert.time(test9(pdp2))
    tConvert.time(test10(pdp2))
    lconvert ::= tConvert.mean
    devconvert ::= tConvert.stdDev
    tConvert.reset
  }
  
   //write results after conversion
  val file2 = new File("convertAccessTest3.txt")
  val bw2 = new BufferedWriter(new FileWriter(file2))
  bw2.write("test1 " + l1.toString + "\n")
  bw2.write("test2 " + l2.toString + "\n")
  bw2.write("test3 " + l3.toString + "\n")
  bw2.write("test4 " + l4.toString + "\n")
  bw2.write("test5 " + l5.toString + "\n")
  bw2.write("test6 " + l6.toString + "\n")
  bw2.write("test7 " + l7.toString + "\n")
  bw2.write("test8 " + l8.toString + "\n")
  bw2.write("test9 " + l9.toString + "\n")
  bw2.write("test10 " + l10.toString + "\n")
  bw2.close()
  
  val file3 = new File("convertTimeTest3.txt")
  val bw3 = new BufferedWriter(new FileWriter(file3))
  bw3.write("time standard deviation \n")
  for(t <- 1 to lconvert.size)
	  bw3.write(lconvert(t-1) + " " + devconvert(t-1) + "\n")
  bw3.close()
  
  val file4 = new File("accessTimeTest3.txt")
  val bw4 = new BufferedWriter(new FileWriter(file4))
  for(t <- 1 to lconvert.size)
	  bw4.write(lnormal(t-1) + " " + lconvert(t-1) + "\n")
  bw4.close()
  }
}