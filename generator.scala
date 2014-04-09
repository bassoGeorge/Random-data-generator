/*
 The DataGenerator Factory
 and DataGenerator Class
 */
package Generator

trait DataGenerator  {
  def randWord: String          // To be overriden by the correct Data Trait
  def writeRandData(f: String => Unit): Unit // To Be overriden by the correct style trait

  override def toString = "DataGenerator"
}

private abstract class BasicGen (val args: Array[String]) extends DataGenerator {

  // randLine: Creates a single line of data. The procedure 'f' is then applied
  // to it. 'f' can be conviniently a output function or an accumulator
  protected def randLine(x: Int, f: String => Unit) {
    if (x == 0)
      return
    else {
      f(randWord + " ")
      randLine(x - 1, f)
    }
  }


  protected val randomGenerator = new util.Random() // random number generator
  
  // returns a random int in the inclusive range [lo, hi]
  protected def randInt(lo: Int, hi: Int): Int =
    randomGenerator.nextInt(hi - lo + 1) + lo

  
  // Parsing Routines
  // parse and return integer 'n' from the string "strn"
  // return NONE if not str not found
  protected def parseFind(str: String): Option[Int] =
  {
    (args find {_.startsWith(str)}) match {
      case Some(x) => Some(x.substring(str.length).toInt)
      case None    => None
    }
  }
  
  // parse and return the String "Res" from the string "strRes"
  // return NONE if not str not found
  protected def parseFindS(str: String): Option[String] =
  {
    (args find {_.startsWith(str)}) match {
      case Some(x) => Some(x.substring(str.length))
      case None => None
    }
  }

  // parse and return the Int 'n' right after the string 'str' in the args array
  // return NONE if not str not found
  protected def parseFindNext(str: String): Option[Int] =
  {
    args.indexOf(str) match {
      case -1 => None
      case v => Some(args(v + 1).toInt)
    }
  }
}

/* ---------------------=== Style Traits ===------------------------------ */
/////////////////////////////////////////////////////////////////////////////

// Default Style
private trait GeneralStyle extends BasicGen {

  def writeRandData(f: String => Unit) {
    val num = parseFindNext("-n") getOrElse 100
    parseFindNext("-l") match {
      case None => randLine(num, f)
      case Some(line) =>
        def makeLine(x: Int) {
          if (x == 0)
            return
          else if (x >= line) {
            randLine(line, f)
            f("\n")
            makeLine(x - line)
          }
          else
            randLine(x, f)
        }

        makeLine(num)
    }
  }

  override def toString = super.toString + " [format:General]"
}

// Secondary Style
private trait CodeJamStyle extends BasicGen {

  def writeRandData(f: String => Unit) {
    val num = parseFindNext("-n") getOrElse 100
    val line = parseFindNext("-l") getOrElse 50
    val least = parseFindNext("-m") match {
      case None => 1
      case Some(v) => if (v < 1) 1 else v
    }

    def makeRandLine(x: Int) {
      f(x + "\n")
      randLine(x,f)
      f("\n")
    }
    
    def makeLine(x: Int) {
      if (x == 0) return
      else {
        makeRandLine(randInt(least,line))
        makeLine(x - 1)
      }
    }

    f(num + "\n")
    makeLine(num)
  }
  override def toString = super.toString + " [format:CodeJam]"
}

/* ----------------------=== Data Traits ===------------------------------ */
/////////////////////////////////////////////////////////////////////////////

// Integer Data mode
private trait IntData extends BasicGen {
  private val max = parseFind("--max=") getOrElse 100
  private val min = parseFind("--min=") getOrElse 0

  def randWord =
    randInt(min, max).toString

  override def toString = super.toString + " [Data:Integer]"
}

// Double Data mode
private trait DoubleData extends BasicGen {
  private def parseFindD(str: String): Option[Double] =
  {
    (args find {_.startsWith(str)}) match {
      case Some(x) => Some(x.substring(str.length).toDouble)
      case None    => None
    }
  }

  private val max = parseFindD("--max=") getOrElse 1.0
  private val min = parseFindD("--min=") getOrElse 0.0
  private val prec = parseFind("--prec=") getOrElse 3

  def randWord =
  {
    require (prec > 0)
    require (max > min)
    import math.pow
    val factor = pow(10,prec)
    ((randInt( (min * factor).floor.toInt, (max * factor).floor.toInt)) / factor).toString
  }

  override def toString = super.toString + " [Data:Decimal]"
}


// Common trait for Character and String data mode
private trait BasicCharData extends BasicGen {
  private val numAllowed = args.exists {_=="--nums"}
  private val capsAllowed = args.exists {_=="--caps"}
  private val allCaps = args.exists {_=="--allcaps"}
  private val spcAllowed = args.exists {_=="--spchars"}
  private val dissalowAlpha = args.exists {_=="--noalphas"}

  private val spc = ((33 to 47).toList ::: (58 to 64).toList :::    // List of special Characters
      (91 to 96).toList ::: (123 to 126).toList) map {_.toChar}

  // Returns a List(<function>) which can then be used by randChar
  private def funcList: Array[() => Char] =
  {
    import scala.collection.mutable.ArrayBuffer
    val buf = new ArrayBuffer[() => Char]
    if (!allCaps && !dissalowAlpha)
      buf += (() => randInt('a', 'z').toChar)
    if (!dissalowAlpha && (capsAllowed || allCaps))
      buf += (() => randInt('A', 'Z').toChar)
    if (spcAllowed || (dissalowAlpha && !numAllowed))
      buf += (() => spc( randInt(0,spc.length - 1)))
    if (numAllowed)
      buf += (() => randInt('0', '9').toChar)
    buf.toArray
  }

  // Function list containing the correct functions as per requirement
  private val fList = funcList
  
  def randChar: Char = 
    fList( randInt(0, fList.length - 1)) ()
}

// Character Data Mode
private trait CharData extends BasicCharData {
  def randWord = randChar.toString
  override def toString = super.toString + " [Data:Character]"
}

// String Data mode
private trait StringData extends BasicCharData {
  def randWord = 
  {
    val max = parseFind("--max=") getOrElse 50
    val min = parseFind("--min=") getOrElse 2
    val fix = parseFind("--fix=") getOrElse 0

    val prefix = parseFindS("--prefix=") getOrElse ""
    val suffix = parseFindS("--suffix=") getOrElse ""

    import scala.collection.mutable.StringBuilder
    
    require(max > 0)
    require(min > 0)
    require(max > min)
    require(fix >= 0)

    val size =
      if (fix == 0)
        randInt(min, max)
      else
        fix
    val buf = new StringBuilder(size)
    buf append prefix

    for (i <- 1 to size)
      buf += randChar
    buf append suffix
    buf.toString
  }
  override def toString = super.toString + " [Data:String]"
}

// DataGenerator Factory
object DataGenerator {
  // Hard Coded Help String
  def help: String =
  """
  |Usage:
  |DataGenerator [-I | -D | -C | -S] [Options] [-f filename1 filename2.. ]

  |Data Modes:
  |-I : Integer
  |-D : Decimal, Double precision floating point data
  |-C : Character
  |-S : String

  |Style Option
  |--General : Default, simple syle, put words onto file
  |--CodeJam : Style following input data style of Google Code Jam,
  |           where first line contains # of lines, each line preceded
  |           by a line telling # of words on the line
  |
  |Major Options
  |-n : followed by the total # of words e.g. -n 40
  |-f : Should be the last option, followed by a list of output filenames
  |    e.g. -f data1.txt temp_data.txt
  |    (If -f flag not present, data will be outputted to terminal,
  |      moreover, each file will recieve a new randomly generated data
  |      sequence)
  |-l : # of words in a single line. In case of --CodeJam option, this
  |    will mean the MAX # of words on a line e.g. -l 3
  |-m : Available only in --CodeJam mode, Minimum # of words on a line
  |    e.g. -m 2

  |Data Specific Options
  |INTEGER:
  |--max=<n> : Upper bound of data, e.g. --max=32  [Default -> 100]
  |--min=<n> : Lower bound of data, e.g. --min=-10 [Default -> 0]

  |DECIMAL:
  |--max=<n> : Upper bound of data, e.g. --max=100.3 [Default -> 1.0]
  |--min=<n> : Lower bound of data                   [Default -> 0.0]
  |--prec=<n>: Precision, max # of digits after
  |            decimal point. e.g. --prec=5          [Default -> 3]

  |STRING:
  |--max=<n> : Max size of string (in characters) [Default -> 50]
  |--min=<n> : Min size of string                 [Default -> 2]
  |--prefix=<str>: Prefix to add to every word
  |--suffix=<str>: Suffix to add to every word

  |Common options to String and Char modes
  |--caps : Allow capitals
  |--allcaps : No smalls, overrides --caps
  |--nums : Allow Numerics
  |--noalphas: NO alphabets [Defaults to adding spchars if no other option is provided]
  |--spchars : Allow Special Characters """.stripMargin

  def apply(args: Array[String]): DataGenerator =
  {
    // Step 1: Decide the Style (General or CodeJam)
    val isCodeJam =
      args.exists {x => x.toLowerCase == "--codejam"}

    // Step 2: Decide the Data Mode (Int, Double, Char, String)

    // Enumeration of possible data modes
    object DataSystem extends Enumeration {
      type DataSystem = Value
      val IntD, CharD, StringD, DoubleD = Value
    }
    import DataSystem._

    // deciding data mode
    val data: DataSystem = 
      if (args.exists {_=="-I"})
        IntD
      else if (args.exists {_=="-D"})
        DoubleD
      else if (args.exists {_=="-S"})
        StringD
      else if (args.exists {_=="-C"})
        CharD
      else
        IntD

      // Create Custom Classes depending on the situation and return new object
    (isCodeJam, data) match {
      case (false, IntD)      => new BasicGen(args) with IntData    with GeneralStyle
      case (false, StringD)   => new BasicGen(args) with StringData with GeneralStyle
      case (false, CharD)     => new BasicGen(args) with CharData   with GeneralStyle
      case (false, DoubleD)   => new BasicGen(args) with DoubleData with GeneralStyle                             
      case (true, IntD)       => new BasicGen(args) with IntData    with CodeJamStyle                         
      case (true, StringD)    => new BasicGen(args) with StringData with CodeJamStyle                         
      case (true, CharD)      => new BasicGen(args) with CharData   with CodeJamStyle                         
      case (true, DoubleD)    => new BasicGen(args) with DoubleData with CodeJamStyle                         
    }
  }
}
