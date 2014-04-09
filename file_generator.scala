import Generator._
import java.io._

object FileGenerator {

  def fileList (names: Array[String]): Array[PrintWriter] =
  {
    (for (name <- names) yield
      new PrintWriter (new File(name))).toArray
  }

  def main (args: Array[String]) =
  {
    import DataGenerator.help

    if (!args.isEmpty && args(0) == "--help")
      println( help )
    else {
      try {
        args.indexOf("-f") match {
          case -1 =>
            val gen = DataGenerator(args)
            gen.writeRandData(print)
            print("\n")
          case n => 
            val (gArg,names) = args splitAt (n+1)
            if (names.isEmpty)
              println("Error, provide 1 or more fileNames after '-f'\n" + help)
            else {
              val outputList = fileList(names)
              val gen = DataGenerator(gArg)
              for (file <- outputList) {
                gen.writeRandData(file.write)
                file.close()
              }
            }
        }
      } catch {
        case e: Exception => println("There was an error !\n" + help)
      }
    }
  }
}
