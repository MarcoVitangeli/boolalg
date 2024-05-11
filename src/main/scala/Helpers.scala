import scala.util.Failure
import scala.util.Success
import java.io.BufferedWriter
import java.io.FileWriter
import com.github.tototoshi.csv.CSVWriter
import com.github.tototoshi.csv.defaultCSVFormat
import java.io.File

object PrintHelper {
    def print_truth_table(s: String, cols: Array[String]): Unit = {
        val m = generateTruthTable(cols.length)

        println("COLS:\n")
        cols.foreach(println)
        println()

        for (comb <- m) {
            val params = cols.zip(comb).toMap
            val res = SimpleParser.parse(s, params)

            res match
                case Failure(x) => throw RuntimeException("error parsing proposition", x)
                case Success(value) =>
                    val rowStr = comb
                        .appended(value)
                        .map{_.toString()}
                        .mkString("\t")

                    println("|" + rowStr + "   |")
        }
    }

    def truth_table_csv (s: String, cols: Array[String]): Unit = {
        val f = new File("Result.csv")
        val writter = CSVWriter.open(f)


        val m = generateTruthTable(cols.length)

        writter.writeRow(cols.appended(s))
        for (comb <- m) {
            val params = cols.zip(comb).toMap
            val res = SimpleParser.parse(s, params)

            res match
                case Failure(x) => throw RuntimeException("error parsing proposition", x)
                case Success(value) =>
                    val rowStr = comb
                        .appended(value)
                        .map{c => if (c) "T" else "F"}

                    writter.writeRow(rowStr)
        }
    }

    def generateTruthTable(N: Int): Seq[Seq[Boolean]] = {
        // Generate all possible combinations of truth values for N propositions
        val numCombinations = math.pow(2, N).toInt
        (0 until numCombinations).map { i =>
            (0 until N).map(j => ((i >> j) & 1) == 1)
        }
    }

    def generate_truth_table(): Unit =
        val m = Array(
            Array(true, true, true),
            Array(false, true, true),
            Array(true, false, true),
            Array(false, false, true),
            Array(true, true, false),
            Array(false, true, false),
            Array(true, false, false),
            Array(false, false, false),
        )

        val s = "(p v q) ^ (~p v r) v (p ^ q v (r ^ p))"

        m.foreach(r => {
            val res = SimpleParser.parse(s, Map[String, Boolean](
            ("p", r.array(0)),
            ("q", r.array(1)),
            ("r", r.array(2))
            ))
            printf("%b\t%b\t%b\t%b\n", r.array(0), r.array(1), r.array(2), res.get)
        })
  
    def generate_truth_table_negate(): Unit =
        val m = Array(
            Array(true, true, true),
            Array(false, true, true),
            Array(true, false, true),
            Array(false, false, true),
            Array(true, true, false),
            Array(false, true, false),
            Array(true, false, false),
            Array(false, false, false),
        )

        //val s = "~(p v q v ~r)"
        val s = "~(paulGraham v quo v ~rio)"

        m.foreach(r => {
            val res = SimpleParser.parse(s, Map[String, Boolean](
            ("paulGraham", r.array(0)),
            ("quo", r.array(1)),
            ("rio", r.array(2))
            ))
            printf("%b\t%b\t%b\t%b\n", r.array(0), r.array(1), r.array(2), res.get)
        })
}