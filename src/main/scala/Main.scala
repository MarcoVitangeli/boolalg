import scala.util.Failure
import scala.util.Success

def generate_truth_table(): Unit = {
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
  
}

def generate_truth_table_negate(): Unit = {
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

@main 
def hello(): Any =
  generate_truth_table_negate()
  
  