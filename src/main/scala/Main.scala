@main 
def hello(): Any =
  //(m∧∼p)→r
  PrintHelper.truth_table_csv("(m ^ ~p) & r", Array("m","p", "r"))
  //PrintHelper.truth_table_csv("~(one v two) ^ ~three v (four ^ ~six)", Array("one", "two", "three", "four", "six"))
  //PrintHelper.print_truth_table("~(one v two) ^ ~three v (four ^ ~six)", Array("one", "two", "three", "four", "six"))