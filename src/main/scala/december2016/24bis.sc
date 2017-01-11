List().head

val a = Seq(
  Some(Seq()),
  Some(Seq(1,2)),
  Some(Seq(3)),
  None
).flatMap(s=>s.head)