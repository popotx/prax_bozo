object Main extends App {
  case class Pos(y: Int, x: Int)
  case class Props(red: Int, green: Int, blue: Int, kokot: Option[Int])
  case class Pixel(pos: Pos, props: Props)

  val screen =
    """
      |0:100,125,56,200
      |1:255,0,0
      |2:0,135,200
      |3:220,12,0,100
      |4:45,97,0
      |5:0,0,0
      |6:0,0,0,0
      |7:0,0,0
      |8:255,255,255
    """.stripMargin

  val pixels = screen.split("\n").toList.flatMap { line =>
    val parts = line.split(":")
    if(parts.length>=2) {
      val color = parts(1).split(",")
      if(color.length>=3) Some(Pixel(Pos(parts(0).toInt / 3, parts(0).toInt % 3), Props(color(0).toInt, color(1).toInt, color(2).toInt, color.lift(3).map(_.toInt))))
      else None
    }
    else None
  }
  pixels.foreach(println)

  val red = pixels.groupBy((x) => x.pos.y).map { case (y, pixels) => (y, pixels.maxBy((x) => x.props.red)) }
  println("max red -> " + red)

  val kokot = pixels.groupBy((x) => x.pos.y).map { case (y, pixels) => (y, pixels.maxBy((x) => x.props.kokot)) }
  println("max kokot -> " + kokot)
}