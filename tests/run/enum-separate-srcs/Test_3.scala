@main def Test: Unit =
  val out = DataOut_2()
  val red = Colour.valueOf("Red")
  out.writeByte(red.ordinal)
