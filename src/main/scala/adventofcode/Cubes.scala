package adventofcode

object Cubes extends App with ProgramWithSource {

  type Cube[T] = Array[Array[Array[T]]]

  override def filename: String = "test.txt"

  val cube: Cube[Int] = ???

  def cycle(cube: Cube[Int]): Cube[Int] = {
    ???
  }

  cycle(
    cycle(
      cycle(
        cycle(
          cycle(
            cycle(
              cube
            )
          )
        )
      )
    )
  )
}
