package example

import scala.io.StdIn

// Lancer l'exemple avec: sbt "runMain example.sayHello"
// TODO: supprimer le fichier
@main
def sayHello(): Unit = {
  println(":> Enter your name: ")

  val user = StdIn.readLine()

  println(s":> ${Hello.greeting} $user")

  ()
}

object Hello extends Greeting

trait Greeting {
  lazy val greeting: String = "hello"
}
