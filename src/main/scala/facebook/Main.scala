package facebook

import akka.actor.{ActorSelection, Props, ActorSystem}
import akka.io.IO
import com.typesafe.config.ConfigFactory
import spray.can.Http
import spray.routing._


/**
  * Created by Pratyoush on 20-11-2015.
  */
object Main extends App{

  implicit val system = ActorSystem("Facebook")

  var server = new Array[ActorSelection](64)
  for(i<-0 until 64){
    server(i) = system.actorSelection("server"+i.toString)
  }

  val service = system.actorOf(Props(new RestInterface(server)), name = "service")
  IO(Http) ! Http.Bind(service, "localhost", port = 8080)
  //system.shutdown()
}



