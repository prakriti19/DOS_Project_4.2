package facebook

import akka.actor.{Actor, ActorSelection}
import spray.can.Http
import spray.can.Http.Register
import spray.http.{HttpEntity, HttpResponse, HttpRequest}

class RestInterface(server: Array[ActorSelection]) extends Actor with SampleTrait {
  //  implicit val system = ActorSystem()
  def actorRefFactory = context
  def receive = runRoute(sampleRoute)
  /*def receive:Actor.Receive = {
    case _: Http.Connected =>
      sender ! Register(self)
    case HttpRequest =>
      runRoute(sampleRoute)
     //sender ! HttpResponse(entity = HttpEntity("Hello WOrld"))
  }*/
}
