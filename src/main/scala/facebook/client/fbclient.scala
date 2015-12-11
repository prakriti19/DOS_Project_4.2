package facebook.client

import java.util.concurrent.ConcurrentHashMap

import akka.actor._
import spray.client.pipelining
import spray.client.pipelining._
import spray.http._
import facebook.{client, SampleTrait, RestInterface}
import spray.routing.HttpService
import scala.util.Random
import spray.json._
import scala.concurrent.duration._
/**
  * Created by Pratyoush on 29-11-2015.
  */
object fbclient extends App{
  implicit val system = ActorSystem("Main")
  val master = system.actorOf(Props[Master], "Master")
  master ! "create"
}

class Master extends Actor with SampleTrait{
  implicit val sys = context.system
  override implicit def actorRefFactory: ActorRefFactory = sys
  val pipeline = sendReceive
    def receive = {
      case "create" => println("Inside Master")
        for(i<- 0 to 2400){
          val friendlist = friendMap.get(i).friendList
          val hashMap = pageMap
          Master.this.FbPage
          val child = context.system.actorOf(Props(new ClientUser(pipeline,i, friendlist)), "activeuser"+i.toString)
          child ! "high"
        }
       for(i<- 2401 to 4500 ){
          val friendlist = friendMap.get(i).friendList
          val child = context.system.actorOf(Props(new ClientUser(pipeline,i, friendlist)), "activeuser"+i.toString)
          child ! "medium"
        }
        for(i<- 4501 to 4999){
          val friendlist = friendMap.get(i).friendList
          val child = context.system.actorOf(Props(new ClientUser(pipeline,i, friendlist)), "activeuser"+i.toString)
          child ! "low"
        }
    }
  }

class ClientUser(pipeline: pipelining.SendReceive,userid: Int, friendlist: List[Int]) extends Actor{

  //override implicit def executionContext = actorRefFactory.dispatcher
  println(self.path.name)

import scala.concurrent.ExecutionContext.Implicits.global


    def receive = {
      case "high" => {
        println(friendlist)
       /*sys.scheduler.schedule(0 seconds, 3 seconds) ({val random = new Random()
        val r = random.nextInt(friendlist.size-1)
        println(friendlist(r))})*/

        context.system.scheduler.schedule(0 seconds, 30 seconds) ({var result = pipeline(Get("http://localhost:8080/"+userid+"/profile"))
          result.foreach { response =>
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }
          result = pipeline(Get("http://localhost:8080/"+userid+"/wallposts"))
          result.foreach { response =>
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }})

        context.system.scheduler.schedule(0 seconds, 60 seconds) ({val random = new Random()
          val r = random.nextInt(friendlist.size-1)
          var result = pipeline(Get("http://localhost:8080/"+friendlist(r).toString +"/profile"))
          result.foreach { response =>
            println(s"Friends Profile!!!! Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }

          result = Get("http://localhost:8080/" + userid.toString + "/wallposts/" + friendlist(r).toString + "/addpost?post=Hello" + System.currentTimeMillis()) ~>sendReceive
          result.foreach { response =>
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }})
        /*context.system.scheduler.schedule(0 seconds, 200 seconds) ({val random = new Random()
          val r = random.nextInt(pageMap.size-1)
          var result = Get("http://localhost:8080/"+r.toString +"/page") ~> sendReceive
          result.foreach { response =>
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }

          result = Get("http://localhost:8080/" + userid.toString + "/page/addpost/" + userid + "?post=HelloPage" + System.currentTimeMillis()) ~>sendReceive
          result.foreach { response =>
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }})*/
      }
      case "low" => {
        context.system.scheduler.schedule(0 seconds, 200 seconds) ({var result = Get("http://localhost:8080/"+userid+"/profile") ~> sendReceive
          result.foreach { response =>
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }
      })
        context.system.scheduler.schedule(0 seconds, 400 seconds) ({val random = new Random()
          val r = random.nextInt(friendlist.size-1)
          var result = Get("http://localhost:8080/"+friendlist(r).toString +"/profile") ~> sendReceive
          result.foreach { response =>
            println(s"Friends Profile!!!! Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }})
          }

      case "medium" => {
        context.system.scheduler.schedule(0 seconds, 90 seconds) ({var result = Get("http://localhost:8080/"+userid+"/profile") ~> sendReceive
          result.foreach { response =>
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }
          result = Get("http://localhost:8080/"+userid+"/wallposts") ~> sendReceive
          result.foreach { response =>
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }})

        context.system.scheduler.schedule(0 seconds, 150 seconds) ({val random = new Random()
          val r = random.nextInt(friendlist.size-1)
          var result = Get("http://localhost:8080/"+friendlist(r).toString +"/profile") ~> sendReceive
          result.foreach { response =>
            println(s"Friends Profile!!!! Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }

          result = Get("http://localhost:8080/" + userid.toString + "/wallposts/" + friendlist(r).toString + "/addpost?post=Hello" + System.currentTimeMillis()) ~>sendReceive
          result.foreach { response =>
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }})
      }
    }
}
