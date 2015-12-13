package facebook.client

import java.io.File
import java.util.concurrent.ConcurrentHashMap

import akka.actor._
/*import facebook.SampleTrait.FacebookFriends
import facebook.SampleTrait.FbPage
import facebook.SampleTrait.FbPost
import facebook.SampleTrait.UserProfile
import facebook.SampleTrait.fbFormat._*/
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

class Master extends Actor with FbApi{
  import scala.concurrent.ExecutionContext.Implicits.global  //***check if this is actually needed or not
  implicit val sys = context.system
  override implicit def actorRefFactory: ActorRefFactory = sys
  val pipeline = sendReceive
    def receive = {
      case "create" => println("Inside Master")
        for(i<- 0 to 24){
          val friendlist = friendMap.get(i).friendList
          val hashMap = pageMap
          Master.this.FbPage
          val child = context.system.actorOf(Props(new ClientUser(pipeline,i, friendlist)), "activeuser"+i.toString)
          child ! "high"
        }
       for(i<- 25 to 45 ){
          val friendlist = friendMap.get(i).friendList
          val child = context.system.actorOf(Props(new ClientUser(pipeline,i, friendlist)), "activeuser"+i.toString)
          child ! "medium"
        }
        for(i<- 46 to 99){
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

trait FbApi extends HttpService with DefaultJsonProtocol{
  case class UserProfile(userid: Int, name: String, age: Int, dob: Int, wall: FbPost, picture: String,  var albumid: List[Int])
  case class ProfilePic(pic: File)
  case class FacebookFriends(var friendList: List[Int])
  case class FbPage(pageid: Int, name: String, admin: Int, var subscribers: List[Int], wall: FbPost)
  case class FbPost(var posts: Map[String, String])
  case class Album(photos: List[String])
  implicit def executionContext = actorRefFactory.dispatcher

  object fbFormat extends DefaultJsonProtocol {
    implicit val fbPostFormat = jsonFormat1(FbPost.apply)
    implicit val userProfileFormat = jsonFormat7(UserProfile.apply)
    implicit val facebookFriendsFormat = jsonFormat1(FacebookFriends.apply)
    implicit val albumFormat = jsonFormat1(Album.apply)
  }

  var userProfileMap = new ConcurrentHashMap[Int, UserProfile]
  var friendMap = new ConcurrentHashMap[Int, FacebookFriends]
  var pageMap = new ConcurrentHashMap[Int, FbPage]
  var albumMap = new ConcurrentHashMap[Int, Album]

  //create 10,000 users
  for (i <- 0 until 100) {
    val initialPost = FbPost(Map())
    initialPost.posts += (0.toString() -> "Hello")

    //ProfilePic(File.createTempFile("profilepic", ".jpg", new File("C:\\Users\\Pratyoush\\Desktop\\tmp")));
    val ftmp = File.createTempFile("upload", ".jpg", new File("C:\\Users\\Pratyoush\\Desktop\\tmp"));
    userProfileMap.put(i, UserProfile(i, "UserProfile" + i.toString, i, i + 10, initialPost, ftmp.getAbsolutePath,
      List[Int]() ))
    friendMap.put(i, FacebookFriends(List()))
  }

  for(i <- 0 until 10){
    val random = new Random()
    val r = random.nextInt(100)  // admin id
    val p = random.nextInt(20)  //number of subscribers
    var subscribers = List[Int]()
    for(j <- 0 to p ){
      val u = random.nextInt(100) //pick a random user
      val user = userProfileMap.get(u)
      if (!subscribers.contains(user)){
        subscribers = userProfileMap.get(u).userid :: subscribers
      }
    }
    val initialPost = FbPost(Map())
    initialPost.posts.updated(0.toString,"")
    pageMap.put(i, FbPage(i, "Page"+i.toString()+ ": admin "+userProfileMap.get(r).userid, userProfileMap.get(r).userid, subscribers, initialPost ))
  }
  for (i <- 0 until 100) {
    val random = new Random()
    val r = random.nextInt(10) //number of friends
    for (j <- 0 to r) {
      val u = random.nextInt(99) //pick a random user
      val user = userProfileMap.get(u).userid
      if (!friendMap.contains(user)) {
        friendMap.get(i).friendList = user :: friendMap.get(i).friendList//add the random user to i's friendlist
        friendMap.get(u).friendList = userProfileMap.get(i).userid :: friendMap.get(u).friendList
      }
    }
  }
  /*for (i <- 2500 until 5000) {
    val random = new Random()
    val range = 100 to random.nextInt(400) //number of friends
    for (j <- 0 to range.end) {
      val u = random.nextInt(4999) //pick a random user
      val user = userProfileMap.get(u).userid
      if (!friendMap.contains(user)) {
        friendMap.get(i).friendList = user :: friendMap.get(i).friendList//add the random user to i's friendlist
        friendMap.get(u).friendList = userProfileMap.get(i).userid :: friendMap.get(u).friendList
      }
    }
  }
    for (i <- 500 until 1500) {
      val random = new Random()
      val range = 10 to random.nextInt(25) //number of friends
      for (j <- 0 to range.end) {
        val u = random.nextInt(4999) //pick a random user
        val user = userProfileMap.get(u).userid
        if (!friendMap.contains(user)) {
          friendMap.get(i).friendList = user :: friendMap.get(i).friendList//add the random user to i's friendlist
          friendMap.get(u).friendList = userProfileMap.get(i).userid :: friendMap.get(u).friendList
        }
      }
    }
    for (i <- 1500 until 2500) {
      //Initialize friendlist of remaining users
      val random = new Random()
      val range = 25 to random.nextInt(100) //number of friends
      for (j <- 0 to range.end) {
        val u = random.nextInt(4999) //pick a random user
        val user = userProfileMap.get(u).userid
        if (!friendMap.contains(user)) {
          friendMap.get(i).friendList = user :: friendMap.get(i).friendList//add the random user to i's friendlist
          friendMap.get(u).friendList = userProfileMap.get(i).userid :: friendMap.get(u).friendList
        }
      }
    }*/
  println("Ready!")
}
