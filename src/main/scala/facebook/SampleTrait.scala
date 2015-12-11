package facebook

import java.io.File
import java.io._
import java.nio.file.attribute.UserPrincipalLookupService
import java.util.concurrent.ConcurrentHashMap

import akka.japi.Option
import spray.http._
import spray.json.DefaultJsonProtocol
import spray.routing.HttpService

import scala.Option
import scala.collection.immutable.HashMap
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.util.Random
import MediaTypes._
import spray.json._
import java.util.UUID
import scala.reflect.ClassTag
import spray.httpx.marshalling.{MetaMarshallers, Marshaller, CollectingMarshallingContext}
import spray.httpx.SprayJsonSupport

import scala.util.parsing.json.JSONObject


/**
  * Created by Pratyoush on 24-11-2015.
  */
trait SampleTrait extends HttpService with DefaultJsonProtocol{

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
    val ftmp = File.createTempFile("upload", ".jpg", new File("C:\\Users\\Prakriti\\Desktop\\tmp"));
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
  import fbFormat._

  val sampleRoute = {
    get {
      //respondWithMediaType(MediaTypes.`application/json`) {
      path(IntNumber / "user" / IntNumber / "addfriend"){ (userid, friendid) =>   //userid wants to add friendid
        if(friendid >= userProfileMap.size()){
          complete{
            "Not a valid user."
          }
        }
        else if(!friendMap.get(userid).friendList.contains(friendid)){
          friendMap.get(userid).friendList = friendid :: friendMap.get(userid).friendList //add the random user to i's friendlist
          friendMap.get(friendid).friendList = userid :: friendMap.get(friendid).friendList
          complete{
            "Friend " + friendid + "added to friendlist of " + userid
          }
        }
        else{
          complete{
            "Already a friend. Find another friend."
          }
        }
      }~
      path(IntNumber / "user" / IntNumber / "deletefriend"){ (userid, friendid) =>
        if(friendid >= userProfileMap.size()){
          complete{
            "Not a valid user."
          }
        }
        else if(friendMap.get(userid).friendList.contains(friendid)){

          friendMap.get(userid).friendList = friendMap.get(userid).friendList diff List(friendid)
          friendMap.get(friendid).friendList = friendMap.get(friendid).friendList diff List(userid)
          complete{
            "Friend " + friendid + "deleted from friendlist of " + userid
          }
        }
        else{
          complete{
            "Not your friend."
          }
        }
      }~
      path(IntNumber / "profile") { id =>

        var res = userProfileMap.get(id)
        complete {
          "with id $id" + res.toJson.prettyPrint
        }
      } ~
        path(IntNumber / "profile" / "deleteprofile") { id =>
          val user = friendMap.get(id)
          println(user.friendList.size)
          for(i <- 0 to (user.friendList.size - 1)){   //few checks are to be added
              val friendid = user.friendList(i)
              println(friendid)
            friendMap.get(friendid).friendList = friendMap.get(friendid).friendList diff List(id)
          }
          userProfileMap.remove(id)
          friendMap.remove(id)
          complete {
            "Deleting user "
          }
        } ~
      path(IntNumber / "profile" / "update") { userid=>
        parameter("name".?, "age".?, "dob".?){ (newName,newAge,newDob) =>
          if(userProfileMap.get(userid) != null){
            if(newName != null){
              val u1 = userProfileMap.get(userid)
              val u2 = u1.copy(name = newName.getOrElse().toString)
              userProfileMap.put(userid, u2)
            }
            if(!newAge.isEmpty){
              val u1 = userProfileMap.get(userid)
              val u2 = u1.copy(age = newAge.getOrElse().toString.toInt)
              userProfileMap.put(userid, u2)
            }
            if(!newDob.isEmpty){
              val u1 = userProfileMap.get(userid)
              val u2 = u1.copy(dob = newDob.getOrElse().toString.toInt)
              userProfileMap.put(userid, u2)
            }
            complete{
              "User Details Updated"
            }
          }
          else{
            complete{
              "Not a valid user"
            }
          }

        }
      }~
        path(IntNumber / "wallposts") { id =>
          complete {
            "with id $id" + userProfileMap.get(id).wall
          }
        } ~
        path(IntNumber / "wallposts" / IntNumber / "addpost" ) { (userid, friendid) =>    //friendid posts on userid
          parameters("post".as[String]) { post =>
            if(friendMap.get(userid).friendList.contains(friendid)){
              val size = userProfileMap.get(userid).wall.posts.size
              userProfileMap.get(userid).wall.posts += (size.toString() -> post)
              complete {
                "\nadded a post"
              }
            }
            else{
              complete{
                "Not Allowed to Post!"
              }
            }
         }
        } ~
        path(IntNumber / "wallposts" / "deletepost" / IntNumber) { (userid, postid) =>    //check if friend is in friendlist
            if(userProfileMap.get(userid).wall.posts.contains(postid.toString())){
              userProfileMap.get(userid).wall.posts.-(postid.toString())
              complete {
                "\nDeleted post " + postid
              }
            }
            else{
              complete{
                "Wrong Post ID!"
              }
            }
        } ~
        path(IntNumber / "friendlist") { id =>
          complete {
            "Friend List of UserProfile" + id + "\n" + friendMap.get(id)
          }
        }~
        path(IntNumber / "getprofilepic") { id =>
            val ftmp = userProfileMap.get(id).picture
            //var path = ftmp.getAbsolutePath
            val input = new FileInputStream(ftmp)
            var c = 0
            var picByte = new ArrayBuffer[Character]
            while({c = input.read(); c != -1}){
              picByte += c.toChar
            }
            //formData.fields.foreach(f =>output.write(f.entity.data.toByteArray))
            //println(picByte)
            input.close()
            complete{
              picByte.toString().toJson.prettyPrint
            }
          }~
        path(IntNumber / "album" / IntNumber ){ (userid, albumid)=>  //user accesses his album
          if(userProfileMap.get(userid).albumid.contains(albumid)) {
            complete {
              "Album " + albumid + "\n" + albumMap.get(albumid).toString.toJson.prettyPrint
            }
          }
          else{
            complete{
              "Wrong Album ID!"
            }
          }
        }~
        path(IntNumber / "user"/ IntNumber / "showalbum" / IntNumber / "friend"){ (userid, albumid, friendid)=>  //friend accesses user's album
          if(userProfileMap.get(userid).albumid.contains(albumid) && friendMap.get(userid).friendList.contains(friendid)){
            complete{
              "Album "+ albumid + "\n" + albumMap.get(albumid).toString.toJson.prettyPrint
            }
          }
          else{
            complete{
              "Wrong Album ID!"
            }
          }
        }~
        path(IntNumber / "page"){ pageid=>
          complete{
            "Page\n\n"+ pageMap.get(pageid)
          }
        }~
        path(IntNumber /"page"/ "addpost" / IntNumber){ (pageid, userid)=>
          parameters("post".as[String]){post=>
            if(userid == pageMap.get(pageid).admin || pageMap.get(pageid).subscribers.contains(userid)){
              val size = pageMap.get(pageid).wall.posts.size
              pageMap.get(pageid).wall.posts += (size.toString() -> post)
              complete{
                "Post Added by "+ userid + "\nPost : " + post
              }
            }
            else{
              complete{
                "Not allowed to Post!! "+ pageid+userid
              }
            }
          }
        }~
        path(IntNumber / "page" / IntNumber / "user" / "deletepost" / IntNumber) { (pageid, userid, postid)=> //check if friend is in friendlist
          if (userid == pageMap.get(pageid).admin) {
            if (pageMap.get(pageid).wall.posts.contains(postid.toString())) {
              pageMap.get(pageid).wall.posts.-(postid.toString())
              complete {
                "\nDeleted post " + postid
              }
            }
            else{
              complete{
                "Wrong Post ID!"
              }
            }
          }
          else{
            complete{
              "Not Allowed to Delete!"
            }
          }
        } ~
        path(IntNumber / "page"/ "subscribers"){ pageid=>
          complete{
            "Page Subscribers\n\n"+ pageMap.get(pageid).subscribers
          }
        }
    } ~
    path(IntNumber / "addprofilepic") { id => //****check
      post {
        entity(as[MultipartFormData]) {
          formData => {
            //val ftmp = File.createTempFile("upload", ".jpg", new File("C:\\Users\\Pratyoush\\Desktop\\tmp"))
            val ftmp = userProfileMap.get(id).picture

            val output = new FileOutputStream(ftmp)
            formData.fields.foreach(f => output.write(f.entity.data.toByteArray))
            output.close()
            complete("done, file in: " + ftmp)
          }
        }
      }
    } ~
      path(IntNumber / "addalbum") { userid => //****check
        post {
          entity(as[MultipartFormData]) {
            formData => {
              var photos = List[String]()
              formData.fields.foreach(f => {
                val ftmp = File.createTempFile("album", ".jpg", new File("C:\\Users\\Prakriti\\Desktop\\tmp"))
                val output = new FileOutputStream(ftmp)
                output.write(f.entity.data.toByteArray)
                photos = ftmp.getAbsolutePath :: photos
                  println("file done!!")
                output.close()
              })
              val id = albumMap.size()
              albumMap.put(id, Album(photos))
              userProfileMap.get(userid).albumid =  id :: userProfileMap.get(userid).albumid
              complete("done, file in: ")
            }
          }

        }
      }
  }


  }

