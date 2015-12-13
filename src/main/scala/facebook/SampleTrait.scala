package facebook

import java.io.File
import java.io._
import java.nio.file.attribute.UserPrincipalLookupService
import java.util.concurrent.ConcurrentHashMap

import akka.japi.Option
import facebook.client.FbApi
//import facebook.client.FbApi.fbFormat._
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
trait SampleTrait extends HttpService with DefaultJsonProtocol with FbApi{


  println("Ready!")
  object fbFormat2 extends DefaultJsonProtocol {
    implicit val fbPostFormat = jsonFormat1(FbPost.apply)
    implicit val userProfileFormat = jsonFormat7(UserProfile.apply)
    implicit val facebookFriendsFormat = jsonFormat1(FacebookFriends.apply)
    implicit val albumFormat = jsonFormat1(Album.apply)
  }
  import fbFormat2._

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
                val ftmp = File.createTempFile("album", ".jpg", new File("C:\\Users\\Pratyoush\\Desktop\\tmp"))
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

