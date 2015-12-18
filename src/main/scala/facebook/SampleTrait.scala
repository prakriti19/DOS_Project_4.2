package facebook

import java.io.File
import java.io._
import java.nio.file.{Files, Paths}
import java.nio.file.attribute.UserPrincipalLookupService
import java.security.spec.X509EncodedKeySpec
import java.security._
import java.util.concurrent.ConcurrentHashMap
import javax.crypto.Cipher
import javax.crypto.spec.{SecretKeySpec, IvParameterSpec}

import akka.japi.Option
//import facebook.client.FbApi
import org.apache.commons.codec.binary.Base64
//import sun.security.ssl.KeyManagerFactoryImpl.X509

/*
import facebook.client.FbApi.FacebookFriends
import facebook.client.FbApi.FbPost
import facebook.client.FbApi.UserProfile
*/

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
trait SampleTrait extends HttpService with DefaultJsonProtocol {

  case class UserProfile(userid: Int, name: String, age: Int, dob: Int, wall: FbPost, var picture: (String,String) , var albumid: List[Int])

  case class ProfilePic(pic: File)

  case class FacebookFriends(var friendList: List[Int])

  case class FbPage(pageid: Int, name: String, admin: Int, var subscribers: List[Int], wall: FbPost)

  case class FbPost(var posts: ConcurrentHashMap[String, (String, String)])

  case class Album(photos: List[String])

  implicit def executionContext = actorRefFactory.dispatcher

  /*object fbFormat extends DefaultJsonProtocol {
    implicit val fbPostFormat = jsonFormat1(FbPost.apply)
    implicit val userProfileFormat = jsonFormat7(UserProfile.apply)
    implicit val facebookFriendsFormat = jsonFormat1(FacebookFriends.apply)
    implicit val albumFormat = jsonFormat1(Album.apply)
  }
*/
  def DSDecrypt(text: Array[Byte], key1: PublicKey): Array[Byte] = {
    var cipherText: Array[Byte] = null
    val cipher: Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.DECRYPT_MODE, key1)
    cipherText = cipher.doFinal(text);
    //println(" encrypted text " + cipherText);
    return cipherText;
  }

  var initVector1:String = "RandomInitVector"

  def encryptAESServer(key:String, initVector:String , value:Array[Byte]):Array[Byte] = {
    var iv:IvParameterSpec = new IvParameterSpec(initVector.getBytes("UTF-8"));
    var skeySpec:SecretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES");
    var cipher:Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv);
    return cipher.doFinal(value);
  }

  def encryptRSA (text:Array[Byte], key1:PublicKey) : Array[Byte] = {
    var cipherText: Array[Byte] = null
    val cipher: Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.ENCRYPT_MODE, key1)
    cipherText = cipher.doFinal(text);
    println(" encrypted text " + cipherText);
    return cipherText;
  }
  var userProfileMap = new ConcurrentHashMap[Int, UserProfile]
  var friendMap = new ConcurrentHashMap[Int, FacebookFriends]
  var pageMap = new ConcurrentHashMap[Int, FbPage]
  var albumMap = new ConcurrentHashMap[Int, Album]
  var publicKeyMap = new ConcurrentHashMap[Int, PublicKey]
  var secureStringMap = new ConcurrentHashMap[Int, Long]

  def MD5(s: String): String = {
    // Besides "MD5", "SHA-256", and other hashes are available
    val m = java.security.MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8"))
    m.map("%02x".format(_)).mkString
  }

  def SRNG2(): Long = {
    val random = new scala.util.Random(new java.security.SecureRandom())
    val key = random.nextLong()

    return key
  }
  def ServerSRNG():String = {
    val random = new scala.util.Random(new java.security.SecureRandom())
    val random2 = new SecureRandom()
    var key:Array[Char] = Array[Char](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    for(i<- 0 until 16){
      key(i) = random.nextPrintableChar()
    }

    return key.mkString("")
  }

  //create 10,000 users
  for (i <- 0 to 10) {
    val initialPost = FbPost(new ConcurrentHashMap())
    //initialPost.posts += (0.toString() -> "Hello")
    // initialPost.posts.put(0.toString,"hi11")
    //initialPost.posts.put(1.toString,"hi33")
    //ProfilePic(File.createTempFile("profilepic", ".jpg", new File("C:\\Users\\Pratyoush\\Desktop\\tmp")));
    val ftmp = File.createTempFile("upload", ".jpg", new File("C:\\Users\\Prakriti\\Desktop\\tmp"));
    userProfileMap.put(i, UserProfile(i, "UserProfile" + i.toString, i, i + 10, initialPost, (ftmp.getAbsolutePath,"key"),
      List[Int]()))
    friendMap.put(i, FacebookFriends(List()))
  }

  /*for(i <- 0 until 100){
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
  }*/
  for (i <- 0 to 10) {
    val random = new Random()
    val r = random.nextInt(10) //number of friends
    for (j <- 0 to r) {
      val u = random.nextInt(9) //pick a random user
      val user = userProfileMap.get(u).userid
      if (!friendMap.contains(user)) {
        friendMap.get(i).friendList = user :: friendMap.get(i).friendList //add the random user to i's friendlist
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



  println("Ready!")
  /*
  object fbFormat2 extends DefaultJsonProtocol {
    implicit val fbPostFormat = jsonFormat1(FbPost.apply)
    implicit val userProfileFormat = jsonFormat7(UserProfile.apply)
    implicit val facebookFriendsFormat = jsonFormat1(FacebookFriends.apply)
    implicit val albumFormat = jsonFormat1(Album.apply)
  }
  import fbFormat2._
*/

  val sampleRoute = {
    get {
      //respondWithMediaType(MediaTypes.`application/json`) {
      path(IntNumber / "register") { id =>
        parameters("publickey".as[String]) { publicKey =>
          var decodedPublicKey = publicKey
          var pubKey: PublicKey = KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(Base64.decodeBase64(decodedPublicKey)))
          publicKeyMap.put(id, pubKey)
          println(" public key at fb Server is " + pubKey + " for user " + id)
          complete {
            "with id $id" /*+ res.toJson.prettyPrint*/
          }
        }
      } ~
        path(IntNumber / "login") { id =>
          val secureNum = SRNG2()
          secureStringMap.put(id, secureNum)
          complete {
            "" + secureNum /*+ res.toJson.prettyPrint*/
          }
        } ~
        path(IntNumber / "loginverify") { id =>
          parameters("es".as[String]) { es =>
            val decryptedSecureNum: String = new String(DSDecrypt(Base64.decodeBase64(es), publicKeyMap.get(id)), "UTF-8")
            println(s" secureStringMap.get(${id})  " + secureStringMap.get(id) + "decryptedSecureNum.toLong  " + decryptedSecureNum.toLong)
            if (secureStringMap.get(id) == decryptedSecureNum.toLong) {
              complete {
                "loginsuccessful" /*+ res.toJson.prettyPrint*/
              }
            }
            else {
              complete {
                "loginunsuccessful"
              }
            }
          }
        }~
            path(IntNumber / "user" / IntNumber / "addfriend") { (userid, friendid) => //userid wants to add friendid
              if (friendid >= userProfileMap.size()) {
                complete {
                  "Not a valid user."
                }
              }
              else if (!friendMap.get(userid).friendList.contains(friendid)) {
                friendMap.get(userid).friendList = friendid :: friendMap.get(userid).friendList //add the random user to i's friendlist
                friendMap.get(friendid).friendList = userid :: friendMap.get(friendid).friendList
                complete {
                  "Friend " + friendid + "added to friendlist of " + userid
                }
              }
              else {
                complete {
                  "Already a friend. Find another friend."
                }
              }
            } ~
            path(IntNumber / "user" / IntNumber / "deletefriend") { (userid, friendid) =>
              if (friendid >= userProfileMap.size()) {
                complete {
                  "Not a valid user."
                }
              }
              else if (friendMap.get(userid).friendList.contains(friendid)) {

                friendMap.get(userid).friendList = friendMap.get(userid).friendList diff List(friendid)
                friendMap.get(friendid).friendList = friendMap.get(friendid).friendList diff List(userid)
                complete {
                  "Friend " + friendid + "deleted from friendlist of " + userid
                }
              }
              else {
                complete {
                  "Not your friend."
                }
              }
            } ~
            path(IntNumber / "profile") { id =>

              var res = userProfileMap.get(id)
              complete {
                "with id $id" + res /*.toJson.prettyPrint*/
              }
            } ~
            path(IntNumber / "profile" / "deleteprofile") { id =>
              val user = friendMap.get(id)
              println(user.friendList.size)
              for (i <- 0 to (user.friendList.size - 1)) {
                //few checks are to be added
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
            path(IntNumber / "profile" / "update") { userid =>
              parameter("name".?, "age".?, "dob".?) { (newName, newAge, newDob) =>
                if (userProfileMap.get(userid) != null) {
                  if (newName != null) {
                    val u1 = userProfileMap.get(userid)
                    val u2 = u1.copy(name = newName.getOrElse().toString)
                    userProfileMap.put(userid, u2)
                  }
                  if (!newAge.isEmpty) {
                    val u1 = userProfileMap.get(userid)
                    val u2 = u1.copy(age = newAge.getOrElse().toString.toInt)
                    userProfileMap.put(userid, u2)
                  }
                  if (!newDob.isEmpty) {
                    val u1 = userProfileMap.get(userid)
                    val u2 = u1.copy(dob = newDob.getOrElse().toString.toInt)
                    userProfileMap.put(userid, u2)
                  }
                  complete {
                    "User Details Updated"
                  }
                }
                else {
                  complete {
                    "Not a valid user"
                  }
                }

              }
            } ~
            path(IntNumber / "wallposts") { id =>
              var res = userProfileMap.get(id).wall.posts

              complete {
                "" + res.values() /*.toJson.prettyPrint*/
              }
            } ~
            path(IntNumber / "wallposts" / IntNumber / "addpost") { (userid, friendid) => //friendid posts on userid
              parameters("post".as[String], "key".as[String]) { (post, key) =>
                if (friendMap.get(userid).friendList.contains(friendid)) {
                  val size = userProfileMap.get(friendid).wall.posts.size
                  //userProfileMap.get(friendid).wall.posts += (size.toString() -> post)
                  userProfileMap.get(friendid).wall.posts.put(size.toString, (post, key))
                  complete {
                    "\nadded a post" + " length" + userProfileMap.get(friendid).wall.posts.size()
                  }
                }
                else {
                  complete {
                    "Not Allowed to Post!"
                  }
                }
              }
            } ~
            path(IntNumber / "wallposts" / "deletepost" / IntNumber) { (userid, postid) => //check if friend is in friendlist
              if (userProfileMap.get(userid).wall.posts.containsKey(postid.toString())) {
                userProfileMap.get(userid).wall.posts.remove(postid.toString())
                complete {
                  "\nDeleted post " + postid
                }
              }
              else {
                complete {
                  "Wrong Post ID!"
                }
              }
            } ~
            path(IntNumber / "friendlist") { id =>
              var res = friendMap.get(id)
              complete {
                "Friend List of UserProfile" + id + "\n" + res /*.toJson.prettyPrint*/
              }
            } ~
            path(IntNumber / "getprofilepic") { id =>
             /* val ftmp = userProfileMap.get(id).picture._1
              //var path = ftmp.getAbsolutePath
              val input = new FileInputStream(ftmp)
              var c = 0
              var picByte = new ArrayBuffer[Character]
              while ( {
                c = input.read(); c != -1
              }) {
                picByte += c.toChar
              }

              input.close()
*/
              complete {
                //picByte.toString()/*.toJson.prettyPrint*/
                "" + userProfileMap.get(id).picture._1 + "),(" + userProfileMap.get(id).picture._2
              }
            } ~
            path(IntNumber / "album" / IntNumber) { (userid, albumid) => //user accesses his album
              if (userProfileMap.get(userid).albumid.contains(albumid)) {
                complete {
                  "Album " + albumid + "\n" + albumMap.get(albumid).toString /*.toJson.prettyPrint*/
                }
              }
              else {
                complete {
                  "Wrong Album ID!"
                }
              }
            } ~
            path(IntNumber / "user" / IntNumber / "showalbum" / IntNumber / "friend") { (userid, albumid, friendid) => //friend accesses user's album
              if (userProfileMap.get(userid).albumid.contains(albumid) && friendMap.get(userid).friendList.contains(friendid)) {
                complete {
                  "Album " + albumid + "\n" + albumMap.get(albumid).toString /*.toJson.prettyPrint*/
                }
              }
              else {
                complete {
                  "Wrong Album ID!"
                }
              }
            } ~
            path(IntNumber / "page") { pageid =>
              var res = pageMap.get(pageid).toString
              complete {
                "Page\n\n" + res /*.toJson.prettyPrint*/
              }
            } ~
            path(IntNumber / "page" / "addpost" / IntNumber) { (pageid, userid) =>
              parameters("post".as[String]) { post =>
                if (userid == pageMap.get(pageid).admin || pageMap.get(pageid).subscribers.contains(userid)) {
                  val size = pageMap.get(pageid).wall.posts.size
                  //pageMap.get(pageid).wall.posts += (size.toString() -> post)
                  //pageMap.get(pageid).wall.posts.put(size.toString(), post)
                  complete {
                    "Post Added by " + userid + "\nPost : " + post
                  }
                }
                else {
                  complete {
                    "Not allowed to Post!! " + pageid + userid
                  }
                }
              }
            } ~
            path(IntNumber / "page" / IntNumber / "user" / "deletepost" / IntNumber) { (pageid, userid, postid) => //check if friend is in friendlist
              if (userid == pageMap.get(pageid).admin) {
                if (pageMap.get(pageid).wall.posts.containsKey(postid.toString())) {
                  pageMap.get(pageid).wall.posts.remove(postid.toString())
                  complete {
                    "\nDeleted post " + postid
                  }
                }
                else {
                  complete {
                    "Wrong Post ID!"
                  }
                }
              }
              else {
                complete {
                  "Not Allowed to Delete!"
                }
              }
            } ~
            path(IntNumber / "page" / "subscribers") { pageid =>
              var res = pageMap.get(pageid).subscribers
              complete {
                "Page Subscribers\n\n" + res /*.toJson.prettyPrint*/
              }
            }
        } ~
        path(IntNumber / "addprofilepic") { id => //****check
          post {
            entity(as[MultipartFormData]) {
              formData => {
                //val ftmp = File.createTempFile("upload", ".jpg", new File("C:\\Users\\Pratyoush\\Desktop\\tmp"))
                val ftmp = userProfileMap.get(id).picture._1
                val key:String = ServerSRNG()   //check if its working
                //println ( " id is " +  id + " public key " + publicKeyMap.get(id) )
                //val encryptKey =  new String( encryptRSA( key.getBytes("UTF-8"), publicKeyMap.get(id)), "UTF-8")
                val encryptKey =  Base64.encodeBase64String(encryptRSA( key.getBytes("UTF-8"), publicKeyMap.get(id)))
                //println( " encrypt key is " + encryptKey)

                val output = new FileOutputStream(ftmp)
                formData.fields.foreach(f => {
                 output.write(f.entity.data.toByteArray)})

                val byteArray = Files.readAllBytes(Paths.get(ftmp))
                println(ftmp +  "  this is the path")
                val encryptedByteArray = encryptAESServer(key, initVector1, byteArray)
                val encodedByteArray = Base64.encodeBase64String(encryptedByteArray)
                println("Picture Uploaded " + new String (byteArray, "UTF-8"))

                val u1 = userProfileMap.get(id)
                val u2 = u1.copy(picture =(encodedByteArray,encryptKey) )
                userProfileMap.put(id, u2)

                output.close()
                //println(" file here " + base64String)
                //println(" sssss    " + userProfileMap.get(id).picture._1 )
                //println(" picture object     " + picByte.mkString("") )
               // println("*********** " + s)
                complete("Updated Profile Picture!")
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
                userProfileMap.get(userid).albumid = id :: userProfileMap.get(userid).albumid
                complete("done, file in: ")
              }
            }

          }
        }
    }


  }

