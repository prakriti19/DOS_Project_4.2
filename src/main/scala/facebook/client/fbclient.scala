package facebook.client

import java.io.File
import java.math.BigInteger
import java.util.concurrent.ConcurrentHashMap
import javax.crypto.spec.{SecretKeySpec, IvParameterSpec}

import akka.actor._
import org.apache.commons.codec.binary.Base64

import scala.concurrent.Await
import akka.pattern.ask
import akka.actor._
import akka.util.Timeout
//import facebook.client.ClientUser.FbApi

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

import java.security._
import javax.crypto._
import java.security.interfaces._
import javax.crypto.interfaces.DHKey
import scala.util.parsing.combinator.RegexParsers
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.PublicKey;

case class addPost(encrypt:Array[Byte],encryptKey:Array[Byte])
case class sendPost(sendPostMap:ConcurrentHashMap[Int,(String,String)], pubKey:PublicKey)
case class requestPic(pubKey:PublicKey)
/**
  * Created by Pratyoush on 29-11-2015.
  */
object fbclient extends App{
  implicit val system = ActorSystem("Main")
  val master = system.actorOf(Props[Master], "Master")
  master ! "create"
}

class Master extends Actor with SampleTrait{
  //import scala.concurrent.ExecutionContext.Implicits.global  //***check if this is actually needed or not
  implicit val sys = context.system
  override implicit def actorRefFactory: ActorRefFactory = sys
  val pipeline = sendReceive
    def receive = {
      case "create" => println("Inside Master")
      /*  for(i<- 0 to 24){
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
        }*/
        for(i<- 0 to 10){
          val friendlist = friendMap.get(i).friendList
          val child = context.system.actorOf(Props(new ClientUser(pipeline,i, friendlist)), "activeuser"+i.toString)
          child ! "low"
        }
    }
  }

class ClientUser(pipeline: pipelining.SendReceive,userid: Int, friendlist: List[Int]) extends Actor with SampleTrait{
  implicit val sys = context.system
  override implicit def actorRefFactory: ActorRefFactory = sys
  //import scala.concurrent.ExecutionContext.Implicits.global  //***check if this is actually needed or not
  //override implicit def executionContext = actorRefFactory.dispatcher
  def SRNG():String = {
    val random = new scala.util.Random(new java.security.SecureRandom())
    var key:Array[Char] = Array[Char](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    for(i<- 0 until 16){
      key(i) = random.nextPrintableChar()
    }

    return key.mkString("")
  }
  println(self.path.name)
  var postTemp = new ConcurrentHashMap[String,(Array[Byte],Array[Byte])]
  val keys:KeyPairGenerator = KeyPairGenerator.getInstance("RSA");
  keys.initialize(1024);
  val keypair:KeyPair = keys.generateKeyPair
  val privateKey:PrivateKey = keypair.getPrivate;
  val publicKey:PublicKey = keypair.getPublic;
//println("public keys of "+userid+ " are "  + publicKey);
 /* var key:String = SRNG().toString/*"Bar12345Bar12345";*/ // 16 byte key //should be changed
 */ var initVector:String = "RandomInitVector"; // 16 byte IV

  val password = "user"+userid.toString;


    def receive = {
      case "publickey" =>{
  //      println(sender.path.name + "sends message to " + self.path.name)
        sender ! publicKey
      }

      case addPost(encrypt:Array[Byte], encryptKey:Array[Byte]) =>{
        postTemp.put(postTemp.size().toString,(encrypt,encryptKey))
        sender !"Post added"
      }
      case requestPic(pubKey:PublicKey)=> {
        println("sender.path.name222 = " + sender.path.name)
       /* var result = Get("http://localhost:8080/9/getprofilepic") ~> sendReceive
        result.foreach { response =>
          var picture: Array[Byte] = response.entity.asString.getBytes("UTF-8")
          println(" pic########### " + picture)
          var key: String = SRNG()
          var encryptedPicAES = encryptAES(key, initVector, picture)
          var encryptedKeyRSA = encrypt(key.getBytes("UTF-8"), pubKey)
          println("sender.path.name = " + sender.path.name)
          sender ! "Test Message"/*(encryptedPicAES, encryptedKeyRSA)*/
        }*/
      }
      case sendPost(sendPostMap:ConcurrentHashMap[Int,(String, String)], pubKey:PublicKey) =>{
        var returnVal:ConcurrentHashMap[Int,(String,String)] = new ConcurrentHashMap[Int,(String,String)]()
        for( i <- 0 until sendPostMap.size()){
          var key = decrypt(Base64.decodeBase64(sendPostMap.get(i)._2),privateKey)
         // var posts = decryptAES(new String(key, "UTF-8"),initVector,Base64.decodeBase64(mapOfRow.get(i)._1) )
          var key2: String= Base64.encodeBase64String(encrypt(key,pubKey))
          returnVal.put(i, (sendPostMap.get(i)._1, key2))
        }
        sender ! returnVal
      }
      /*case sendPost(sendPostMap:ConcurrentHashMap[String,(Array[Byte],Array[Byte])]) =>{
        for( i <- 0 until sendPostMap.size()){
          var decryptKey:String = new String(decrypt(sendPostMap.get(i.toString)._2, privateKey), "UTF-8")
          var decryptPost:Array[Byte] = decryptAES(decryptKey, initVector, sendPostMap.get(i.toString)._1)
          println( " friend's wall post "+ i.toString + " is " + new String(decryptPost,"UTF-8"))
        }
        sender !"successful"
      }*/
      case "low" => {
        context.system.scheduler.schedule(0 seconds, 20 seconds) ({var result = Get("http://localhost:8080/"+userid+"/profile") ~> sendReceive
          result.foreach { response =>
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }
      })

       /* context.system.scheduler.schedule(0 seconds, 20 seconds) ({

            implicit val timeout = Timeout (15 seconds)
            val future = context.actorSelection("../activeuser9") ? requestPic(publicKey)

            val picKeyPair = Await.result(future, timeout.duration).asInstanceOf[String/*(Array[Byte],Array[Byte])*/]
          println(" current user broooo = " + self.path.name)
            println("Message Received  " + picKeyPair)
          /*val decryptKeyRSA:String = new String(decrypt(picKeyPair._2,privateKey), "UTF-8")
            val decryptedPic:String = new String( decryptAES(decryptKeyRSA, initVector, picKeyPair._1), "UTF-8")
            println(" picture @@@@@@@   " + decryptedPic)*/
            //println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")

        })
*/
       var result = Get("http://localhost:8080/"+userid.toString +s"/register?publickey=${Base64.encodeBase64String(publicKey.getEncoded)}") ~> sendReceive
          result.foreach { response =>
            println(s"Friends Profile!!!! Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }

        context.system.scheduler.schedule(0 seconds, 30 seconds) ({val random = new Random()
          val r = random.nextInt(friendlist.size-1)
          var result = Get("http://localhost:8080/"+friendlist(r).toString +"/profile") ~> sendReceive
          result.foreach { response =>
            println(s"Friends Profile!!!! Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }})


        context.system.scheduler.schedule(0 seconds, 30 seconds) ({
          var result = pipeline(Get("http://localhost:8080/"+userid+"/wallposts"))
          result.foreach { response =>
            var postMapnew = response.entity.asString
            var row = postMapnew.split("\\), \\(")
            var mapOfRow:ConcurrentHashMap[Int,(String,String)] = new ConcurrentHashMap[Int,(String,String)]()
            println("\nRow Length = " + row.length + "\n")
            if(row.length >0){
             for(i <- 0 until row.length){
               row(i) = row(i).replaceFirst("\\[\\(","")
               row(i) = row(i).replaceFirst("\\)\\]","")
               var parts:Array[String] = row(i).split(",")
               parts(0) = parts(0).replaceAll("_","=")
               parts(1) = parts(1).replaceAll("_","=") // error for part 1
               parts(0) = parts(0).replaceAll(" ","+")
               parts(1) = parts(1).replaceAll(" ","+")
               mapOfRow.put(i,(parts(0),parts(1)))
             }
           }
            if(mapOfRow.size() >0){
              for( i <- 0 until mapOfRow.size()){
                var key = decrypt(Base64.decodeBase64(mapOfRow.get(i)._2),privateKey)
                var posts = decryptAES(new String(key, "UTF-8"),initVector,Base64.decodeBase64(mapOfRow.get(i)._1) )
                println( " decrypted post should come " + new String(posts, "UTF-8"))
              }
            }

            println(s"Request completed with status ${response.status}")
          }})
        context.system.scheduler.schedule(0 seconds, 30 seconds) ({
          val random = new Random()
          val r = random.nextInt(friendlist.size-1)
          val friendid = friendlist(r)
          var result = pipeline(Get("http://localhost:8080/"+friendid+"/wallposts"))
          result.foreach { response =>
            var postMapnew = response.entity.asString
            var row = postMapnew.split("\\), \\(")
            var mapOfRow:ConcurrentHashMap[Int,(String,String)] = new ConcurrentHashMap[Int,(String,String)]()
            println("\nRow Length = " + row.length + "\n")
            if(row.length >0){
              for(i <- 0 until row.length){
                row(i) = row(i).replaceFirst("\\[\\(","")
                row(i) = row(i).replaceFirst("\\)\\]","")
                var parts:Array[String] = row(i).split(",")
                parts(0) = parts(0).replaceAll("_","=")
                parts(1) = parts(1).replaceAll("_","=") // error for part 1
                parts(0) = parts(0).replaceAll(" ","+")
                parts(1) = parts(1).replaceAll(" ","+")
                mapOfRow.put(i,(parts(0),parts(1)))
              }
            }

            implicit val timeout = Timeout (5 seconds)
            val future = context.actorSelection("../activeuser"+friendid.toString) ? sendPost(mapOfRow, publicKey)
            val friendPost = Await.result(future, timeout.duration).asInstanceOf[ConcurrentHashMap[Int,(String,String)]]

            if(friendPost.size() >0){
              for( i <- 0 until friendPost.size()){
                var key = decrypt(Base64.decodeBase64(friendPost.get(i)._2),privateKey)
                var posts = decryptAES(new String(key, "UTF-8"),initVector,Base64.decodeBase64(friendPost.get(i)._1) )
                println( " decrypted FRIEND's post should come " + new String(posts, "UTF-8"))
              }
            }

            println(s"Request completed with status ${response.status}")
          }})


        //request for friends posts
       /* context.system.scheduler.schedule(0 seconds, 30 seconds) ({
          val random = new Random()
          val r = random.nextInt(friendlist.size-1)
          val friendid = friendlist(r)

          implicit val timeout = Timeout (5 seconds)
          val future = context.actorSelection("../activeuser"+friendid.toString) ? "publickey"
          val friendPublicKey = Await.result(future, timeout.duration).asInstanceOf[PublicKey]
          var sendPostMap = new ConcurrentHashMap[String,(Array[Byte],Array[Byte])]
          if( postTemp.size() >0){
            for(i <- 0 until postTemp.size()){
              //println("Post passed  = " + postTemp.get(i.toString))
              //decryptAES(key, initVector,postTemp.get(i.toString))
              var decryptKey:Array[Byte] = decrypt(postTemp.get(i.toString)._2,privateKey)
             // var key:String = new String( decryptKey, "UTF-8")
              //println( " AES key of post id "+ i + " is " + key)
              var encryptedKey:Array[Byte] = encrypt(decryptKey,friendPublicKey)
              sendPostMap.put(i.toString, (postTemp.get(i.toString())._1, encryptedKey ))
              }
            val futurefriend = context.actorSelection("../activeuser"+friendid.toString) ? sendPost(sendPostMap)
            val friendPost = Await.result(futurefriend, timeout.duration).asInstanceOf[String]

          }
          /*var result = pipeline(Get("http://localhost:8080/"+userid+"/wallposts"))
          result.foreach { response =>
            var size = (response.entity.asString.replaceAll("[^0-9]", ""))
            //println(" user mapee  size " + size)
            //var mapee = new ConcurrentHashMap[String,String]
            //mapee = (userProfileMap.get(userid).wall.posts)
            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }*/})*/


        context.system.scheduler.schedule(0 seconds, 20 seconds) ({val random = new Random()
          val r = random.nextInt(friendlist.size-1)
          val friendid = friendlist(r)
          var postMsg:Array[Byte] = ("Hello"+userid.toString).getBytes("UTF-8")
          var key:String = SRNG()
          var encryptedwithAES:Array[Byte] = encryptAES(key, initVector, postMsg);
          implicit val timeout = Timeout (5 seconds)
          val future = context.actorSelection("../activeuser"+friendid.toString) ? "publickey"
          val friendPublicKey = Await.result(future, timeout.duration).asInstanceOf[PublicKey]

          var encryptedKey:Array[Byte] = encrypt(key.getBytes("UTF-8"),friendPublicKey)
          var encryptedString64 = Base64.encodeBase64String(encryptedwithAES)
          var encryptedKey64 = Base64.encodeBase64String(encryptedKey)
          encryptedString64 = encryptedString64.replaceAll("=", "_")
          encryptedKey64 = encryptedKey64.replaceAll("=", "_")

         // println( " encryptedString64 is  " + encryptedString64 + "  encryptedKey64 is  "+ encryptedKey64)
          //val futurefriend = context.actorSelection("../activeuser"+friendid.toString) ? addPost(encryptedwithAES,encryptedKey)
          //val friendPost = Await.result(futurefriend, timeout.duration).asInstanceOf[String]
          var result = Get("http://localhost:8080/" + userid.toString + "/wallposts/" + friendid.toString + s"/addpost?post=${encryptedString64}&key=${encryptedKey64}") ~>sendReceive
          result.foreach { response =>
            /*var size = (response.entity.asString.replaceAll("[^0-9]", ""))
            var intSize = 0
            if(size == ""){
              println(" added post size " + size)
              intSize = 0
            }
            else{
              println(" added post size " + size)
              intSize =  postTemp.size()
              //postTemp.put((intSize).toString, "Hello" + System.currentTimeMillis())
            }*/
            //println(" my wall post " + postTemp)

            println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }})


      }


}

  def encrypt (text:Array[Byte], key1:PublicKey) : Array[Byte] = {
    var cipherText: Array[Byte] = null
    val cipher: Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.ENCRYPT_MODE, key1)
    cipherText = cipher.doFinal(text);
    //println(" encrypted text " + cipherText);
    return cipherText;
  }

  def decrypt (text:Array[Byte], key2:PrivateKey) : Array[Byte] = {
    //println("Text is " + text)
    var cipherText:Array[Byte] = null
    val cipher:Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.DECRYPT_MODE,key2)
    cipherText = cipher.doFinal(text);

    return (cipherText)
  }

  def encryptAES(key:String, initVector:String , value:Array[Byte]):Array[Byte] = {
    var iv:IvParameterSpec = new IvParameterSpec(initVector.getBytes("UTF-8"));
    var skeySpec:SecretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES");
    var cipher:Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv);
    return cipher.doFinal(value);
  }

  def decryptAES(key:String , initVector: String , encrypted:Array[Byte]):Array[Byte] = {
    var iv:IvParameterSpec  = new IvParameterSpec(initVector.getBytes("UTF-8"));
    var skeySpec:SecretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES");
    var cipher:Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
    cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv);
    return cipher.doFinal(encrypted);
  }

}


trait FbApi extends HttpService with DefaultJsonProtocol{
 /* case class UserProfile(userid: Int, name: String, age: Int, dob: Int, wall: FbPost, picture: String,  var albumid: List[Int])
  case class ProfilePic(pic: File)
  case class FacebookFriends(var friendList: List[Int])
  case class FbPage(pageid: Int, name: String, admin: Int, var subscribers: List[Int], wall: FbPost)
  case class FbPost(var posts: ConcurrentHashMap[String, String])
  case class Album(photos: List[String])
  implicit def executionContext = actorRefFactory.dispatcher

  /*object fbFormat extends DefaultJsonProtocol {
    implicit val fbPostFormat = jsonFormat1(FbPost.apply)
    implicit val userProfileFormat = jsonFormat7(UserProfile.apply)
    implicit val facebookFriendsFormat = jsonFormat1(FacebookFriends.apply)
    implicit val albumFormat = jsonFormat1(Album.apply)
  }
*/
  var userProfileMap = new ConcurrentHashMap[Int, UserProfile]
  var friendMap = new ConcurrentHashMap[Int, FacebookFriends]
  var pageMap = new ConcurrentHashMap[Int, FbPage]
  var albumMap = new ConcurrentHashMap[Int, Album]
  var publicKeyMap = new ConcurrentHashMap[Int,PublicKey]
  var privateKeyMap = new ConcurrentHashMap[Int,PrivateKey]


  //create 10,000 users
  for (i <- 0 to 10) {


    val initialPost = FbPost(new ConcurrentHashMap())
    //initialPost.posts += (0.toString() -> "Hello")
    initialPost.posts.put(0.toString,"hi11")

    initialPost.posts.put(0.toString,"hi33")

    //ProfilePic(File.createTempFile("profilepic", ".jpg", new File("C:\\Users\\Pratyoush\\Desktop\\tmp")));
    val ftmp = File.createTempFile("upload", ".jpg", new File("C:\\Users\\Prakriti\\Desktop\\tmp"));
    userProfileMap.put(i, UserProfile(i, "UserProfile" + i.toString, i, i + 10, initialPost, ftmp.getAbsolutePath,
      List[Int]() ))
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
*/}
