package facebook.client

import java.util.concurrent.ConcurrentHashMap
import javax.crypto.spec.{SecretKeySpec, IvParameterSpec}

import org.apache.commons.codec.binary.Base64

import scala.concurrent.{Future, Await}
import akka.pattern.ask
import akka.actor._
import akka.util.Timeout
import spray.client.pipelining
import spray.client.pipelining._
import facebook.{SampleTrait}
import spray.routing.HttpService
import scala.util.Random
import spray.json._
import scala.concurrent.duration._

import javax.crypto._
import java.security._
;

case class addPost(encrypt:Array[Byte],encryptKey:Array[Byte])
case class sendPost(sendPostMap:ConcurrentHashMap[Int,(String,String)], pubKey:PublicKey)
case class requestPic(pubKey:PublicKey)

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

class ClientUser(pipeline: pipelining.SendReceive,userid: Int, friendlist: List[Int]) extends Actor{
  implicit val sys = context.system
  //override implicit def actorRefFactory: ActorRefFactory = sys
  import scala.concurrent.ExecutionContext.Implicits.global  //***check if this is actually needed or not
  //override implicit def executionContext = actorRefFactory.dispatcher
  def fbMD5(s: String): String = {
    // Besides "MD5", "SHA-256", and other hashes are available
    val m = java.security.MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8"))
    m.map("%02x".format(_)).mkString
  }



  def SRNG():String = {
    val random = new scala.util.Random(new java.security.SecureRandom())
    val random2 = new SecureRandom()
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
 var initVector:String = "RandomInitVector"; // 16 byte IV
  //val password = "user"+userid.toString;
  var loginStatus: String = ""

    def receive = {
      case "publickey" =>{
        sender ! publicKey
      }

      case addPost(encrypt:Array[Byte], encryptKey:Array[Byte]) =>{
        postTemp.put(postTemp.size().toString,(encrypt,encryptKey))
        sender !"Post added"
      }

      case sendPost(sendPostMap:ConcurrentHashMap[Int,(String, String)], pubKey:PublicKey) =>{
        var returnVal:ConcurrentHashMap[Int,(String,String)] = new ConcurrentHashMap[Int,(String,String)]()
        println ( " sendPostMap size " +sendPostMap.size())
        for( i <- 0 until sendPostMap.size()){
          println("sendPostMap.get(i)._2  " + i  +"  "+sendPostMap.get(i)._2)
          println("sendPostMap.get(i)._2  " + i  +"  "+Base64.decodeBase64(sendPostMap.get(i)._2))

          var key = decrypt(Base64.decodeBase64(sendPostMap.get(i)._2),privateKey)
         // var posts = decryptAES(new String(key, "UTF-8"),initVector,Base64.decodeBase64(mapOfRow.get(i)._1) )
          var key2: String= Base64.encodeBase64String(encrypt(key,pubKey))
          returnVal.put(i, (sendPostMap.get(i)._1, key2))
        }
        sender ! returnVal
      }

      case "low" => {

        var encodedPublicKey = Base64.encodeBase64URLSafeString(publicKey.getEncoded)

         val registerResult = Get("http://localhost:8080/"+userid.toString +s"/register?publickey=${encodedPublicKey}") ~> sendReceive
          registerResult.foreach { response =>
            //println(" public key at fb client is " + publicKey+ " for user "+ userid)
            println(s"User Registered!!!! Request completed with status ${response.status} and content: \n ${response.entity.asString}")
          }

        val loginResult = Get("http://localhost:8080/"+userid.toString +"/login") ~> sendReceive
        loginResult.foreach { response =>
          val secureString = response.entity.asString
          val encryptedSecureString:String = Base64.encodeBase64URLSafeString(DSEncrypt(secureString.getBytes("UTF-8"),privateKey))

          val loginVerifyResult = Get("http://localhost:8080/"+userid.toString +s"/loginverify?es=${encryptedSecureString}")~> sendReceive
          loginVerifyResult.foreach { response =>
              loginStatus = response.entity.asString
            //println(" before print " + loginStatus + " boolean " + loginStatus.equalsIgnoreCase("loginsuccessful"))
          }
          println(s"User Login!!!! Request completed with status ${response.status} and content: \n ${response.entity.asString}")
        }

        Thread.sleep(10000L)
        //println(" print " + loginStatus + " boolean " + loginStatus.equalsIgnoreCase("loginsuccessful"))
        //rest of the activities

        if(loginStatus.equalsIgnoreCase("loginsuccessful")) {
        println("Inside Login")
          context.system.scheduler.schedule(0 seconds, 20 seconds) ({var result = Get("http://localhost:8080/"+userid+"/profile") ~> sendReceive
            result.foreach { response =>
              println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
            }
          })
          context.system.scheduler.schedule(0 seconds, 30 seconds)({
            val random = new Random()
            val r = random.nextInt(friendlist.size - 1)
            var result = Get("http://localhost:8080/" + friendlist(r).toString + "/profile") ~> sendReceive
            result.foreach { response =>
              println(s"Friends Profile!!!! Request completed with status ${response.status} and content: \n ${response.entity.asString}")
            }
          })

          context.system.scheduler.schedule(0 seconds, 20 seconds) ({
            if(userid == 9 ){
            var result = Get("http://localhost:8080/9/getprofilepic") ~> sendReceive // add param for friendid
            result.foreach { response =>
              var getData = response.entity.asString
              var getPicKeyPair = getData.split("\\),\\(")
              var decodedKey = Base64.decodeBase64(getPicKeyPair(1))
              println(" decode key " + getPicKeyPair(1))
              if( !getPicKeyPair(1).toString.equals("key")){
                var key = new String((decrypt(decodedKey,privateKey)), "UTF-8")
                var decodedPicture = Base64.decodeBase64(getPicKeyPair(0))
                var decryptedPicture = decryptAES(key, initVector, decodedPicture)
                println(" 0   pic########### "+ userid+ " key " + key)
                println(" 1   pic########### " + new String(decryptedPicture, "UTF-8"))
              }
              }
            }
          })

          context.system.scheduler.schedule(0 seconds, 30 seconds)({
            var result = pipeline(Get("http://localhost:8080/" + userid + "/wallposts"))
            result.foreach { response =>
              var postMapnew = response.entity.asString
              var row = postMapnew.split("\\), \\(")
              var mapOfRow: ConcurrentHashMap[Int, (String, String)] = new ConcurrentHashMap[Int, (String, String)]()
              println("\nRow Length = " + row.length + "\n")
              if (row.length > 1) {
                for (i <- 0 until row.length) {
                  row(i) = row(i).replaceFirst("\\[\\(", "")
                  row(i) = row(i).replaceFirst("\\)\\]", "")
                  println( " row i "+ i + " " + row(i))
                  var parts: Array[String] = row(i).split(",")
                  println( " parts length " + parts.length)
                  parts(0) = parts(0).replaceAll("_", "=")
                  parts(1) = parts(1).replaceAll("_", "=") // error for part 1
                  parts(0) = parts(0).replaceAll(" ", "+")
                  parts(1) = parts(1).replaceAll(" ", "+")
                  println ( " part0 " +  parts(0) + " parts1 "+ parts(1) )
                  mapOfRow.put(i, (parts(0), parts(1)))
                }
              }
              case class post(var post:String)
              var postMap: Map[String, post] = Map()
              object mapJsonFormat extends DefaultJsonProtocol {
                implicit val fbPostFormat = jsonFormat1(post.apply)
              }
              import mapJsonFormat._
              if (mapOfRow.size() > 1) {
                for (i <- 0 until mapOfRow.size()) {
                  println( " encoded key " + mapOfRow.get(i)._2)
                  println( " decoded key " + Base64.decodeBase64(mapOfRow.get(i)._2) )
                  println( " map of row at i "+ i + mapOfRow.get(i)._2)
                  var key = decrypt(Base64.decodeBase64(mapOfRow.get(i)._2), privateKey)
                  println( " key here " + key)
                  var posts = decryptAES(new String(key, "UTF-8"), initVector, Base64.decodeBase64(mapOfRow.get(i)._1))

                  postMap += (i.toString -> post(new String(posts, "UTF-8")))
                  //println(" My Wall Posts!! " + new String(posts, "UTF-8"))
                }
                println ( " Wall!! " + postMap.toJson.prettyPrint)
              }

              println(s"Request completed with status ${response.status}")
            }
          })
          context.system.scheduler.schedule(0 seconds, 30 seconds)({
            val random = new Random()
            val r = random.nextInt(friendlist.size - 1)
            val friendid = friendlist(r)

            var result = pipeline(Get("http://localhost:8080/" + friendid + "/wallposts"))
            result.foreach { response =>
              var postMapnew = response.entity.asString
              var row = postMapnew.split("\\), \\(")

              var mapOfRow: ConcurrentHashMap[Int, (String, String)] = new ConcurrentHashMap[Int, (String, String)]()

              //println("\nRow Length = " + row.length + "\n")

              if (row.length > 1) {
                for (i <- 0 until row.length) {
                  row(i) = row(i).replaceFirst("\\[\\(", "")
                  row(i) = row(i).replaceFirst("\\)\\]", "")
                  var parts: Array[String] = row(i).split(",")
                  parts(0) = parts(0).replaceAll("_", "=")
                  parts(1) = parts(1).replaceAll("_", "=") // error for part 1
                  parts(0) = parts(0).replaceAll(" ", "+")
                  parts(1) = parts(1).replaceAll(" ", "+")
                  println ( " Parts0 friends " + parts(0) + " Parts1 friend" + parts(1))
                  mapOfRow.put(i, (parts(0), parts(1)))
                }
              }

              implicit val timeout = Timeout(15 seconds)

              val future = context.actorSelection("../activeuser" + friendid.toString) ? sendPost(mapOfRow, publicKey)
              val friendPost = Await.result(future, timeout.duration).asInstanceOf[ConcurrentHashMap[Int, (String, String)]]

              if (friendPost.size() > 0) {
                for (i <- 0 until friendPost.size()) {
                  var key = decrypt(Base64.decodeBase64(friendPost.get(i)._2), privateKey) //226 error
                  var posts = decryptAES(new String(key, "UTF-8"), initVector, Base64.decodeBase64(friendPost.get(i)._1))
                  println(" My Friends Wall Posts!! " + new String(posts, "UTF-8"))
                }
              }

              println(s"Request completed with status ${response.status}")
            }
          })

          context.system.scheduler.schedule(0 seconds, 20 seconds)({
            val random = new Random()
            val r = random.nextInt(friendlist.size - 1)
            val friendid = friendlist(r)
            var postMsg: Array[Byte] = ("Hello" + userid.toString).getBytes("UTF-8")
            var key: String = SRNG()
            var encryptedwithAES: Array[Byte] = encryptAES(key, initVector, postMsg);
            implicit val timeout = Timeout(5 seconds)
            val future = context.actorSelection("../activeuser" + friendid.toString) ? "publickey" // asking for public key from actor
            val friendPublicKey = Await.result(future, timeout.duration).asInstanceOf[PublicKey]
            println(" friend's public key " + friendPublicKey)
            var encryptedKey: Array[Byte] = encrypt(key.getBytes("UTF-8"), friendPublicKey)
            var encryptedString64 = Base64.encodeBase64String(encryptedwithAES)
            var encryptedKey64 = Base64.encodeBase64String(encryptedKey)
            encryptedString64 = encryptedString64.replaceAll("=", "_")
            encryptedKey64 = encryptedKey64.replaceAll("=", "_")

            // println( " encryptedString64 is  " + encryptedString64 + "  encryptedKey64 is  "+ encryptedKey64)
            //val futurefriend = context.actorSelection("../activeuser"+friendid.toString) ? addPost(encryptedwithAES,encryptedKey)
            //val friendPost = Await.result(futurefriend, timeout.duration).asInstanceOf[String]
            var result = Get("http://localhost:8080/" + userid.toString + "/wallposts/" + friendid.toString + s"/addpost?post=${encryptedString64}&key=${encryptedKey64}") ~> sendReceive
            result.foreach { response =>
              println(s"Request completed with status ${response.status} and content: \n ${response.entity.asString}")
            }
          })

        }
      }
}

  //Digital Signature
  def DSEncrypt (text:Array[Byte], key1:PrivateKey) : Array[Byte] = {
    var cipherText: Array[Byte] = null
    val cipher: Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.ENCRYPT_MODE, key1)
    cipherText = cipher.doFinal(text);
    //println(" encrypted text " + cipherText);
    return cipherText;
  }


  //RSA Encryption
  def encrypt (text:Array[Byte], key1:PublicKey) : Array[Byte] = {
    var cipherText: Array[Byte] = null
    val cipher: Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.ENCRYPT_MODE, key1)
    cipherText = cipher.doFinal(text);
    //println(" encrypted text " + cipherText);
    return cipherText;
  }

  //RSA Decryption
  def decrypt (text:Array[Byte], key2:PrivateKey) : Array[Byte] = {
    //println("Text is " + text)
    var cipherText:Array[Byte] = null
    val cipher:Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.DECRYPT_MODE,key2)
    cipherText = cipher.doFinal(text);

    return (cipherText)
  }

  //AES Encryption
  def encryptAES(key:String, initVector:String , value:Array[Byte]):Array[Byte] = {
    var iv:IvParameterSpec = new IvParameterSpec(initVector.getBytes("UTF-8"));
    var skeySpec:SecretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES");
    var cipher:Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv);
    return cipher.doFinal(value);
  }

  //AES DEcryption
  def decryptAES(key:String , initVector: String , encrypted:Array[Byte]):Array[Byte] = {
    var iv:IvParameterSpec  = new IvParameterSpec(initVector.getBytes("UTF-8"));
    var skeySpec:SecretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES");
    var cipher:Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
    cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv);
    return cipher.doFinal(encrypted);
  }

}


