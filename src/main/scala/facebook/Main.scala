package facebook

import java.security._
import java.util
import java.util.concurrent.ConcurrentHashMap
import javax.crypto.{SecretKey, KeyGenerator, Cipher}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import akka.actor.{ActorSelection, Props, ActorSystem}
import akka.io.IO

import scala.util.Random

//import akka.parboiled2.util.Base64
import com.typesafe.config.ConfigFactory
import spray.can.Http
import spray.routing._
import java.security.MessageDigest
import java.util
import org.apache.commons.codec.binary.Base64
import scala.collection.convert.Wrappers.ConcurrentMapWrapper
import scala.collection.immutable._


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






/*


  val keys:KeyPairGenerator = KeyPairGenerator.getInstance("RSA");
  keys.initialize(2048);
  val keypair:KeyPair = keys.generateKeyPair
  val privateKey:PrivateKey = keypair.getPrivate;
  val publicKey:PublicKey = keypair.getPublic;
  println("keys are " + privateKey + "   anddddd     " + publicKey);

  var messMap = new ConcurrentHashMap[Int,Array[Byte]]

  for( i <- 1 to 2){
    val mess1:Array[Byte] = ("hello"+i.toString).getBytes("UTF8")
    val sec:Array[Byte] = encrypt(mess1,publicKey)
    messMap.put(i,sec)

  }

  /*val msg:String = "bye";
  val m:Array[Byte] = Array[Byte](1,2,3)
  for( i <- 0 until msg.length)
    m(i) = msg

  println(" array is " + m)
*/
 for( i <- 1 to 2){
  val res = decrypt(messMap.get(i),privateKey)
    println(" key is " + new String(res, "UTF8") )
  }
  /*val  message:Array[Byte] = "Hello World new ".getBytes("UTF8");
  val secret:Array[Byte] = encrypt(message, publicKey);
  val recovered_message:Array[Byte] = decrypt(secret, privateKey);
  println("  hh    " + new String(recovered_message, "UTF8"));
*/
  def encrypt (text:Array[Byte], key1:PublicKey) : Array[Byte] = {
    var cipherText: Array[Byte] = null
    val cipher: Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.ENCRYPT_MODE, key1)
    cipherText = cipher.doFinal(text);
    printf(" encrypted text " + cipherText);
    return cipherText;
  }

  def decrypt (text:Array[Byte], key2:PrivateKey) : Array[Byte] = {
    var cipherText:Array[Byte] = null
    val cipher:Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.DECRYPT_MODE,key2)
    cipherText = cipher.doFinal(text);

    return (cipherText)
  }

  /*var msg:String = "hello to you"
  var encryptedmsg:Array[Byte] = encrypt(msg,publicKey);
  var res = decrypt(encryptedmsg,privateKey)
  */println("bye " + "   there ");

*/
  /*var encryptionKey:String = "MZygpewJsCpRrfOr";
  var plainText:String = "Hello world!hgjghhjj";

  var cipherText:String = encrypt(plainText);
  var decryptedCipherText:String = decrypt(cipherText);

  println(" plain " + plainText);
  println(" Cypher " + cipherText);
  println(" decrypted " + decryptedCipherText);

  def encrypt(plainText:String ):String = {
    var cipher:Cipher = getCipher(Cipher.ENCRYPT_MODE);
    var encryptedBytes:Array[Byte] = cipher.doFinal(plainText.getBytes());

    return Base64.encodeBase64String(encryptedBytes);
  }

  def decrypt(encrypted:String):String = {
    var cipher:Cipher = getCipher(Cipher.DECRYPT_MODE);
    var plainBytes:Array[Byte] = cipher.doFinal(Base64.decodeBase64(encrypted));

    return new String(plainBytes);
  }

  def getCipher(cipherMode:Int):Cipher = {
    var encryptionAlgorithm:String  = "AES";
    var keySpecification:SecretKeySpec = new SecretKeySpec(encryptionKey.getBytes("UTF-8"), encryptionAlgorithm);
    var cipher:Cipher = Cipher.getInstance(encryptionAlgorithm);
    cipher.init(cipherMode, keySpecification);
    return cipher;
  }
*/
  /*var key:String = "Bar12345Bar12345"; // 16 byte key
  var initVector:String = "RandomInitVector"; // 16 byte IV
  var encryptedBytes:Array[Byte] = encrypt(key, initVector, new String("Hello  gete World").getBytes());

  println(" final string " + new String(decrypt(key, initVector, encryptedBytes)));

  def encrypt(key:String, initVector:String , value:Array[Byte]):Array[Byte] = {
    var iv:IvParameterSpec = new IvParameterSpec(initVector.getBytes("UTF-8"));
    var skeySpec:SecretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES");
    var cipher:Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv);
    return cipher.doFinal(value);
  }

  def decrypt(key:String , initVector: String , encrypted:Array[Byte]):Array[Byte] = {

    var iv:IvParameterSpec  = new IvParameterSpec(initVector.getBytes("UTF-8"));
    var skeySpec:SecretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES");

    var cipher:Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
    cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv);

    return cipher.doFinal(encrypted);
  }*/

  /*val keys:KeyPairGenerator = KeyPairGenerator.getInstance("RSA");
  keys.initialize(2048);
  val keypair:KeyPair = keys.generateKeyPair
  val privateKey:PrivateKey = keypair.getPrivate;
  val publicKey:PublicKey = keypair.getPublic;
  /*val keypair1:KeyPair = keys.generateKeyPair
  val privateKey1:PrivateKey = keypair1.getPrivate;
  val publicKey1:PublicKey = keypair1.getPublic;
*/
  println("keys are " + privateKey + "   anddddd     " + publicKey);


  def encrypt (text:Array[Byte], key1:PublicKey) : Array[Byte] = {
    var cipherText: Array[Byte] = null
    val cipher: Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.ENCRYPT_MODE, key1)
    cipherText = cipher.doFinal(text);
    println(" encrypted text " + cipherText);
    return cipherText;
  }

  def decrypt (text:Array[Byte], key2:PrivateKey) : Array[Byte] = {
    var cipherText:Array[Byte] = null
    val cipher:Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.DECRYPT_MODE,key2)
    cipherText = cipher.doFinal(text);

    return (cipherText)
  }

  var key:String = "Bar12345Bar12345"; // 16 byte key
  var initVector:String = "RandomInitVector"; // 16 byte IV
  var encryptedBytes:Array[Byte] = encryptAES(key, initVector, new String("Hello  gete World").getBytes());
  println( " encripted wirh aes " + encryptedBytes)
  //println(" final string  is " + new String(decryptAES(key, initVector, encryptedBytes)));
  var resRSA:Array[Byte] = encrypt(encryptedBytes,publicKey)
  println( " encripted with rsa " + resRSA)
  var deresRSA:Array[Byte] = decrypt(resRSA,privateKey)
  println(" decrypt with rsa " + deresRSA)
  println( " string back " +new String(decryptAES(key, initVector,deresRSA), "UTF-8"))
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
*/

  def SRNG():String = {
    val random = new scala.util.Random(new java.security.SecureRandom())
    var key:Array[Char] = Array[Char](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    for(i<- 0 until 16){
      key(i) = random.nextPrintableChar()
    }

    return key.mkString("")
  }
  for( i <- 0 to 5)
  println( " key is " + SRNG())
}





/*var encryptedBytes:Array[Byte] = encryptAES(key, initVector, new String("Hello  gete World").getBytes());
println( " encripted wirh aes " + encryptedBytes)
//println(" final string  is " + new String(decryptAES(key, initVector, encryptedBytes)));
var resRSA:Array[Byte] = encrypt(encryptedBytes,publicKey)
println( " encripted with rsa " + resRSA)
var deresRSA:Array[Byte] = decrypt(resRSA,privateKey)
println(" decrypt with rsa " + deresRSA)
println( " string back " +new String(decryptAES(key, initVector,deresRSA), "UTF-8"))*/
