package shared

import javax.inject.Singleton

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import play.api.Logger
import play.api.libs.json._
import play.api.libs.ws.ahc.AhcWSClient
import play.api.libs.ws.{WSClient, WSResponse}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global


@Singleton
object SharedCollectionsTransformations {

  def getJsonArray(list: List[JsObject]): JsArray = {
    list.foldLeft(JsArray())((acc, x) => acc ++ Json.arr(x))
  }

  def filterSubstring(listStr: Seq[(String, String)], subStr: String): Seq[(String, String)] = {
    val toRemove = "\"".toSet
    val subStrFiltered = subStr.filterNot(toRemove)

    listStr.filter(_._1.contains(subStrFiltered))
  }
}

@Singleton
object SharedAPIClient {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  private val ws: WSClient = AhcWSClient()

  def get : WSClient = ws
}

@Singleton
object SharedPokemonAPICalls {

  def initializePokemonList: Seq[(String, String)] = {
    getAllPokemon.value.map(v => {
      ((v \ "name").as[String], (v \ "url").as[String])
    })
  }

  def getAllPokemon: JsArray = {
    Logger.info("PokeAPI call to get all pokemons : " + SharedPokemonURL.get + "pokemon/")
    val futureResponse: Future[Int] =
      SharedAPIClient.get.url(SharedPokemonURL.get + "pokemon/").withFollowRedirects(true).get().map {
        response => (response.json \ "count").as[Int]
      }
    val count : Int = Await.result(futureResponse, Duration.Inf)
    var nbOfRequests : Int = count / 20
    if (count % 20 == 0) nbOfRequests -= 1

    (0 to nbOfRequests).map(_*20).map(offset => {
      SharedAPIClient.get.url(SharedPokemonURL.get + "pokemon/")
        .withFollowRedirects(true)
        .withQueryString("offset" -> offset.toString).get()
        .map { response =>
          (response.json \ "results").as[JsArray]
        }
    }).map(future => {
      Await.result(future, Duration.Inf)
    }).reduce(_ ++ _)
  }

  def getPokemonById(pokeURL: String): JsValue = {
    val futureResponse: Future[WSResponse] =
      SharedAPIClient.get.url(pokeURL).withFollowRedirects(true).get()
    Await.result(futureResponse, Duration.Inf).json
  }
}

@Singleton
object SharedPokemonList {
  private var pokemonList: Seq[(String,String)] = Seq.empty[(String, String)]

  def set(newList: Seq[(String, String)]) : Unit = pokemonList = newList
  def get : Seq[(String, String)] = pokemonList
  def getWithNames : Seq[String] = pokemonList.map(tuple => tuple._1)
  def isDefined : Boolean = pokemonList != Seq.empty[(String, String)]
}

@Singleton
object SharedPokemonActive {
  private var pokemonActive: JsValue = JsNull

  def get : JsValue = pokemonActive
  def set(newActive: JsValue): Unit = pokemonActive = newActive
  def isDefined : Boolean = pokemonActive != JsNull
}

@Singleton
object SharedPokemonURL {
  private val pokemonURL: String = "http://pokeapi.co/api/v2/"

  def get : String = pokemonURL
}

@Singleton
object SharedTwitterAPICalls {

  def getLastTweets(tag: String) : JsArray = {
    val toRemove = "\"".toSet
    val tagFiltered = tag.filterNot(toRemove)

    var accessToken = SharedTwitterAccessToken.get
    if (!SharedTwitterAccessToken.isDefined) {
      val newAccessToken = SharedTwitterAPICalls.getAccessToken
      SharedTwitterAccessToken.set(newAccessToken)
      accessToken = newAccessToken
    }

    val futureResponse = SharedAPIClient.get.url(SharedTwitterURL.get + "1.1/search/tweets.json")
      .withFollowRedirects(true)
      .withHeaders(
        "Authorization" -> s"Bearer $accessToken"
      ).withQueryString(
        "q" -> s"%23$tagFiltered",
        "result_type" -> "latest",
        "lang" -> "en",
        "count" -> "10"
      ).get().map { response =>
        (response.json \ "statuses").as[JsArray]
      }
    Logger.info("Updated last Tweets from "+tagFiltered)
    Await.result(futureResponse, Duration.Inf)
  }

  def getAccessToken: String = {
    val base64 = SharedTwitterConsumerBase64.get
    val futureResponse = SharedAPIClient.get.url(SharedTwitterURL.get + "oauth2/token")
      .withHeaders(
        "Authorization" -> s"Basic $base64",
        "Content-Type" -> "application/x-www-form-urlencoded;charset=UTF-8"
      ).withFollowRedirects(true).post("grant_type=client_credentials")
      .map { response =>
        (response.json \ "access_token").as[String]
      }
    Await.result(futureResponse, Duration.Inf)
  }
}

@Singleton
object SharedTwitterAccessToken {
  private var token = ""

  def get : String = token
  def set(newToken: String): Unit = token = newToken
  def isDefined : Boolean = token != ""
}

@Singleton
object SharedTwitterConsumerBase64 {
  private val encodedBase64 = "RWphb0RyMlB1U0JVYkJFQklrb2tXQmVCNjo3RlRBaVVUaXd0MEx0QjlhT0JHSDNQRDNTc1RMb0MxM1YzWFY2NzJjTHpMdm0yVUJaMg=="

  def get : String = encodedBase64
}

@Singleton
object SharedTwitterURL {
  private val twitterURL : String = "https://api.twitter.com/"

  def get : String = twitterURL
}