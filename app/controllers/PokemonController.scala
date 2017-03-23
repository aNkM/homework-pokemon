package controllers


import javax.inject.Inject

import play.api.Logger
import play.api.cache._
import play.api.libs.json.{JsArray, JsObject, JsValue}
import play.api.mvc._
import shared._


class PokemonController @Inject() (cache: CacheApi) extends Controller {

  def postTweets = Action(parse.json) { implicit req =>
    Ok(SharedTwitterAPICalls.getLastTweets(req.body.toString()))
  }

  // Name, URL, Slot/Base_stat
  def parsePokemonWithParameter(param: String, param2: String, types: JsArray): Seq[((String, String), Int)] = {
    (types \\ param).map(x => ( (x \ "name").as[String], (x \ "url").as[String] ))
      .zip( (types \\ param2).map(x => x.as[Int]) )
  }

  def index(name: String) = Action {
    SharedPokemonList.set(cache.getOrElse[Seq[(String,String)]]("pokemonList") {
      val list = SharedPokemonAPICalls.initializePokemonList
      cache.set("pokemonList", list)
      list
    })

    val foundPokemons = SharedPokemonList.get.filter(x => x._1 == name)
    if (foundPokemons == Seq.empty[(String, String)])
      NotFound(views.html.notFoundPage(name))
    else {
      val foundPokemon = foundPokemons.head
      val result: JsValue = SharedPokemonAPICalls.getPokemonById(foundPokemon._2).as[JsObject] - "moves"
      SharedPokemonActive.set(result)

      Ok(views.html.pokemon(
        (result \ "sprites" \ "front_default").as[String],
        (result \ "name").as[String],
        (result \ "base_experience").as[Int],
        parsePokemonWithParameter("type", "slot", (result \ "types").as[JsArray] ),
        parsePokemonWithParameter("ability", "slot", (result \ "abilities").as[JsArray] ),
        parsePokemonWithParameter("stat", "base_stat", (result \ "stats").as[JsArray] ),
        SharedTwitterAPICalls.getLastTweets((result \ "name").as[String]),
        60000,
        routes.PokemonController.postTweets
      ))
    }
  }

}
