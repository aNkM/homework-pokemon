package controllers

import javax.inject.Inject

import play.api.Logger
import play.api.cache._
import play.api.libs.json._
import play.api.mvc._
import shared._


class SearchController @Inject() (cache: CacheApi) extends Controller {

  def postSearch = Action(parse.json) { implicit req =>
    val listResults: Seq[(String, String)] = SharedPokemonList.get
      .map(res => (res._1, routes.PokemonController.index(res._1).toString))
      .sortWith(_._1 < _._1)

    val listResultsJson = SharedCollectionsTransformations.getJsonArray(
      SharedCollectionsTransformations.filterSubstring(listResults, req.body.toString).map(
        res => JsObject(Map("name" -> JsString(res._1), "url" -> JsString(res._2)))
      ).toList
    )

    Ok(listResultsJson)
  }

  def index = Action { implicit req =>
    SharedPokemonList.set(cache.getOrElse[Seq[(String,String)]]("pokemonList") {
      val list = SharedPokemonAPICalls.initializePokemonList
      cache.set("pokemonList", list)
      list
    })

    Ok(views.html.index(SharedPokemonList.get, routes.SearchController.postSearch))
  }

}
