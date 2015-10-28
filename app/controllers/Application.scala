package controllers

import java.io.{BufferedOutputStream, FileOutputStream}
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import play.api._
import play.api.data.Form
import scala.concurrent.{Await, Future}

import play.api.libs.json._
import play.api.mvc.AnyContent
import play.api.libs.ws.{WS, WSRequest, WSClient}
import play.api.mvc._
import play.api.Play.current
import scala.concurrent.{Future, Await}
import scala.collection.immutable.ListMap
import scala.concurrent.duration._
import play.api.libs.ws.WS
import play.api.libs.functional.syntax._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Failure, Success}
import scala.concurrent._
import ExecutionContext.Implicits.global


case class entityobj(name:String,vFrequency: String,strength:String, frequency: Double)


class Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready.", Nil,"",-1))
  }

  def display1 = Action {
    Ok(views.html.rohith("Your new application is ready.", Nil,"",-1))
  }

  def display2 = Action {
    Ok(views.html.ved("Your new application is ready.", Nil,"",-1))
  }

  def display3 = Action {
    Ok(views.html.sarthak("Your new application is ready.", Nil,"",-1))
  }

  import play.api.data.Forms._
  val newsForm = Form(
    single("news" -> text)
  )


  def getVertices1(item: String,fFilter: Double) = Action.async {implicit request =>
    //    val newsItem = newsForm.bindFromRequest().get
    val newsItem = item.toLowerCase()
    //val newsItem = URLEncoder.encode(item,"UTF-8")
    var freq = ""

//    for {
//      a <- WS.url("http://107.167.178.97:8282/graphs/titanconnected/vertices?key=name&value=" + newsItem).get()
//      b = (a.json \ "results").as[JsArray].value
//      c = b.map(res => (res \\ "frequency")).toString()
//      d = b.map(res => (res \ "_id").get.toString()) match {
//        case Nil => None
//        case string: Seq[String] => Some(string.head)
//      }
//      e <- WS.url("http://107.167.178.97:8282/graphs/titanconnected/vertices/" + d.get + "/outE?_label=Relation").get()
//      f <- (e.json \ "results").as[JsArray].value
//      g <- WS.url("http://107.167.178.97:8282/graphs/titanconnected/vertices/" + d.get + "/outE?_label=To").get()
//      h <- (g.json \ "results").as[JsArray].value
//      i <-
//
//
//    }
   val futureResult: Future[Option[String]] = WS.url("http://localhost:8282/graphs/titanexample/vertices?key=name&value=" + newsItem).get().map {
      response => {
        val temp = (response.json \ "results").as[JsArray].value
        freq = temp.map(res => (res \\ "frequency")).toString()
        println(freq)
        val id = temp.map(res => (res \ "_id").get.toString()) match {
          case Nil => None
          case string: Seq[String] => Some(string.head)
        }
        val both = (id,freq)
        id
      }
    }

    var data: Seq[Seq[JsValue]] = Seq()

    val future: Future[Seq[Seq[JsValue]]]= futureResult.flatMap {

      case None => Future(Seq[Seq[JsValue]]())
      //case None => Future(Seq[JsValue]())
      case Some(a) =>

        val finaldata = {

          val output = WS.url("http://localhost:8282/graphs/titanexample/vertices/" + a + "/outE?_label=relation").get().map {
            result => {
              (result.json \ "results").as[JsArray].value
            }
          }

          val outputtoLabel = WS.url("http://localhost:8282/graphs/titanexample/vertices/" + a + "/outE?_label=TO").get().map{
            result => {
              (result.json \ "results").as[JsArray].value
            }
          }

          val relationseq = output.map{x => {
            data = data :+ x
            //println(data)
            data
          }
          }

          val bothseq = relationseq.flatMap(count => {
            val gotonevalue = outputtoLabel.map{y => {
              data = data :+ y
              data
            }
            }
            gotonevalue
          })


          //        val either1 = output.map {
          //          case e: JsResultException => {
          //            Left(e)
          //            Redirect(routes.Application.index)
          //                        Redirect("/")
          //            Ok(views.html.index("Your new application is ready.", Nil, "", -1))
          //          }
          //          case result: Seq[JsValue] =>
          //            Right(result)
          //        }

          val recover = output.recover{
            case e: JsResultException =>
              None
            //Redirect("http://google.com")

            case js: Future[Seq[JsValue]] => Some(js)
          }

          bothseq
        }
        finaldata
    }

    val response2 = future.map(
      obj => {
        val check = obj.map(error1 => {

          error1.map(error2 => {
            val strength = (error2 \ "strength").get.toString()
            val frequency = (error2 \ "frequency").get.toString().toDouble
            val inv = (error2 \ "_inV").get.toString()

            val tuple = (inv,(frequency,strength))

            tuple
          })
        })
      //check.foreach().groupBy()

        val filteredresults = check.map(temp => {
          val   afilter = temp.filter(x => {
            x._2._1 > fFilter
          })

          val results = afilter.take(100)

          results
        })

        //      val filteredresults = check.filter(temp => {
        //        temp._2._1 > fFilter
        //      })

        //val take = filteredresults.take(100)
        //take

        filteredresults
      })


    val now = response2.flatMap{ x => {
      //       temp.foreach(x => println(x))
      val dataToSend = x.map { temp => {
        val necessary = temp.map(key => {

          val getName = key._1

          //println(key._1)

          val check = WS.url("http://localhost:8282/graphs/titanexample/vertices/" + getName).get().map {
            res => {
             val temp =  (res.json \ "results").get
              val name = (temp \ "name").get.toString()
              val vertexFrequency = (temp \ "frequency").get.toString()

              (name,vertexFrequency)
            }
          }

          val finalTuple = check.map(name => entityobj(name._1,name._2, key._2._2, key._2._1))
          finalTuple
        })

        Future.sequence(necessary)
      }
      }
      Future.sequence(dataToSend)
    }
    }

    now.map {
      case result: Seq[Seq[entityobj]] =>
        //println("result check  "+result)
        Ok(views.html.rohith("Showing data in table", result,item,fFilter))
    }
  }


  def getVertices2(item: String,fFilter: Double) = Action.async {implicit request =>
    //    val newsItem = newsForm.bindFromRequest().get
    val newsItem = item.toLowerCase()
    //val newsItem = URLEncoder.encode(item,"UTF-8")
    var freq = ""

    val futureResult: Future[Option[String]] = WS.url("http://localhost:8282/graphs/titanconnected/vertices?key=name&value=" + newsItem).get().map {
      response => {
        val temp = (response.json \ "results").as[JsArray].value
        freq = temp.map(res => (res \\ "frequency")).toString()
        println(freq)
        val id = temp.map(res => (res \ "_id").get.toString()) match {
          case Nil => None
          case string: Seq[String] => Some(string.head)
        }
        val both = (id,freq)
        id
      }
    }

    var data: Seq[Seq[JsValue]] = Seq()

    val future: Future[Seq[Seq[JsValue]]]= futureResult.flatMap {

      case None => Future(Seq[Seq[JsValue]]())
      //case None => Future(Seq[JsValue]())
      case Some(a) =>

        val finaldata = {

          val output = WS.url("http://localhost:8282/graphs/titanconnected/vertices/" + a + "/outE?_label=Relation").get().map {
            result => {
              (result.json \ "results").as[JsArray].value
            }
          }

          val outputtoLabel = WS.url("http://localhost:8282/graphs/titanconnected/vertices/" + a + "/outE?_label=To").get().map{
            result => {
              (result.json \ "results").as[JsArray].value
            }
          }

          val relationseq = output.map{x => {
            data = data :+ x
            //println(data)
            data
          }
          }

          val bothseq = relationseq.flatMap(count => {
            val gotonevalue = outputtoLabel.map{y => {
              data = data :+ y
              data
            }
            }
            gotonevalue
          })


          //        val either1 = output.map {
          //          case e: JsResultException => {
          //            Left(e)
          //            Redirect(routes.Application.index)
          //                        Redirect("/")
          //            Ok(views.html.index("Your new application is ready.", Nil, "", -1))
          //          }
          //          case result: Seq[JsValue] =>
          //            Right(result)
          //        }

          val recover = output.recover{
            case e: JsResultException =>
              None
            //Redirect("http://google.com")

            case js: Future[Seq[JsValue]] => Some(js)
          }

          bothseq
        }
        finaldata
    }

    val response2 = future.map(
      obj => {
        val check = obj.map(error1 => {

          error1.map(error2 => {
            val strength = (error2 \ "strength").get.toString()
            val frequency = (error2 \ "frequency").get.toString().toDouble
            val inv = (error2 \ "_inV").get.toString()

            val tuple = (inv,(frequency,strength))

            tuple
          })
        })
        //check.foreach().groupBy()

        val filteredresults = check.map(temp => {
          val   afilter = temp.filter(x => {
            x._2._1 > fFilter
          })

          val results = afilter.take(100)

          results
        })
        filteredresults
      })


    val now = response2.flatMap{ x => {
      val dataToSend = x.map { temp => {
        val necessary = temp.map(key => {

          val getName = key._1

          val check = WS.url("http://localhost:8282/graphs/titanconnected/vertices/" + getName).get().map {
            res => {
              val temp =  (res.json \ "results").get
              val name = (temp \ "name").get.toString()
              val vertexFrequency = (temp \ "frequency").get.toString()

              (name,vertexFrequency)
            }
          }

          val finalTuple = check.map(name => entityobj(name._1,name._2, key._2._2, key._2._1))
          finalTuple
        })

        Future.sequence(necessary)
      }
      }
      Future.sequence(dataToSend)
    }
    }

    now.map {
      case result: Seq[Seq[entityobj]] =>
        Ok(views.html.ved("Showing data in table", result,item,fFilter))
    }
  }

  def getVertices3(item: String,fFilter: Double) = Action.async {implicit request =>

    val newsItem = item.toLowerCase()

    var freq = ""

    val futureResult: Future[Option[String]] = WS.url("http://localhost:8282/graphs/sarthakretail/vertices?key=name&value=" + newsItem).get().map {
      response => {
        val temp = (response.json \ "results").as[JsArray].value
        freq = temp.map(res => (res \\ "frequency")).toString()
        println(freq)
        val id = temp.map(res => (res \ "_id").get.toString()) match {
          case Nil => None
          case string: Seq[String] => Some(string.head)
        }
        val both = (id,freq)
        id
      }
    }

    var data: Seq[Seq[JsValue]] = Seq()

    val future: Future[Seq[Seq[JsValue]]]= futureResult.flatMap {

      case None => Future(Seq[Seq[JsValue]]())
      //case None => Future(Seq[JsValue]())
      case Some(a) =>

        val finaldata = {

          val output = WS.url("http://localhost:8282/graphs/sarthakretail/vertices/" + a + "/outE?_label=relation").get().map {
            result => {
              (result.json \ "results").as[JsArray].value
            }
          }

          val outputtoLabel = WS.url("http://localhost:8282/graphs/sarthakretail/vertices/" + a + "/outE?_label=To").get().map{
            result => {
              (result.json \ "results").as[JsArray].value
            }
          }

          val relationseq = output.map{x => {
            data = data :+ x
            //println(data)
            data
          }
          }

          val bothseq = relationseq.flatMap(count => {
            val gotonevalue = outputtoLabel.map{y => {
              data = data :+ y
              data
            }
            }
            gotonevalue
          })

          val recover = output.recover{
            case e: JsResultException =>
              None
            //Redirect("http://google.com")

            case js: Future[Seq[JsValue]] => Some(js)
          }

          bothseq
        }
        finaldata
    }

    val response2 = future.map(
      obj => {
        val check = obj.map(error1 => {

          error1.map(error2 => {
            val strength = (error2 \ "strength").get.toString()
            val frequency = (error2 \ "frequency").get.toString().toDouble
            val inv = (error2 \ "_inV").get.toString()

            val tuple = (inv,(frequency,strength))

            tuple
          })
        })
        //check.foreach().groupBy()

        val filteredresults = check.map(temp => {
          val   afilter = temp.filter(x => {
            x._2._1 > fFilter
          })

          val results = afilter.take(100)

          results
        })

        filteredresults
      })


    val now = response2.flatMap{ x => {

      val dataToSend = x.map { temp => {
        val necessary = temp.map(key => {

          val getName = key._1

          val check = WS.url("http://localhost:8282/graphs/sarthakretail/vertices/" + getName).get().map {
            res => {
              val temp =  (res.json \ "results").get
              val name = (temp \ "name").get.toString()
              val vertexFrequency = (temp \ "frequency").get.toString()

              (name,vertexFrequency)
            }
          }

          val finalTuple = check.map(name => entityobj(name._1,name._2, key._2._2, key._2._1))
          finalTuple
        })

        Future.sequence(necessary)
      }
      }
      Future.sequence(dataToSend)
    }
    }

    now.map {
      case result: Seq[Seq[entityobj]] =>
        Ok(views.html.sarthak("Showing data in table", result,item,fFilter))
    }
  }

}
