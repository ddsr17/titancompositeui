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
    Ok(views.html.rohith("Your new application is ready.",("",Nil),"",0))
  }

  def display2 = Action {
    Ok(views.html.ved("Your new application is ready.",("",Nil),"",0))
  }

  def display3 = Action {
    Ok(views.html.sarthak("Your new application is ready.",("",Nil),"",0))
  }

  import play.api.data.Forms._
  val newsForm = Form(
    single("news" -> text)
  )


  def getVertices1(item: String,fFilter: Double) = Action.async {implicit request =>
    //    val newsItem = newsForm.bindFromRequest().get
    val newsitem = item.toLowerCase()
    val newsItem = URLEncoder.encode(newsitem,"UTF-8")
    var freq = ""

//    val forOutput1 = for {
//      a <- WS.url("http://localhost:8282/graphs/titanconnected/vertices?key=name&value=" + newsItem).get()
//      b = (a.json \ "results").as[JsArray].value.head
//      c = (b \ "frequency").asOpt[String]
//      d = (b \ "_id").asOpt[String]
//      e = d match {
//        case None => Seq.empty[entityobj]
//        case Some(id) =>
//          val out = for {
//          e <- WS.url("http://107.167.178.97:8282/graphs/titanconnected/vertices/" + id + "/outE?_label=Relation").get()
//          f <- (e.json \ "results").as[JsArray].value
//          //          f1 <- ((f \ "frequency").as[Double], (f \ "strength").as[String])
//          g <- WS.url("http://107.167.178.97:8282/graphs/titanconnected/vertices/" + id + "/outE?_label=To").get()
//          h <- (g.json \ "results").as[JsArray].value
//          //          h = ((g.json \ "results" \ "frequency").as[Double], (g.json \ "results" \ "strength").as[String])
//          k <- WS.url("http://107.167.178.97:8282/graphs/titanexample/vertices/" + (f \ "_inv").as[String]).get()
//          m = (k.json \ "results").as[JsArray].value.head
//          l <- WS.url("http://107.167.178.97:8282/graphs/titanexample/vertices/" + (h \ "_inv").as[String]).get()
//          n = (l.json \ "results").as[JsArray].value.head//
//          o = entityobj((m \ "name").as[String], (m \ "frequency").as[String], (f \ "strength").as[String], (f \ "frequency").as[Double])
//          p = entityobj((n \ "name").as[String], (n \ "frequency").as[String], (h \ "strength").as[String], (h \ "frequency").as[Double])
//        } yield (Seq(o, p))
//      }
//    } yield (c, e)
//
//    for {
//      res1 <- forOutput1
//      a = res1 match {
//        case N
//      }
//    }
//
//    match {
//      case None => None
//      case Some(id) => for {
//
//      }
//        u =
//          c  b.map(res => (res \\ "frequency")).toString()
//      d = b.map(res => (res \ "_id").get.toString()) match {
//        case Nil => None
//        case string: Seq[String] => Some(string.head)
//      }
//      e <- WS.url("http://107.167.178.97:8282/graphs/titanconnected/vertices/" + d.get + "/outE?_label=Relation").get()
//      f = ((e.json \ "results" \ "frequency").as[Double], (e.json \ "results" \ "strength").as[String])
//      g <- WS.url("http://107.167.178.97:8282/graphs/titanconnected/vertices/" + d.get + "/outE?_label=To").get()
//      h = ((g.json \ "results" \ "frequency").as[Double], (g.json \ "results" \ "strength").as[String])
//      k <- WS.url("http://107.167.178.97:8282/graphs/titanexample/vertices/" + (e.json \ "results" \ "_inv").as[String]).get()
//      m = ((k.json \ "results" \ "name").as[String], (k.json \ "results" \ "frequency").as[String])
//      l <- WS.url("http://107.167.178.97:8282/graphs/titanexample/vertices/" + (g.json \ "results" \ "_inv").as[String]).get()
//      n = ((l.json \ "results" \ "name").as[String], (l.json \ "results" \ "frequency").as[String])
//      o = entityobj(m._1, m._2, f._2, f._1)
//      p = entityobj(n._1, n._2, h._2, h._1)
//    } yield (o, p)
//    forO
    //: Future[Option[String]]
    val futureResult = WS.url("http://localhost:8182/graphs/titanexample/vertices?key=name&value=" + newsItem).get().map {
      response => {
        val temp = (response.json \ "results").as[JsArray].value
	temp match {
          case Nil => {
            freq = ""
          }
          case a => {
            freq = (temp.head \ "frequency").get.toString
          }
        }        

	val id = temp.map(res => (res \ "_id").get.toString()) match {
          case Nil => None
          case string: Seq[String] => Some(string.head)
        }
        (id,freq)
      }
    }

    var data: Seq[Seq[JsValue]] = Seq()

    val future: Future[Seq[Seq[JsValue]]] = futureResult.map(x=> x._1).flatMap {

      case None => Future(Seq[Seq[JsValue]]())
      //case None => Future(Seq[JsValue]())
      case Some(a) =>

        val finaldata = {

          val output = WS.url("http://localhost:8182/graphs/titanexample/vertices/" + a + "/outE?_label=relation").get().map {
            result => {
              (result.json \ "results").as[JsArray].value
            }
          }

          val outputtoLabel = WS.url("http://localhost:8182/graphs/titanexample/vertices/" + a + "/outE?_label=TO").get().map{
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

//          val recover = output.recover{
//            case e: JsResultException =>
//              None
//            //Redirect("http://google.com")
//
//            case js: Future[Seq[JsValue]] => Some(js)
//          }

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

          val check = WS.url("http://localhost:8182/graphs/titanexample/vertices/" + getName).get().map {
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


    val htmlInput = for {
      fut1 <- futureResult
      fut2 <- now
      //res1 <- fut1._2
    } yield (fut1._2,fut2)

    htmlInput.map {
      case result: (String,Seq[Seq[entityobj]]) =>
        //println("result check  "+result)
        Ok(views.html.rohith("Showing data in table", result,item,fFilter))
    }
  }


  def getVertices2(item: String,fFilter: Double) = Action.async {implicit request =>
    //    val newsItem = newsForm.bindFromRequest().get
    val newsitem = item.toLowerCase()
    val newsItem = URLEncoder.encode(newsitem,"UTF-8")
    var freq = ""

    val futureResult = WS.url("http://localhost:8182/graphs/titanconnected/vertices?key=name&value=" + newsItem).get().map {
      response => {
        val temp = (response.json \ "results").as[JsArray].value
       
	temp match {
          case Nil => {
            freq = ""
          }
          case a => {
            freq = (temp.head \ "frequency").get.toString
          }
        } 

	val id = temp.map(res => (res \ "_id").get.toString()) match {
          case Nil => None
          case string: Seq[String] => Some(string.head)
        }
        val both = (id,freq)
        (id,freq)
      }
    }

    var data: Seq[Seq[JsValue]] = Seq()

    val future: Future[Seq[Seq[JsValue]]]= futureResult.map(x => x._1).flatMap {

      case None => Future(Seq[Seq[JsValue]]())
      //case None => Future(Seq[JsValue]())
      case Some(a) =>

        val finaldata = {

          val output = WS.url("http://localhost:8182/graphs/titanconnected/vertices/" + a + "/outE?_label=Relation").get().map {
            result => {
              (result.json \ "results").as[JsArray].value
            }
          }

          val outputtoLabel = WS.url("http://localhost:8182/graphs/titanconnected/vertices/" + a + "/outE?_label=To").get().map{
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

//          val recover = output.recover{
//            case e: JsResultException =>
//              None
//            //Redirect("http://google.com")
//
//            case js: Future[Seq[JsValue]] => Some(js)
//          }

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
        val newcheck = check.map(x => {
          val y = x.groupBy(_._1)

          val z = y.map{
            case (k,v) => (v.head)
          }.toSeq
          z
        })

        val filteredresults = newcheck.map(temp => {
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

          val check = WS.url("http://localhost:8182/graphs/titanconnected/vertices/" + getName).get().map {
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

    val htmlInput = for {
      fut1 <- futureResult
      fut2 <- now
    //res1 <- fut1._2
    } yield (fut1._2,fut2)

    htmlInput.map {
      case result: (String,Seq[Seq[entityobj]]) =>
        Ok(views.html.ved("Showing data in table", result,item,fFilter))
    }
  }

  def getVertices3(item: String,fFilter: Double) = Action.async {implicit request =>

    val newsitem = item.toLowerCase()
    val newsItem = URLEncoder.encode(newsitem,"UTF-8")
    var freq = ""

    val futureResult = WS.url("http://localhost:8182/graphs/walmart/vertices?key=name&value=" + newsItem).get().map {
      response => {
        val temp = (response.json \ "results").as[JsArray].value
        
	temp match {
          case Nil => {
            freq = ""
          }
          case a => {
            freq = (temp.head \ "frequency").get.toString
          }
        }
	val id = temp.map(res => (res \ "_id").get.toString()) match {
          case Nil => None
          case string: Seq[String] => Some(string.head)
        }
        val both = (id,freq)
        both
      }
    }

    var data: Seq[Seq[JsValue]] = Seq()

    val future: Future[Seq[Seq[JsValue]]]= futureResult.map(x => x._1).flatMap {

      case None => Future(Seq[Seq[JsValue]]())
      //case None => Future(Seq[JsValue]())
      case Some(a) =>

        val finaldata = {

          val output = WS.url("http://localhost:8182/graphs/walmart/vertices/" + a + "/outE?_label=relation").get().map {
            result => {
              (result.json \ "results").as[JsArray].value
            }
          }

          val outputtoLabel = WS.url("http://localhost:8182/graphs/walmart/vertices/" + a + "/outE?_label=To").get().map{
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

//          val recover = output.recover{
//            case e: JsResultException =>
//              None
//            //Redirect("http://google.com")
//
//            case js: Future[Seq[JsValue]] => Some(js)
//          }

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

          val results = afilter.take(1000)

          results
        })

        filteredresults
      })


    val now = response2.flatMap{ x => {

      val dataToSend = x.map { temp => {
        val necessary = temp.map(key => {

          val getName = key._1

          val check = WS.url("http://localhost:8182/graphs/walmart/vertices/" + getName).get().map {
            res => {
              val temp =  (res.json \ "results").get
              val name = (temp \ "name").get.toString()
              val vertexFrequency = (temp \ "frequency").get.toString()
		val dimension = name.split("@")(0)
		val temptuple = (dimension,name,vertexFrequency)

              temptuple
            }
          }

          val finalTuple = check.map(name => (name._1,name._2,name._3,key._2._2, key._2._1))
          finalTuple
        })

     val send =  Future.sequence(necessary)
	val tosend = send.map{y => {y
        val y1=  y.groupBy(_._1).flatMap{ case (k,v) => v.map(f => entityobj(f._2,f._3,f._4,f._5))}.toSeq
          y1
        }}

      tosend
      }
      }
      Future.sequence(dataToSend)
    }
    }

    val htmlInput = for {
      fut1 <- futureResult
      fut2 <- now
    //res1 <- fut1._2
    } yield (fut1._2,fut2)

    htmlInput.map {
      case result: (String,Seq[Seq[entityobj]]) =>
        Ok(views.html.sarthak("Showing data in table", result,item,fFilter))
    }
  }

}
