package builcirclelist

import java.io.File
import java.io.FileInputStream
import java.io.InputStream

import scala.collection.JavaConversions._

import org.yaml.snakeyaml.Yaml

import config.Config
import config.LoadConfig
import model.Book
import model.CheckList
import model.Circle

/**
 * 複数人のyamlリストをマージしてhtml出力する
 *
 * TODO: yaml追記
 */
object BCLMain {
  val config: Config = LoadConfig.config
  def main(args: Array[String]):Unit = {
    SaveHTML.saveHTML(
      mergeList(config.input.map(input => {
        loadYaml(input)
      }))
    )
  }

  /**
   * 各人のYAMLを読み込み、リストを返す
   *
   * @params String filename ファイル名
   * @return CheckList チェックリスト
   */
  def loadYaml(filename: String): CheckList = {
    val input: InputStream = new FileInputStream(new File(filename))
    val yaml: Yaml = new Yaml

    val ymap: Map[String, Object] = mapAsScalaMap(yaml.load(input).asInstanceOf[java.util.Map[String, Object]]).toMap
    val user: Option[String] = ymap.get("user").asInstanceOf[Option[String]]
    val ycircles: List[Map[String, Object]] = ymap.get("list").get.asInstanceOf[java.util.List[java.util.Map[String, Object]]].toList.map(_.toMap)

    CheckList(
      parseCircles(user, ycircles).sortWith(
        (o1, o2) => o1.space.compareTo(o2.space) < 0
      )
    )
  }

  /**
   * 複数人のリストをマージする
   * 重複したスペースはユーザ情報を複数人のものに書き換える
   *
   * @params Seq[CheckList] lists チェックリストのリスト
   * @return CheckList マージしたチェックリスト
   */
  def mergeList(lists: Seq[CheckList]): CheckList = {
    val list: CheckList = CheckList(
      lists.map(_.circles).reduceLeft((l1,l2) => List.concat(l1, l2))
    )
    /* スペースが重複しているところをマージ */
    val listGroup = list.circles.groupBy(_.space)
    val circles = CheckList(listGroup.map( circle => {
      if(circle._2.size > 1){
        Circle(config.getDoubleName(circle._2(0).user, circle._2(1).user).get,
          circle._1,
          circle._2(0).name,
          circle._2(0).mergeBooks(circle._2(1)),
          circle._2(0).mergeDescription(circle._2(1)),
          circle._2(0).pixiv match {
            case Some(p) => Some(p)
            case None => circle._2(1).pixiv
          },
          circle._2(0).twitter match {
            case Some(t) => Some(t)
            case None => circle._2(1).twitter
          },
          circle._2(0).web match {
            case Some(w) => Some(w)
            case None => circle._2(1).web
          },
          Some(circle._2(0).isDomo || circle._2(1).isDomo)
        )
      } else {
        circle._2(0)
      }
    }).toSeq)
    CheckList(circles.circles.sortWith((o1, o2) => o1.space.compareTo(o2.space) < 0))
  }

  /**
   * サークル情報をパースする
   *
   * @params Option[String] user ユーザ名
   * @params List[Map[String, Object]] ycircles サークルリストのYAML
   * @return List[Circle] サークル情報のリスト
   */
  def parseCircles(user: Option[String], ycircles: List[Map[String, Object]]): List[Circle] = {
    ycircles.map(y => {
      val userName = (y.get("user"),user) match {
        case (Some(name),_) => name.asInstanceOf[String]
        case (None,Some(name)) => name
        case _ => ""
      }
      Circle(
        userName,
        y.get("loc").get.asInstanceOf[String],
        y.get("name").get.asInstanceOf[String],
        parseBooks(y.get("book").get.asInstanceOf[java.util.List[java.util.Map[String, Object]]].toList.map(_.toMap)),
        parseCircleElement(y.get("description"), classOf[String]),
        parseCircleElement(y.get("pixiv"), classOf[String]),
        parseCircleElement(y.get("twitter"), classOf[String]),
        parseCircleElement(y.get("web"), classOf[String]),
        parseCircleElement(y.get("domo"), classOf[Boolean])
      )
    })
  }

  def parseCircleElement[T](e: Option[Object], T: Class[T]): Option[T] = {
    e match {
      case Some(null) => None
      case Some(e) => Option(e.asInstanceOf[T])
      case None => None
    }
  }

  /**
   * 頒布物の情報をパース
   *
   * @params List[Map[String, Object]] ybooks 本情報のYAML
   * @return List[Book] 本情報のリスト
   */
  def parseBooks(ybooks: List[Map[String, Object]]): List[Book] = {
    ybooks.map(y =>
      Book(y.get("name").asInstanceOf[Option[String]],
          y.get("prize").asInstanceOf[Option[Int]],
          y.get("url").asInstanceOf[Option[String]]
          )
    )
  }
}

