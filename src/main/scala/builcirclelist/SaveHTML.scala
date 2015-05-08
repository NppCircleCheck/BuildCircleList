package builcirclelist

import config.LoadConfig
import config.Config
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.io.File
import model.CheckList
import model.Circle
import org.jsoup.nodes.Element
import java.io.FileOutputStream
import java.io.PrintWriter
import java.io.OutputStreamWriter

/**
 * HTML出力
 */
object SaveHTML {
  val config: Config = LoadConfig.config
  /**
   * テンプレートの読み込み
   */
  def loadTemplate: Document = {
    Jsoup.parse(new File(config.template), "UTF-8")
  }

  /**
   * HTML出力
   *
   * @params CheckList checkList 全員分のマージされたチェックリスト
   */
  def saveHTML(checkList: CheckList) = {
    val document = loadTemplate
    val htmlCheckList = document.getElementById("checklist")
    val htmlDomoList = document.getElementById("do-mo")
    checkList.circles.foreach(circle =>
      (circle.domo match {
        case Some(true) => htmlDomoList
        case _ => htmlCheckList
      }).appendChild(
        createElement(circle)))
    val printWriter: PrintWriter = new PrintWriter(new OutputStreamWriter(new FileOutputStream(config.output), "UTF-8"))
    printWriter.print(document.toString)
    printWriter.close
  }

  /**
   * スペース単位のテーブルタグ生成
   *
   * @params Circle circle サークル情報
   * @return Element スペース単位のテーブルタグDOM
   */
  def createElement(circle: Circle): Element = {
    val document = new Document("")
    /* tr */
    val element = document.createElement("tr").addClass(circle.user)
    /* スペース名 */
    val space = document.createElement("td").text(circle.space)
    /* サークル名 */
    val name = document.createElement("td").text(circle.name)

    /* 頒布物情報 */
    val books = document.createElement("td")
    if (circle.books.isEmpty) {
      books.appendText("不明")
    } else {
      circle.books.map(book =>
        books.appendChild(book.toHTML).appendElement("br"))
      books.children.last.remove
    }
    /* 価格情報 */
    val prises = document.createElement("td").attr("align","right")
    circle.books.map(book => prises.appendText(book.prize match {
      case Some(prise) => prise + "円"
      case None => "不明"
    }).appendElement("br"))
    if (circle.books.nonEmpty)
      prises.children.last.remove
    /* 備考欄 */
    val description = document.createElement("td")
    if (circle.getDescriptionHTML.nonEmpty) {
      circle.getDescriptionHTML.foreach(node =>
        description.appendChild(node).appendElement("br"))
      description.children.last.remove
    }
    element.appendChild(space).appendChild(name).appendChild(books).appendChild(prises).appendChild(description)
  }
}