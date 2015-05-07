package model

import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node
import org.jsoup.nodes.TextNode

/**
 * 頒布物情報
 */
case class Book(
  val name: Option[String],
  val prize: Option[Int],
  val url: Option[String]
) {

  /**
   * 頒布物情報のHTMLテキスト
   *
   * @return Node 頒布物td内へ追加するDOMノード
   */
  def toHTML: Node = {
    val document = new Document("")
    url match {
      case Some(url) => document.createElement("a").attr("href",url).text(getName)
      case None => new TextNode(getName,"")
    }
  }

  def getName = name.getOrElse("不明")

  /**
   * 名前が同じ頒布物同士のマージ
   */
  def mergeBook(book: Book): Book = {
    val p = prize match {
      case Some(p) => prize
      case None => book.prize
    }
    val u = url match {
      case Some(null) => book.url
      case Some(u) => url
      case None => book.url
    }
    Book(name, p, u)
  }
}