package model

import org.jsoup.nodes.Document
import org.jsoup.nodes.Node
import org.jsoup.nodes.TextNode

/**
 * サークル情報
 */
case class Circle(
  val user: String,                /* チェックした人 */
  val space: String,               /* スペース名*/
  val name: String,                /* サークル名 */
  val books: Seq[Book],            /* 頒布物リスト */
  val description: Option[String], /* 備考 */
  val pixiv: Option[String],       /* pixivURL */
  val twitter: Option[String],     /* twitterURL */
  val web: Option[String],         /* webサイトURL */
  val domo: Option[Boolean]        /* 挨拶回りサークルかどうか */
) {
  /**
   * 備考欄のHTMLテキスト
   *
   * @return Seq[Node] 備考欄td内へ挿入するDOMノードのリスト
   */
  def getDescriptionHTML: Seq[Node] = {
    (Seq(
      makeLink(pixiv,"pixiv"),
      makeLink(twitter,"twitter"),
      makeLink(web,"web")
    )++buildDescription).filter(link => link.isDefined).map(_.get)
  }

  /**
   * 複数人のdescriptionをマージ
   */
  def mergeDescription(circle: Circle): Option[String] = {
    (description,circle.description) match {
      case (Some(desc1), Some(desc2)) => Some(desc1 + "\n" + desc2)
      case (Some(desc1), None) => Some(desc1)
      case (None, Some(desc2)) => Some(desc2)
      case (_, _) => None
    }
  }

  /**
   * 本リストのマージ
   */
  def mergeBooks(circle: Circle): Seq[Book] = {
    books.filter(book1 =>
      circle.books.exists(book2 =>
        book1.name.equals(book2.name)
      )
    ).map(book1 => book1.mergeBook(
      circle.books.find(book2 =>
        book1.name.equals(book2.name)
      ).get
    )) ++
    books.filterNot(book1 =>
      circle.books.exists(book2 =>
        book1.name.equals(book2.name))
    ) ++
    circle.books.filterNot(book2 =>
      books.exists(book1 =>
        book1.name.equals(book2.name))
    )
  }

  def buildDescription: Seq[Option[Node]] = {
    description match {
      case None => Seq(None)
      case Some(desc) => {
        val strList = desc.split("\n")
        strList.map(str => Some(new TextNode(str,"")))
      }
    }
  }

  /**
   * リンク生成（pixiv, twitter, web）
   *
   * @params Option[String] url URL
   * @params String str リンク文字列
   * @return 備考欄td内へ挿入するDOMノード
   */
  def makeLink(url: Option[String], str: String): Option[Node] = {
    val document = new Document("")
    url match {
      case Some(url) => Some(document.createElement("a").attr("href",url).text(str))
      case None => None
    }
  }

  /**
   * 挨拶対象であるかどうか
   */
  def isDomo = {
    domo match {
      case Some(domo) => domo
      case None => false
    }
  }
}