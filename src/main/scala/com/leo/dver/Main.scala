package me.leo.dver

import java.io.{InputStream, OutputStream, File, PrintWriter}
import java.net.InetSocketAddress
import scala.io.Source._
import scala.collection.immutable.Map

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

object Main {

	def main(args: Array[String]) {
		val server = HttpServer.create(new InetSocketAddress(8000), 0)
		server.createContext("/r/", new ReadHandler())
		server.createContext("/w/", new WriteHandler())
		server.setExecutor(null)
		server.start()
	}

}

class Tag(name:String, args:Map[String,String], value:Option[String]) {

	def this(name:String) = this(name, Map(), None)
	def this(name:String, args:Map[String,String]) =
		this(name, args, None)
	def this(name:String, value:String) = this(name, Map(), Some(value))
	def this(name:String, children:List[Tag]) = this(name, Map(),
		Some(children.foldLeft(""){(a, v)=> a + v.toString})
	)

	private def argsStr = args.size match {
		case 0 => ""
		case _ => " " + args.keys.fold ("") {
			(acc, v) => acc + v + "=\"" + args(v) +	"\" "
		}
	}

	private def closingStr = value match {
		case Some(s) => ">" + s + "</" + name + ">"
		case None => "/>"
	}

	override def toString =
		"<" + name + argsStr + closingStr
}

class BList(items:List[String]) extends Tag("ul",
	items.foldLeft("") { (x, r) =>
		x + "<li>" + r + "</li>"
	}
)

class Link(url:String, text:String) extends Tag(
	"a",Map("href" -> url), Some(text)
)

class Encoding(v:String) extends Tag("meta", Map("charset" -> v))

class CssBlock(rules:Map[String,String]) extends Tag("style", Map(),
	Some(rules.keys.foldLeft("") {
		(a, k) => a + "\n" + k + "{" + rules(k) + "}"
	}
))

class JsFunction(name:String, body:String) extends Tag(
	"script", Map("type" -> "text/javascript"),
	Some("function " + name + "() {" + body + "}")
)

class Button(text:String, action:String) extends Tag(
	"button", Map("onclick"->action), Some(text)
)

class ReadHandler extends HttpHandler {
	def fileLink(f:File) : String =
		new Link("/r/" + f.getPath, f.getName).toString

	def headCss(styles:Map[String,String]) = new Tag("head",
		List(new Encoding("utf-8"), new CssBlock(styles))
	)

	def doc(body:List[Tag]) : String = new Tag("html",
		List(	headCss(Map("textarea"->"width:90%;height:90%")),
			new Tag("body", body)
		)).toString

	def jsHttpPost(uri:String, body:String) =
		"var o = new XMLHttpRequest();" +
		"o.open(\"POST\", \"" + uri + "\", true);" +
		"o.onreadystatechange = function(){ " +
			"if(this.readyState == XMLHttpRequest.DONE" +
			" && this.status == 200) alert(\"done!\");};" +
		"o.send("+body+");"
	
	def jsId(id:String) = "document.getElementById(\"" + id + "\")"

	def fileContent(f:File) = f.exists match {
		case true => fromFile(f).mkString
		case false => ""
	}

	def textEditor(file:File) = List(
		new Tag("h3", Map(), Some("File: " + file.getPath)),
		new Tag("textarea", Map("id" -> "texted"),
			Some(fileContent(file))),
		new JsFunction("updateFile",
			jsHttpPost("/w/" + file.getPath,
				jsId("texted") + ".value"
			)),
		new Tag("br"),
		new Button("save", "updateFile()")
	)

	def dirList(f:File) = List(
		new Tag("h3", Map(), Some("Directory: " + f.getPath)),
		new BList(f.listFiles.toList.map(fileLink)),
		new Tag("br"),
		new Tag("input", Map("type"->"text", "id"->"newfilename")),
		new JsFunction("newFile",
			"window.location.href=\"/r/" +
				f.getPath + "/\" + " +
				jsId("newfilename") + ".value;"
		), 
		new Button("Create", "newFile()")
	)

	def fileDoc(file:File) : String = file.isDirectory match {
		case true => doc(dirList(file))
		case false => doc(textEditor(file))
	}

	def lPath(uri:String) = uri match {
		case "/r/" => "."
		case _ => "^/r/".r.replaceFirstIn(uri,"")
	}

	def handle(t: HttpExchange) {
		val response = fileDoc(new File(lPath(t.getRequestURI.getPath)))
		t.sendResponseHeaders(200, response.length())
		val os = t.getResponseBody
		os.write(response.getBytes)
		os.close()
	}

}

class WriteHandler extends HttpHandler {
	def lPath(t:HttpExchange) =
		"." + "^/w".r.replaceFirstIn(t.getRequestURI.getPath,"")

	def handle(t:HttpExchange) {
		{
			val o = new PrintWriter(lPath(t))
				val i = t.getRequestBody

				Iterator.continually(i.read)
				.takeWhile(-1 !=)
				.foreach(o.write)
				o.close
		}

		val response = "<resp status=\"ok\"/>"
		t.sendResponseHeaders(200, response.length)
		
		val o = t.getResponseBody
		o.write(response.getBytes);
		o.close
	}
}
