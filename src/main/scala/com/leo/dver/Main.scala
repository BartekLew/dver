package me.leo.dver

import java.io.{InputStream, OutputStream, File, PrintWriter}
import java.net.InetSocketAddress
import scala.io.Source._
import scala.collection.immutable.Map

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

object Main {

	def main(args: Array[String]) {
		var port = 8000
		if(args.size > 0)
			port = args(0).toInt
	
		val server = HttpServer.create(new InetSocketAddress(port), 0)
		server.createContext("/r/", new ReadHandler())
		server.createContext("/w/", new WriteHandler())
		server.createContext("/d/", new DeleteHandler())
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

class StyledLink(url:String, style:String, text:String) extends Tag(
	"a",Map("href" -> url, "style"->style), Some(text)
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

trait Iface {
	def l:List[Tag]

	def merge(ifaces:List[Iface]): List[Tag] = ifaces.size match {
		case 0 => this.l
		case _ => this.l ++ List(new Tag("br")) ++ 
				ifaces.head.merge(ifaces.tail)
	}
}

class DirListing(f:File) extends Iface {
	def fileLink(f:File) : String =
		new Link("/r/" + f.getPath, f.getName).toString +
		new StyledLink("/d/" + f.getPath,
			"margin-left:1ex;font-size:0.8em;font-weight:bold;",
			"x").toString

	def l = List(
		new Tag("h3", Map(), Some("Directory: " + f.getPath)),
		new BList(f.listFiles.toList.map(fileLink))
	)
}

class FileCreator(cwd:File) extends Iface {
	def jsId(id:String) = "document.getElementById(\"" + id + "\")"

	def l = List(
		new Tag("input", Map("type"->"text", "id"->"newfilename")),
		new JsFunction("newFile",
			"window.location.href=\"/r/" +
				cwd.getPath + "/\" + " +
				jsId("newfilename") + ".value;"
		), 
		new Button("Create", "newFile()")
	)
}

class FileUploader(cwd:File) extends Iface {
	def jsId(id:String) = "document.getElementById(\"" + id + "\")"

	def l = List(
		new Tag("input", Map("type"->"file", "id"->"upFile")),
		new JsFunction("uploadFile",
			"var f = " + jsId("upFile") + ".files[0];" +
			"var o = new XMLHttpRequest();" +
			"o.open(\"POST\", \"/w/"+ cwd.getPath +"/\"+ f.name);"+
			"o.onreadystatechange = function(){ " +
				"if(this.readyState == XMLHttpRequest.DONE" +
				" && this.status == 200)" +
				" window.location.reload(false);};" +
			"o.send(f);"
		),
		new Button("Upload", "uploadFile()")
	)
}

class FileEditor(cwd:File) extends Iface {
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

	def l = List(
		new Tag("h3", Map(), Some("File: " + cwd.getPath)),
		new Tag("textarea", Map("id" -> "texted"),
			Some(fileContent(cwd))),
		new JsFunction("updateFile",
			jsHttpPost("/w/" + cwd.getPath,
				jsId("texted") + ".value"
			)),
		new Tag("br"),
		new Button("save", "updateFile()")
	)
}

class Document(ifaces : List[Iface]) extends Iface {
	private def tags = ifaces.size match {
		case 0 => List()
		case _ => ifaces.head.merge(ifaces.tail)
	}

	def headCss(styles:Map[String,String]) = new Tag("head",
		List(new Encoding("utf-8"), new CssBlock(styles))
	)

	def l = List(
		headCss(Map("textarea"->"width:90%;height:90%")),
		new Tag("body", tags)
	)

	def html = new Tag("html", l).toString
}

class ReadHandler extends HttpHandler {
	def fileIfaces(file:File) : List[Iface]= file.isDirectory match {
		case true => List(
			new DirListing(file), new FileCreator(file),
			new FileUploader(file)
		)
		case false => List(new FileEditor(file))
	}

	def lPath(uri:String) = uri match {
		case "/r/" => "."
		case _ => "^/r/".r.replaceFirstIn(uri,"")
	}

	def handle(t: HttpExchange) {
		val response = new Document(
			fileIfaces(new File(lPath(t.getRequestURI.getPath)))
		).html
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

class DeleteHandler extends HttpHandler {
	def lPath(t:HttpExchange) =
		"." + "^/d".r.replaceFirstIn(t.getRequestURI.getPath,"")

	def handle(t:HttpExchange) {
		val target = new File(lPath(t))
		target.delete

		val response = new Document(new ReadHandler().fileIfaces(
			new File(lPath(t)).getParentFile
		)).html
		t.sendResponseHeaders(200, response.length)
		
		val o = t.getResponseBody
		o.write(response.getBytes);
		o.close
	}
}
