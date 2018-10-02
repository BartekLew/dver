package me.leo.dver

import java.io.{InputStream, OutputStream, File, PrintWriter}
import java.net.InetSocketAddress
import scala.io.Source._
import scala.collection.immutable.Map

import scala.sys.process._

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

object Main {

	def fileIfaces(file:File) : List[Iface]= file.isDirectory match {
		case true => List(
			new DirListing(file), new FileCreator(file),
			new FileUploader(file)
		)
		case false => List(new FileEditor(file))
	}

	def main(args: Array[String]) {
		var port = 8000
		if(args.size > 0)
			port = args(0).toInt
	
		val server = HttpServer.create(new InetSocketAddress(port), 0)
		server.createContext("/r/", new UriHandler( uri => {
			val handle = new File( "./" + uri)
			handle.isDirectory match {
				case true => new Document(List(
					new DirListing(handle),
					new FileCreator(handle),
					new FileUploader(handle)
				)).html
				case false => new Document(
					List(new FileEditor(handle))
				).html
			}

		} ))
		server.createContext("/w/", new WriteHandler())
		server.createContext("/d/", new DeleteHandler())
		server.createContext("/sh/", new UriHandler( uri => uri match {
			case "" => new Document(List(new Shell())).html
			case _ => uri !!
		}))
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

class Button(text:String, action:String) extends Tag(
	"button", Map("onclick"->action), Some(text)
)

trait Iface {
	def tags:List[Tag]
	def js:String

	def merge(ifaces:List[Iface]): List[Tag] = ifaces.size match {
		case 0 => this.tags
		case _ => this.tags ++ List(new Tag("br")) ++ 
				ifaces.head.merge(ifaces.tail)
	}

	def jsFun(name:String, body:String) =
		"function " + name + "() {" + body + "}"

	def jsFun(name:String, args:String, body:String) =
		"function " + name + "(" + args + ") { " + body + "}"

	def jsId(id:String) = "document.getElementById(\"" + id + "\")"
}

class DirListing(f:File) extends Iface {
	def fileLink(f:File) : String =
		new Link("/r/" + f.getPath, f.getName).toString +
		new StyledLink("/d/" + f.getPath,
			"margin-left:1ex;font-size:0.8em;font-weight:bold;",
			"x").toString

	def tags = List(
		new Tag("h3", Map(), Some("Directory: " + f.getPath)),
		new BList(f.listFiles.toList.map(fileLink))
	)

	def js = ""
}

class FileCreator(cwd:File) extends Iface {
	def tags = List(
		new Tag("input", Map("type"->"text", "id"->"newfilename")),
		new Button("Create", "newFile()")
	)

	def js = jsFun("newFile",
		"window.location.href=\"/r/" +
		cwd.getPath + "/\" + " +
		jsId("newfilename") + ".value;"
	) 
}

class FileUploader(cwd:File) extends Iface {
	def tags = List(
		new Tag("input", Map("type"->"file", "id"->"upFile")),
		new Button("Upload", "uploadFile()")
	)

	def js = jsFun("uploadFile",
		"var f = " + jsId("upFile") + ".files[0];" +
		"var o = new XMLHttpRequest();" +
		"o.open(\"POST\", \"/w/"+ cwd.getPath +"/\"+ f.name);"+
		"o.onreadystatechange = function(){ " +
			"if(this.readyState == XMLHttpRequest.DONE" +
			" && this.status == 200)" +
			" window.location.reload(false);};" +
		"o.send(f);"
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
	
	def fileContent(f:File) = f.exists match {
		case true => fromFile(f).mkString
		case false => ""
	}

	def tags = List(
		new Tag("h3", Map(), Some("File: " + cwd.getPath)),
		new Tag("textarea", Map("id" -> "texted",
				"onkeypress"->"onKey(event)"),
			Some(fileContent(cwd))),
		new Tag("br"),
		new Button("save", "updateFile()")
	)

	def js = jsFun("updateFile",
		jsHttpPost("/w/" + cwd.getPath,
			jsId("texted") + ".value"
		)) + jsFun("onKey", "ev",
			"if(ev.keyCode == 9) {" +
				"ev.preventDefault();" +
				"var ed = " + jsId("texted") + ";" +
				"var pos = ed.selectionStart;" +
				"ed.value = ed.value.substring(0, pos) + \"\\t\" + ed.value.substring(ed.selectionEnd, ed.value.length);" +
				"ed.selectionStart = ed.selectionEnd = pos+1;" +
			"}"
		)
}

class Shell() extends Iface {
	def js = jsFun("sh_cmd", "ev",
		"if(ev.keyCode == 13) { " +
			"var cmd = " + jsId("sh_in") + ".value;" +
			"var o = new XMLHttpRequest();" +
			"o.open(\"GET\", \"/sh/\" + cmd);" +
			"o.onreadystatechange = function(){ " +
				"if(this.readyState == XMLHttpRequest.DONE" +
				" && this.status == 200) {" +
					"var i = " + jsId("sh_in") +";" +
					jsId("sh_out") + ".value += i.value + "+
					"\">\\n\" + this.responseText + \"\\n\";" +
					"i.value = \"\";}};" +
			"o.send();" +
		"}"
	)
	
	def tags = List(
		new Tag("input", Map(
			"type"->"text", "id"->"sh_in",
			 "onkeypress"->"sh_cmd(event)"
		)),
		new Tag("textarea", Map("id"->"sh_out"), Some(""))
	)
}

class Document(ifaces : List[Iface]) extends Iface {
	private def getTags = ifaces.size match {
		case 0 => List()
		case _ => ifaces.head.merge(ifaces.tail)
	}

	def headCss(styles:Map[String,String]) = new Tag("head",
		List(new Encoding("utf-8"), new CssBlock(styles),
			new Tag("script", js))
	)

	def tags = List(
		headCss(Map("textarea"->"width:90%;height:90%")),
		new Tag("body", getTags)
	)

	def js = ifaces.foldLeft("") {
		(a,v) => a + v.js
	}

	def html = new Tag("html", tags).toString
}

class UriHandler(respond:String=>String) extends HttpHandler {
	def lPath(uri:String) :String = "^/\\w+/".r.replaceFirstIn(uri,"")

	def handle(t: HttpExchange) {
		val response = respond(lPath(t.getRequestURI.getPath))
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

		t.getResponseHeaders().add(
			"Location", "/r/" + target.getParentFile
		)
		t.sendResponseHeaders(308, 0)
		
		val o = t.getResponseBody
		o.close
	}
}
