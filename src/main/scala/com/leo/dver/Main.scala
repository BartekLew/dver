package me.leo.dver

import java.io.{FileInputStream, FileOutputStream, InputStream, OutputStream, File}
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

	def dirContent(f:File) =
		new File(f.getPath + "/.content.sh").exists match{
			case false => List(
				new DirListing(f),
				new FileCreator(f),
				new FileUploader(f),
				new Shell(f.getPath)
			)
			case true => List(
				new ScriptResult(f.getPath + "/.content.sh")
			)
		}

	def shCmd(cmd:String, cwd:String) : String = {
		val pwd = cwd

		/* box is a utility allowing to run a program redirecting all
		   streams to FS nodes. If it doesn't exist, named pipe is used.
		   I want regular files so that I can load whole output/error
		   history anytime. That's I create files for output and error
		   streams. More info:

		   https://github.com/BartekLew/box/blob/master/box.c
		*/
		if(new File(pwd + "/box.out").exists == false) {
			new File(cwd + "/box.out").createNewFile()
			new File(cwd + "/box.err").createNewFile()
			Process("box sh", new File(pwd)).!!
		}

		val o = new FileOutputStream(pwd + "/box.in")
		o.write(("echo $PWD'> '" + cmd + "\n").getBytes)
		o.write((cmd + "\n").getBytes)
		o.write("echo\n".getBytes)
		o.close

		return "<resp>ok</resp>"
	}

	def main(args: Array[String]) {
		var port = 8000
		if(args.size > 0)
			port = args(0).toInt
	
		val server = HttpServer.create(new InetSocketAddress(port), 8000)
		server.createContext("/r/", new UriHandler( uri => {
			val handle = new File( "./" + uri)
			handle.isDirectory match {
				case true => new Document(dirContent(handle)).html
				case false => new Document(
					List(new FileEditor(handle), new Shell(handle.getParent))
				).html
			}

		} ))
		server.createContext("/R/", new DownloadHandler())
		server.createContext("/w/", new WriteHandler())
		server.createContext("/d/", new DeleteHandler())
		server.createContext("/sh/", new GetPostHandler( req =>
			req.method match {
				case "GET" =>
					new Document(List(new Shell(req.query))).html
				case "POST" => shCmd(req.post, "./" + req.query)
			}))
		server.setExecutor(
			java.util.concurrent.Executors.newCachedThreadPool()
		)
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

	def fileContent(f:File) = f.exists match {
		case true => xml.Utility.escape(fromFile(f, "utf-8").mkString)
		case false => ""
	}
}

class ScriptResult(path:String) extends Iface {
	def tags : List[Tag] = List(
		new Tag("div", Map(), Some(path!!))
	)

	def js = ""
}
 
class DirListing(f:File) extends Iface {
	def openLinks(f:File) : String =
		f.isDirectory match {
			case true => new Link(
				"/r/" + f.getPath, f.getName
			).toString

			case false => new Link(
				"/R/" + f.getPath, f.getName
			).toString + " " + new Link (
				"/r/" + f.getPath, "[edit]"
			).toString
		}

	def listing(f:File) : List[File] = f.getPath match {
		case "/" | "." => f.listFiles.toList
		case _ => List(new File(f.getPath+"/..")) ++ f.listFiles.toList
	}

	def fileLinks(f:File) : String =
		openLinks(f) +
		new StyledLink("/d/" + f.getPath,
			"margin-left:1ex;font-size:0.8em;font-weight:bold;",
			"x").toString

	def tags = List(
		new Tag("h3", Map(), Some("Directory: " + f.getPath)),
		new BList(listing(f).map(fileLinks))
	)

	def js = ""
}

class FileCreator(cwd:File) extends Iface {
	def tags = List(
		new Tag("input", Map("type"->"text", "id"->"newfilename")),
		new Button("Create", "newFile()")
	)

	def js = new JsLocation().set(
		new Js().literal("/r/" + cwd.getPath + "/")
			.jsId("newfilename", "value")
	).asFun("newFile").code 
}

class FileUploader(cwd:File) extends Iface {
	def tags = List(
		new Tag("input", Map("type"->"file", "id"->"upFile")),
		new Button("Upload", "uploadFile()")
	)

	def js = (new JsVar("r").set(new Js().jsId("upFile", "files[0]")) +
		new JsHttp("POST", new Js().literal("/w/" + cwd.getPath + "/")
				.jsVar("r.name"), new Js("r"),
				new Js("window.location.reload(false)")
		)).asFun("uploadFile").code
	
}

class FileEditor(cwd:File) extends Iface {
	def tags = List(
		new Tag("h3", Map(), Some("File: " + cwd.getPath + " " +
			new Tag("a", Map("href" -> ("/r/" + cwd.getParent)), Some("(parent directory)")).toString
		)),
		new Tag("textarea", Map("id" -> "texted",
				"onkeypress"->"onKey(event)"),
			Some(fileContent(cwd))),
		new Tag("br"),
		new Button("save", "updateFile()")
	)

	def js = new JsHttp("POST", new Js().literal("/w/" + cwd.getPath),
			new Js().jsId("texted", "value"),
			new Js("alert(\"done\");")
		).asFun("updateFile").code +
		new Js().cond(new Js("ev.keyCode == 9"),
			new Js().call("ev.preventDefault", List()) +
			new JsVar("ed").set(new Js().jsId("texted")) +
			new JsVar("pos").set(new Js("ed.selectionStart")) +
			new Js("ed.value").set(
				new Js("ed.value.substring(0, pos)")
					.literal("\\t").jsVar(
		"ed.value.substring(ed.selectionEnd, ed.value.length)"
				)
			) +
			new Js("ed.selectionStart = ed.selectionEnd = pos+1;")
		).asFun("onKey", "ev").code

}

class Shell(path:String) extends Iface {
	def js = (new JsHttp("GET",
			new Js().literal("/R/" + path + "/box.out"),
			new Js(),
			new JsVar("o").set(new Js().jsId("sh_out")) +
			new Js().jsVar("o.value").set(
				new Js().jsVar("this.responseText")
			) +
			new Js().jsVar("o.scrollTop").set(
				new Js().jsVar("o.scrollHeight")
			)
		).asFun("refresh_output") +
		new Js().cond(new Js("ev.keyCode == 13"),
			new JsVar("cmd").set(new Js().jsId("sh_in", "value")) +
			new JsVar("out").set(new Js().jsId("sh_out")) +
			new Js("refresh_output();") +
			new JsHttp("POST", new Js().literal("/sh/" + path),
				new Js().jsVar("cmd"),
				new Js("setInterval(refresh_output, 2000);")
		)).asFun("sh_cmd", "ev")).code

	def tags = List(
		new Tag("h4", Map(), Some("shell:")),
		new Tag("input", Map(
			"type"->"text", "id"->"sh_in",
			 "onkeypress"->"sh_cmd(event)"
		)),
		new Tag("textarea disabled", Map("id"->"sh_out"),
			Some(fileContent(new File(path + "/box.out")))
		)
	)
}

class Js(val code:String) {
	def this() = this("")

	def +(s:String) = new Js(code + s)
	def +(js:Js) = new Js(code + js.code)

	def asFun(name:String, args:String = "") = new Js(
		"function " + name + "(" + args +") {" + code + "}"
	)
	
	def set(v:Js) = new Js(code + " = " + v.code + ";")

	def get(t:Js) = new Js(t.code + " = " + code + ";")
	
	def increase(v:Js) = new Js(code + " += " + v.code + ";")

	def append(s:String) = code match {
		case "" => new Js(s)
		case x => new Js(x + " + " + s)
	}

	def literal(s:String) : Js = append("\"" + s + "\"")

	def jsVar(s:String) = append(s)

	def jsId(id:String, property:String) = append(
		"document.getElementById(\"" + id + "\")" + "." + property
	)

	def jsId(id:String) = append(
		"document.getElementById(\"" + id + "\")"
	)

	private def argList(args:List[Js]) : String = args.length match {
		case 0 => ""
		case 1 => args.head.code
		case _ => args.head.code + ", " + argList(args.tail)
	}

	def call(fun:String, args:List[Js]) = new Js(
		code + fun + "(" + argList(args) + ");"
	)

	def cond(test:Js, action:Js) =
		new Js(code + "if(" + test.code + ") {" + action.code + "}")
}

class JsLocation() extends Js("window.location.href")

class JsVar(name:String) extends Js("var " + name)

class JsHttp(method:String, url:Js, data:Js, action:Js) extends Js(
	(new JsVar("o").set(new Js("new XMLHttpRequest()")).call(
		"o.open", List(new Js().literal(method), url)
	) + new Js("o.onreadystatechange").set(
		new Js("if(this.readyState == XMLHttpRequest.DONE" +
			" && this.status == 200) {" + action.code + "}")
			.asFun("")
	).call("o.send", List(data)).code).code
)

class Document(ifaces : List[Iface]) extends Iface {
	private def getTags = ifaces.size match {
		case 0 => List()
		case _ => ifaces.head.merge(ifaces.tail)
	}

	def headCss(styles:Map[String,String]) = new Tag("head",
                /* FIXME: if system locale is not UTF-8, fromFile() will
                 * convert content to encoding specified in locale.
                 * That's why set it to UTF-8 or expect garbage in text
                 * file editor. :( */
		List(new Encoding("utf-8"), new CssBlock(styles),
			new Tag("script", js))
	)

	def tags = List(
		headCss(Map(
			"textarea"->"width:60em;height:90%",
			"#sh_in"->"width:60em",
			"body,textarea,input"->"background-color:black; color:white;",
			"a"->"color:yellow")),
		new Tag("body", getTags)
	)

	def js = ifaces.foldLeft("") {
		(a,v) => a + v.js
	}

	def html = new Tag("html", tags).toString
}

class StackTrace(e:Exception) {
	override def toString() =  e.toString() + "\n Caused by:\n" +
		e.getStackTrace().foldLeft(""){
			(total,element) => total + element.toString +"\n"
		}+ "\n"
}

class UriHandler(respond:String=>String) extends HttpHandler {
	def lPath(uri:String) :String = "^/\\w+/".r.replaceFirstIn(uri,"")

	def get(t:HttpExchange) = try{
			respond(lPath(t.getRequestURI.getPath))
		} catch {
			case e:Exception => new StackTrace(e).toString()
		}
	


	def handle(t: HttpExchange) {
		val response = get(t).getBytes
 		t.sendResponseHeaders(200, response.length)
		val os = t.getResponseBody
		os.write(response)
		os.close()
	}
}

class GetPostRequest(val query:String, val method:String, val post:String) {
	def this(queryPrefix:String, t:HttpExchange) = this(
		queryPrefix.r.replaceFirstIn(t.getRequestURI.getPath,""),
		t.getRequestMethod(),
		fromInputStream(t.getRequestBody(), "utf-8").mkString
	)
}

class GetPostHandler(respond:GetPostRequest=>String) extends HttpHandler {
	def responseFor(req:GetPostRequest) = try {
		respond(req)
	} catch {
		case e:Exception => new StackTrace(e).toString
	}

	def handle(t: HttpExchange) {
		val response = responseFor(new GetPostRequest("^/[^/]*/", t)).getBytes
		t.sendResponseHeaders(200, response.length)
		val os = t.getResponseBody
		os.write(response)
		os.close()
	}
}

class FSItem(path:String) {
	def this(f:File) = this(f.getPath)

	def mime = "^.*\\.".r.replaceFirstIn(path,"") match {
		case "pdf" => "application/pdf"
		case "txt" => "text/plain"
		case "html" => "text/html"
		case "jpg" => "image/jpeg"
		case "css" => "text/css"
		case _ => "application/octet-stream"
	}
}

class DownloadHandler() extends HttpHandler {
	def lPath(uri:String) :String = "^/\\w+/".r.replaceFirstIn(uri,"")
	
	def handle(t: HttpExchange) {
		val f = new File(lPath(t.getRequestURI.getPath))
		val in = new FileInputStream(f)
		val mime = new FSItem(f).mime

		t.getResponseHeaders().set(
			"Content-Type", mime
		)

		if(mime == "application/octet-stream") {
			t.getResponseHeaders().set(
				"Content-Disposition", "attachment"
			)
		}

		t.sendResponseHeaders(200, f.length())

		val os = t.getResponseBody
		Iterator.continually(in.read)
			.takeWhile(-1 !=)
			.foreach(os.write)
		os.close()
	}
}
 
class WriteHandler extends HttpHandler {
	def lPath(t:HttpExchange) =
		"." + "^/w".r.replaceFirstIn(t.getRequestURI.getPath,"")

	def handle(t:HttpExchange) {
		{
			val o = new FileOutputStream(lPath(t))
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
		t.sendResponseHeaders(307, 0)
		
		val o = t.getResponseBody
		o.close
	}
}
