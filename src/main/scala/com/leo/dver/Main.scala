package me.leo.dver

import java.io.{InputStream, OutputStream, File, PrintWriter}
import java.net.InetSocketAddress
import scala.io.Source._

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

class ReadHandler extends HttpHandler {
	def tag(name:String, content:String) : String =
		"<" + name + ">" + content + "</" + name + ">"
	def tag(name:String, params:String, content:String) : String =
		"<" + name + " " + params + ">" + content + "</" + name + ">"

	def blist(items : List[String]) : String =
		"<ul>" + items.foldLeft("") { (x, r) =>
			x + "<li>" + r + "</li>"
		} + "</ul>"

	def simpleLink(url:String, text:String) :String =
		tag("a", "href=\"" + url + "\"", text)

	def fileLink(f:File) : String =
		simpleLink("/r/" + f.getPath, f.getName)		

	def encoding (v:String) = "<meta charset=\"" + v + "\"/>"
	def styles = tag("style", "textarea{ width:90%; height:90%; }")
	
	def head = tag("head", encoding("utf-8") + styles)

	def doc(innerHtml:String) : String = tag("html",
		head + tag("body", innerHtml)
	)

	def jsFun(name:String, body:String) =
		tag("script", "type=\"text/javascript\"",
			"function " + name + "() {" + body + "}"
		)

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

	def textEditor(file:File) : String =
		tag("textarea", "id=\"texted\"", fileContent(file)) +
		jsFun("updateFile",
			jsHttpPost("/w/" + file.getPath,
				jsId("texted") + ".value"
			)
		) + "<br/>" +
		tag("button", "onclick=\"updateFile()\"", "save")

	def textInput(id:String) =
		"<input type=\"text\" id=\"" + id + "\"/>"

	def dirList(f:File) =
		doc(
			blist(f.listFiles.toList.map(fileLink)) + "<br/>" +
			textInput("newfilename") + 
			jsFun("newFile",
				"window.location.href=\"/r/" +
					f.getPath + "/\" + " +
					jsId("newfilename") + ".value;"
			) + 
			tag("button", "onclick=\"newFile()\"", "create")
		)

	def doc(file:File) : String = file.isDirectory match {
		case true => dirList(file) 
		case false => doc(textEditor(file))
	}

	def lPath(uri:String) = uri match {
		case "/r/" => "."
		case _ => "^/r/".r.replaceFirstIn(uri,"")
	}

	def handle(t: HttpExchange) {
		val response = doc(new File(lPath(t.getRequestURI.getPath)))
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
