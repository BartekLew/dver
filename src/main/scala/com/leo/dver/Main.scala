package me.leo.dver

import java.io.{InputStream, OutputStream, File}
import java.net.InetSocketAddress

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

object Main {

	def main(args: Array[String]) {
		val server = HttpServer.create(new InetSocketAddress(8000), 0)
		server.createContext("/", new RootHandler())
		server.setExecutor(null)
		server.start()
	}

}

class RootHandler extends HttpHandler {
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
		simpleLink("^\\.".r.replaceFirstIn(f.getPath,""), f.getName)		

	def doc(innerHtml:String) : String = tag("html",
		tag("body", innerHtml)
	)

	def handle(t: HttpExchange) {
		val response = doc(blist(
			new File( "." + t.getRequestURI.getPath)
				.listFiles.toList.map(fileLink)
		))
		t.sendResponseHeaders(200, response.length())
		val os = t.getResponseBody
		os.write(response.getBytes)
		os.close()
	}

}
