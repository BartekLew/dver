package me.leo.dver

import scala.io.Source._
import sys.process._

import java.io.{FileInputStream, FileOutputStream, InputStream, OutputStream, File}
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}


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

trait GetPostResponse{
	def handle(t:HttpExchange) : Unit
}

class TextResponse(content:String) extends GetPostResponse {
	def handle(t:HttpExchange) {
		val response = content.getBytes
		t.sendResponseHeaders(200, response.length)
		val os = t.getResponseBody
		os.write(response)
		os.close()
	}
}

class FileResponse(f:File) extends GetPostResponse {
	def handle(t:HttpExchange) {
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
		var buff = new Array[Byte](4096)
		var s = 0;

		while (in.available > 0) {
			s = in.read(buff)
			os.write(buff, 0, s)
		}

		in.close()
		os.close()
	}
}

class GetPostHandler(respond:GetPostRequest=>GetPostResponse) extends HttpHandler {
	def responseFor(req:GetPostRequest) = try {
		respond(req)
	} catch {
		case e:Exception => new TextResponse(new StackTrace(e).toString)
	}

	def handle(t: HttpExchange) {
		 responseFor(new GetPostRequest("^/[^/]*/", t)).handle(t)
	}
}

class FSItem(path:String) {
	def this(f:File) = this(f.getPath)

	def extension = "^.*\\.".r.replaceFirstIn(path,"").toLowerCase
	def mime = extension match {
		case "pdf" => "application/pdf"
		case "txt" => "text/plain"
		case "html" => "text/html"
		case "jpg" => "image/jpeg"
		case "png" => "image/png"
		case "dng" => "image/dng"
		case "pef" => "image/x-pentax-raw"
		case "ppm" => "image/x-portable-pixmap"
		case "css" => "text/css"
		case "vtt" => "text/vtt"
		case "mp4" => "video/mp4"
		case "avi" => "video/x-msvideo"
		case "mov" => "video/quicktime"
		case _ => "application/octet-stream"
	}

	def imageTransformer = extension match {
		case "dng" | "pef" => new RawTherapee()
		case "jpg" | "png" | "tif" | "bmp" | "ppm" => new ImageMagick()
		case _ => throw new Exception("Not an image file: " + extension)
	}
}

class ImageHandler extends GetPostHandler(req => 
	new ImageFile(req.query, new FSItem(req.query).imageTransformer).transform(req.post)
)

class DownloadHandler() extends GetPostHandler(req => new FileResponse(new File(req.query)))

class WriteHandler extends HttpHandler {
	def lPath(t:HttpExchange) =
		"." + "^/w".r.replaceFirstIn(t.getRequestURI.getPath,"")

	def handle(t:HttpExchange) {
		{
			val o = new FileOutputStream(lPath(t))
			val i = t.getRequestBody

			var buff = new Array[Byte](4096)
			var s = 0
			do {
				s = i.read(buff)
				if(s > 0)
					o.write(buff, 0, s)
			} while (s>0)

			i.close
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