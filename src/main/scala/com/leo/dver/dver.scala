package me.leo.dver

import java.io.{FileOutputStream, File}
import java.net.InetSocketAddress
import scala.io.Source._
import scala.collection.immutable.Map

import scala.sys.process._

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

object Main {
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
			var is = new File(cwd + "/box.in")
			if(is.exists){ is.delete() }
			new File(cwd + "/box.out").createNewFile()
			new File(cwd + "/box.err").createNewFile()
			Process("box bash -i", new File(pwd)).!!
		}

		val o = new FileOutputStream(pwd + "/box.in")
		o.write((cmd + "\n").getBytes)
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
		server.createContext("/img/", new ImageHandler())
		server.createContext("/sh/", new GetPostHandler( req => new TextResponse(
			req.method match {
				case "GET" =>
					throw new Exception("Illegal method: GET")
				case "POST" => shCmd(req.post, "./" + req.query)
			})))
		server.setExecutor(
			java.util.concurrent.Executors.newCachedThreadPool()
		)
		server.start()
	}

}

