package me.leo.dver

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
