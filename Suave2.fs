namespace Suave2
module Http
	type RequestType = GET | POST
	type Request = {
		Route : string,
		Type : RequestType
	}
	type Response = {
		StatusCode : int,
		Content : string
	}
	type Context = {
		Request : Request,
		Response : Response
	}
	type WebPart = Context -> Async<Context option>

module Successful
	open Http

	let Ok content context =
		{ context with Response = { StatusCode = 200, Content = content } }
		|> Some
		|> async.Return

module Combinators
	let compose one two context = async {
		let! contextOne = one context
		match contextOne with
		| None -> return None
		| Some context ->
			let secondContext = two context
			return secondContext
	}

	let (>=>) = compose