(* webhook.ml *)

(* Define the expected payload structure *)
type payment_payload = {
  event : string;
  transaction_id : string;
  amount : float;
  currency : string;
  timestamp : string; (* For simplicity, keeping as string. Could use Ptime for real date/time parsing *)
} [@@deriving yojson] (* This PPX generates of_yojson and to_yojson functions *)

(* --- Validation Logic --- *)

(* 1. Basic payload structure and content validation *)
let is_valid_payload_content (payload : payment_payload) : (unit, string) result =
  if payload.event <> "payment_success" then
    Error "Invalid event type"
  else if payload.currency <> "BRL" then
    Error "Invalid currency"
  else if payload.amount <= 0.0 then
    Error "Invalid amount"
  (* Add more checks: transaction_id format, timestamp format (if parsing), etc. *)
  else
    Ok ()

(* 2. "Payment correctness" check (placeholder) *)
(* This is where you'd interact with a database, an external service, or apply business rules *)
(* let is_payment_correct (payload : payment_payload) : (unit, string) result Lwt.t =
  Printf.printf "Checking payment correctness for transaction_id: %s, amount: %.2f %s\n"
    payload.transaction_id payload.amount payload.currency;

  (* Simulate some business logic, e.g., checking against a known list of fraudulent IDs *)
  if payload.transaction_id = "fraud999" then
    Lwt.return (Error "Transaction flagged as potentially fraudulent")
  else if payload.amount > 10000.00 then
    Lwt.return (Error "Amount exceeds processing limit for this webhook")
  else
    Lwt.return (Ok ()) (* Placeholder: assume payment is correct if not flagged *)


(* --- Request Handler --- *)
let handle_webhook_request request =
  match Dream.method_ request with
  | `POST ->
      let%lwt body = Dream.body request in
      (try
        let json = Yojson.Safe.from_string body in
        match payment_payload_of_yojson json with (* Uses ppx_deriving_yojson generated function *)
        | Ok payload ->
            (match is_valid_payload_content payload with
             | Error msg ->
                 Dream.json ~status:`Bad_Request (Printf.sprintf {|{"status": "error", "message": "Invalid payload content: %s"}|} msg)
             | Ok () ->
                 (* Payload content is valid, now check "payment correctness" *)
                 let%lwt payment_check_result = is_payment_correct payload in
                 match payment_check_result with
                 | Ok () ->
                     Printf.printf "Payment successful for transaction %s\n" payload.transaction_id;
                     Dream.json ~status:`OK {|{"status": "received", "message": "Payment processed successfully"}|}
                 | Error msg ->
                     Printf.printf "Payment check failed for transaction %s: %s\n" payload.transaction_id msg;
                     (* "Cancel" can be interpreted as a 400 Bad Request with a specific error message *)
                     Dream.json ~status:`Bad_Request (Printf.sprintf {|{"status": "error", "message": "Payment check failed: %s"}|} msg)
            )
        | Error msg -> (* Deserialization error (e.g., missing fields, wrong types) *)
            Dream.json ~status:`Bad_Request (Printf.sprintf {|{"status": "error", "message": "Invalid payload structure: %s"}|} msg)
      with
      | Yojson.Json_error msg -> (* Malformed JSON *)
          Dream.json ~status:`Bad_Request (Printf.sprintf {|{"status": "error", "message": "Malformed JSON: %s"}|} msg)
      | ex -> (* Catch any other unexpected error during processing *)
          let error_msg = Printexc.to_string ex in
          Printf.eprintf "Unexpected error: %s\n%s\n" error_msg (Printexc.get_backtrace ());
          Dream.json ~status:`Internal_Server_Error {|{"status": "error", "message": "Internal server error"}|}
      )
  | _ ->
      Dream.json ~status:`Method_Not_Allowed {|{"status": "error", "message": "Method not allowed, please use POST"}|}

(* --- HTTPS Setup (Self-Signed Certificate for Localhost) --- *)
(*
  Generate self-signed certificate and key for local testing:
  openssl req -x509 -newkey rsa:2048 -nodes \
    -keyout key.pem -out cert.pem \
    -subj "/C=US/ST=CA/L=City/O=Org/CN=localhost" \
    -days 365
*)
let () =
  Dream.run ~https:true ~certificate_file:"cert.pem" ~key_file:"key.pem"
  @@ Dream.logger (* Logs requests to console *)
  @@ Dream.router [
    Dream.post "/webhook" handle_webhook_request;
    (* You can add other routes here, e.g., a health check *)
    Dream.get "/health" (fun _ -> Dream.html "OK");
  ]
  @@ Dream.not_found Handles routes not matched *)