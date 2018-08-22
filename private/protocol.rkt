#lang racket/base

;; References:
;; - [1] https://jupyter-client.readthedocs.io/en/stable/kernels.html
;; - [2] https://jupyter-client.readthedocs.io/en/stable/messaging.html

;; In 2018-08, the docs above defined version 5.3 of the Jupyter protocol.

;; ============================================================
;; General Message Format

;; A message has the following structure [2]:
;;
;; {
;;   # The message header contains a pair of unique identifiers for the
;;   # originating session and the actual message id, in addition to the
;;   # username for the process that generated the message.  This is useful in
;;   # collaborative settings where multiple users may be interacting with the
;;   # same kernel simultaneously, so that frontends can label the various
;;   # messages in a meaningful way.
;;   'header' : {
;;                 'msg_id' : str, # typically UUID, must be unique per message
;;                 'username' : str,
;;                 'session' : str, # typically UUID, should be unique per session
;;                 # ISO 8601 timestamp for when the message is created
;;                 'date': str,
;;                 # All recognized message type strings are listed below.
;;                 'msg_type' : str,
;;                 # the message protocol version
;;                 'version' : '5.0',
;;      },
;;
;;   # In a chain of messages, the header from the parent is copied so that
;;   # clients can track where messages come from.
;;   'parent_header' : dict,
;;
;;   # Any metadata associated with the message.
;;   'metadata' : dict,
;;
;;   # The actual content of the message must be a dict, whose structure
;;   # depends on the message type.
;;   'content' : dict,
;;
;;   # optional: buffers is a list of binary data buffers for implementations
;;   # that support binary extensions to the protocol.
;;   'buffers': list,
;; }

;; ============================================================
;; The Wire Format

;; A Jupyter message is serialized as a multi-frame ZMQ message with
;; the following frame structure [2]:

;; message ::=
;;   [identity, ... , separator, hmac-sig, header, parent-header, metadata, content, extra, ...]
;; separator = "<IDS|MSG>"
;; hmac-sig is hex string (empty if authentication not enabled)
;; header, parent-header, metadata, and content are serialized JSON dicts

;; ============================================================
;; Shell-channel Messages

;; ----------------------------------------
;; Error Responses

;; Most/all response types have an error variant with the following structure:

;; error_reply message = {
;;   status : "error"
;;   ename : String  -- exception name
;;   evalue : String  -- exception value
;;   traceback : Listof String
;; }

;; The rest of these notes document only the success reply variant.

;; ----------------------------------------
;; Execute (execute_request)

;; execute_request message = {
;;   code : String  -- might be empty
;;   silent : Boolean
;;   store_history : Boolean  -- increment counter
;;   user_expressions: Hash[String => String(?)] -- executed *after* code
;;   allow_stdin : Boolean
;;   stop_on_error : Boolean
;; }

;; execute_reply message = {
;;   status : "ok"
;;   execution_count : Integer
;;   payload : Listof Hash[???]  -- deprecated
;;   user_expressions : Hash[String => ??]
;; }

;; IIUC, the error variant also contains the execution_count field.

;; ----------------------------------------
;; Introspection

;; inspect_request message = {
;;   code : String
;;   cursor_pos : Integer  -- "offset in unicode codepoints"
;;   detail_level : (U 0 1)
;; }

;; inspect_reply message = {    -- "like display_data message"
;;   status = "ok" | "error"
;;   found : Boolean
;;   data : Hash[??]
;;   metadata : Hash[??]
;; }

;; ----------------------------------------
;; Completion

;; complete_request message = {
;;   code : String
;;   cursor_pos : Integer  -- offset in unicode characters
;; }

;; complete_reply message = {
;;   status : "ok"
;;   matches : Listof String
;;   cursor_start : Integer
;;   cursor_end : Integer  -- usually same as start
;;   metadata : Hash[??]
;; }

;; ----------------------------------------
;; History

;; history_request = {
;;   output : Boolean
;;   raw : Boolean
;;   hist_access_type : (U "range" "tail" "search")
;;   -- if access type is range:
;;   session: Integer
;;   start, stop : Integer  -- cell numbers
;;   -- if access type is "tail" or "search":
;;   n : Integer  -- get the last n cells
;;   -- if access type is "search"
;;   pattern : String  -- get cells matching pattern, with * and ? wildcards
;;   unique : Boolean  -- do not include duplicated history
;; }

;; history_reply message = {
;;   history : Listof 3Tuple (??!)
;;   -- 3Tuple is one of (session, line_number, (input, output)) or (session, line_number, input)
;;   -- depending on whether output was true or false
;; }

;; ----------------------------------------
;; Code Completeness

;; is_complete_request = {
;;   code : String
;; }

;; is_complete_reply message = {
;;   status : (U "complete" "incomplete" "invalid" "unknown")
;;   -- if status is "complete":
;;   indent : String
;; }

;; ----------------------------------------
;; Connect (deprecated since 5.1)

;; connect_request message = {}

;; connect_reply message = {
;;   shell_port : Integer
;;   iopub_port : Integer
;;   stdin_port : Integer
;;   hb_port : Integer
;;   control_port : Integer
;; }

;; ----------------------------------------
;; Comm info (new in 5.1)

;; comm_info_request = {
;;   -- optional:
;;   target_name : String  -- only return currently open comms for given target
;; }

;; comm_info_reply = {
;;   comms : {  -- indexed by UUIDs
;;     $comm_id : { target_name : String, ... },
;;     ...
;;   }
;; }

;; ----------------------------------------
;; Kernel info

;; kernel_info_request = {}

;; kernel_info_reply = {
;;   protocol_version : String  -- matching "X.Y.Z"
;;   implementation : String  -- eg "iracket"
;;   implementation_version : String  -- matching "X.Y.Z"
;;   language_info : {
;;     name : String  -- eg "racket"
;;     version : String  -- eg "7.0"
;;     mimetime : String  -- "mime type for script files in this language"
;;     file_extension : String  -- including the dot, eg ".rkt"
;;     pygments_lexer : String  -- only needed if different from name field
;;     codemirror_mode : String or Dict -- only needed if different from name field
;;     nbconvert_exporter : String
;;   }
;;   banner : String
;;   help_links : [ { text : String, url : String }, ...]
;; }

;; ----------------------------------------
;; Kernel shutdown (on control or shell channel)

;; kernel_shutdown_request = { restart : Boolean }

;; kernel_shutdown_reply = { restart : Boolean }

;; ----------------------------------------
;; Kernel interrupt (since 5.3) (on control channel)

;; interrupt_request = {}
;; interrupt_reply = {}

;; ============================================================
;; IOPub-channel messages

;; stream = {
;;   name : (U "stdout" "stderr")
;;   text : String
;; }

;; display_data = {
;;   data : Dict[??]
;;   metadata : Dict[??]
;;   transient : Dict[??] -- not persisted to notebook (since 5.1)
;; }

;; update_display_data = {  -- since 5.1
;;   data : Dict[??]
;;   metadata : Dict[??]
;;   transient : Dict[??]
;; }

;; execute_input = {
;;   code : String
;;   execution_count : Integer
;; }

;; execution_result = {
;;   execution_count : Integer
;;   data, metadata : Dict[??]  -- like display_data fields
;; }

;; error = {
;;   -- same as execute_reply error case, except "status" field omitted
;; }

;; kernel_status = {
;;   execution_state : (U "busy" "idle" "starting")
;; }

;; clear_output = {
;;   wait : Boolean
;; }

;; ============================================================
;; stdin-channel messages

;; Note: requests come from kernel, replies come from frontend

;; input_request = {
;;   prompt : String
;;   password : Boolean  -- don't echo input
;; }

;; input_reply = {
;;   value : String
;; }

;; ============================================================
;; Heartbeats

;; A heartbeat message consists of a simple bytestring; kernel should
;; just echo same bytestring back (no parsing).

;; ============================================================
;; Custom messages

;; Kernel receives on shell channel, sends to frontend on IOPub channel.

;; comm_open = {
;;   comm_id : String  -- UUID
;;   target_name : String
;;   data : Dict
;; }

;; comm_msg = {
;;   comm_id : String
;;   data : Dict
;; }

;; comm_close = {
;;  comm_id : String
;;  data : Dict
;; }
