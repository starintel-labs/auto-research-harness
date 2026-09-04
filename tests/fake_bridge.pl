% fake_bridge.pl --- a scripted structured transport for tests.
%
% Implements the same stdio JSONL protocol as rlm_bridge.pl without the
% prolog-rlm runtime: after a run request it round-trips one tool call to
% the host and emits a terminal ok(direct_result{...}) envelope. This
% proves the CL side (framing, tool dispatch, result/usage parsing) with
% a fully deterministic, network-free backend.

:- initialization(main, main).
:- use_module(library(http/json)).
:- use_module(library(readutil)).

main :-
    request_loop.

request_loop :-
    catch((json_read_dict(current_input, Request, [end_of_file(error)]),
           handle_request(Request)),
          Error,
          (format(current_output, "~s~n", ["{\"op\":\"error\"}"]),
           halt(2))),
    (   get_dict(op, Request, Op), Op == "run"
    ->  halt(0)
    ;   request_loop
    ).

handle_request(Request) :-
    get_dict(op, Request, Op),
    (   Op == "ping"
    ->  emit(json{op:"pong"})
    ;   Op == "run"
    ->  run_fake_session(Request)
    ;   emit(json{op:"error", message:"unknown op"})
    ).

emit(Message) :-
    with_output_to(string(Json),
                   json_write_dict(current_output, Message, [width(0)])),
    format(current_output, "~s~n", [Json]),
    flush_output(current_output).

usage_dict(Tokens, json{model_calls:2,
                        prompt_tokens:120,
                        completion_tokens:30,
                        total_tokens:Tokens,
                        cost_usd:0.0,
                        cost_known:true,
                        tokens_known:true}).

result_envelope(PayloadValue, Text, ToolCalls, Tokens) :-
    usage_dict(Tokens, Usage),
    Payload = json{'$term':"ok",
                   args:[json{value:Text,
                              payload_value:PayloadValue,
                              usage:Usage,
                              turns:2,
                              iterations:2,
                              context_calls:0,
                              tool_calls:ToolCalls,
                              observation_bytes:64,
                              output_bytes:128,
                              trajectory:[]}]},
    Envelope = json{schema:"prolog-rlm.trace.v1",
                    name:"direct_session",
                    generated_at:"2026-01-01T00:00:00+0000",
                    payload:Payload},
    emit(json{op:"result", envelope:Envelope}).

run_fake_session(Request) :-
    get_dict(query, Request, Query),
    (   get_dict(tools, Request, [_|_])
    ->  emit(json{op:"tool_call",
                  call_id:"call_fake_1",
                  name:"search-research",
                  arguments:json{query:"test"}}),
        json_read_dict(current_input, Reply, [end_of_file(error)]),
        (   get_dict(ok, Reply, true)
        ->  ToolOk = true
        ;   ToolOk = false
        ),
        result_envelope(json{'$term':"ok",
                             with_tools:ToolOk,
                             text:Query},
                        "Say FAKE_OK",
                        1,
                        150)
    ;   result_envelope(json{'$term':"ok", text:Query}, "Say FAKE_OK", 0, 15)
    ).
