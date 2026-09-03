% rlm_bridge.pl --- stdio JSONL bridge between a Common Lisp host and the
% Prolog-RLM runtime in direct mode.
%
% Protocol (one JSON value per line):
%   host -> bridge: {"op":"ping"}                          -> {"op":"pong"}
%   host -> bridge: {"op":"run","run_id":..,"query":..,
%                    "context":..,"budget":{..},
%                    "provider":{..},"capabilities":[..],
%                    "tools":[registry specs]}
%   bridge -> host: {"op":"tool_call","call_id":..,"name":..,"arguments":{..}}
%   host -> bridge: {"op":"tool_result","call_id":..,"ok":true,"value":..}
%                   | {"op":"tool_result","call_id":..,"ok":false,"error":{..}}
%   bridge -> host: {"op":"result","envelope":{trace envelope}}
%   bridge -> host: {"op":"error","message":..} then exit(1)
%
% The envelope payload reuses rlm_trace's portable encoding, so Prolog
% compound terms are represented as {"$term":Functor,"args":[...]}.

:- initialization(bridge_main, main).

:- use_module(library(http/json)).
:- use_module(library(readutil)).
:- use_module(library(lists)).

bridge_main :-
    catch((use_module(library(rlm)),
           use_module(library(rlm_trace)),
           use_module(library(rlm_tool)),
           use_module(library(rlm_chain))),
          E0,
          (message_to_string(E0, Msg0),
           format(string(PreMsg0),
                  "prolog-rlm pack unavailable; set PROLOG_RLM_SWIPL to the prolog-rlm swipl wrapper: ~s",
                  [Msg0]),
           emit(json{op:"error", message:PreMsg0}),
           halt(3))),
    catch(request_loop,
          Error3,
          (emit_error(Error3), halt(2))).

request_loop :-
    catch(read_request(Request0),
          Error4,
          (emit_error(Error4), halt(2))),
    (   get_dict(op, Request0, Op), Op == "ping"
    ->  emit(json{op:"pong"}),
        request_loop
    ;   get_dict(op, Request0, Op), Op == "run"
    ->  catch(run_session(Request0),
              Error2,
              (emit_error(Error2), halt(1)))
    ;   emit(json{op:"error", message:"unknown op"}),
        halt(2)
    ).

read_request(Request) :-
    json_read_dict(current_input, Request, [end_of_file(error)]),
    get_dict(op, Request, _),
    !.
read_request(_) :-
    throw(bridge_fault("frame is not a request object")).

% ---------------------------------------------------------------- emission

emit(Message) :-
    with_output_to(string(Json),
                   json_write_dict(current_output, Message, [width(0)])),
    format(current_output, "~s~n", [Json]),
    flush_output(current_output).

emit_error(Error) :-
    (   catch(message_to_string(Error, Message), _, fail)
    ->  true
    ;   format(string(Message), "~q", [Error])
    ),
    emit(json{op:"error", message:Message}).

% ---------------------------------------------------------------- sessions

run_session(Request) :-
    run_session_(Request, Outcome),
    trace_envelope("direct_session", Outcome, Envelope),
    emit(json{op:"result", envelope:Envelope}).

run_session_(Request, Outcome) :-
    atom_query(Request, Query),
    context_term(Request, Context),
    session_options(Request, Options),
    register_session_tools(Request, Registry),
    rlm_direct_execute(Query, Context, [tool_registry(Registry)|Options], Outcome).

atom_query(Request, Query) :-
    get_dict(query, Request, Query0),
    atom_string(Query, Query0).

% Context is an opaque bounded string; an absent context maps to an empty
% opaque text context because the runtime requires a context argument.
context_term(Request, text("")) :-
    \+ (get_dict(context, Request, C), C \== "", C \== null),
    !.
context_term(Request, text(Context)) :-
    get_dict(context, Request, Context0),
    atom_string(Context, Context0).

session_options(Request, Options) :-
    budget_option(Request, BudgetOption),
    provider_option(Request, ProviderOption),
    capabilities_option(Request, CapabilityOption),
    append([BudgetOption, ProviderOption, CapabilityOption], [], Options).

budget_option(Request, budget(Budget)) :-
    get_dict(budget, Request, Spec),
    is_dict(Spec),
    !,
    rlm:default_completion_budget(Default),
    budget_from_spec(Spec, Default, Budget).
budget_option(_, []).

budget_from_spec(Spec, Default, Budget) :-
    budget_override(Default, Spec, max_iterations, B1),
    budget_override(B1, Spec, max_model_calls, B2),
    budget_override(B2, Spec, max_tool_calls, B3),
    budget_override(B3, Spec, max_total_tokens, B4),
    budget_override(B4, Spec, max_cost_usd, B5),
    budget_override(B5, Spec, max_output_bytes, B6),
    budget_override(B6, Spec, time_limit, Budget).

budget_override(Budget0, Spec, Field, Budget1) :-
    (   get_dict(Field, Spec, Value),
        Value \== null
    ->  put_dict(Field, Budget0, Value, Budget1)
    ;   Budget1 = Budget0
    ).

provider_option(Request, provider(Provider)) :-
    get_dict(provider, Request, Spec),
    is_dict(Spec),
    !,
    provider_from_spec(Spec, Provider).
provider_option(_, []).

provider_from_spec(Spec, Provider) :-
    (   get_dict(endpoint, Spec, Endpoint0)
    ->  atom_string(Endpoint, Endpoint0)
    ;   throw(bridge_fault("custom provider requires an endpoint"))
    ),
    (   get_dict(model, Spec, Model0)
    ->  atom_string(Model, Model0)
    ;   throw(bridge_fault("custom provider requires a model"))
    ),
    (   get_dict(no_credential, Spec, true)
    ->  Credential = none
    ;   get_dict(credential_env, Spec, EnvName0)
    ->  atom_string(EnvName, EnvName0),
        Credential = env(EnvName)
    ;   Credential = env('OPENROUTER_API_KEY')
    ),
    rlm_chain:openai_compatible_provider(Endpoint, Credential, Model, Provider).

capabilities_option(Request, capabilities(Capabilities)) :-
    get_dict(capabilities, Request, Requested),
    is_list(Requested),
    !,
    maplist(capability_term, Requested, Capabilities).
capabilities_option(_, []).

% "context(slice)" -> context(slice); "tool(search_research)" -> tool(search_research)
capability_term(Capability, Term) :-
    (   sub_atom(Capability, Before, _, After, '(')
    ->  sub_atom(Capability, 0, Before, _, Kind),
        sub_atom(Capability, _, After, _, Tail0),
        atom_chars(Tail0, Chars0),
        reverse(Chars0, [')'|Rest0]),
        reverse(Rest0, Rest1),
        atom_chars(Inner, Rest1),
        atom_string(Inner, InnerString),
        atom_string(Kind0, Kind),
        inner_term(Kind0, InnerString, Term)
    ;   atom_string(Term, Capability)
    ).

inner_term("context", Inner, Context) :-
    !,
    atom_string(InnerAtom, Inner),
    Context =.. [InnerAtom].
inner_term("tool", Inner, tool(Tool)) :-
    !,
    atom_string(Tool, Inner).
inner_term(_, Inner, Term) :-
    atom_string(Term, Inner).

% ------------------------------------------------------------------- tools

register_session_tools(Request, Registry) :-
    tool_registry_create(Registry),
    (   get_dict(tools, Request, Tools)
    ->  forall(member(Spec, Tools), register_bridge_tool(Registry, Spec))
    ;   true ).

register_bridge_tool(Registry, Spec) :-
    tool_spec_name(Spec, Name),
    get_dict(description, Spec, Description),
    get_dict(arguments, Spec, Arguments),
    get_dict(effect, Spec, EffectString),
    atom_string(EffectAtom, EffectString),
    (   get_dict(time_limit, Spec, TimeLimit)
    ->  true
    ;   TimeLimit = 1.0
    ),
    (   get_dict(max_output_bytes, Spec, MaxOutputBytes)
    ->  true
    ;   MaxOutputBytes = 4096
    ),
    Schema = tool_schema{name:Name,
                         description:Description,
                         capability:tool(Name),
                         effect:EffectAtom,
                         arguments:Arguments,
                         result:_{},
                         limits:_{time_limit:TimeLimit,
                                  max_output_bytes:MaxOutputBytes}},
    tool_register(Registry, Schema, bridge_tool_handler(Name), _).

tool_spec_name(Spec, Name) :-
    get_dict(name, Spec, Name0),
    atom_string(Name, Name0).

% The handler synchronously round-trips the call to the CL host over stdio.
% A mutex serializes the exchange because rlm_async worker threads may
% invoke tools concurrently.
bridge_tool_handler(Name, Args, Value) :-
    with_mutex(bridge_stdio,
               bridge_roundtrip(Name, Args, Value)).

bridge_roundtrip(Name, Args, Value) :-
    next_call_id(CallId),
    emit(json{op:"tool_call",
              call_id:CallId,
              name:Name,
              arguments:Args}),
    json_read_dict(current_input, Reply, [end_of_file(error)]),
    (   get_dict(ok, Reply, true)
    ->  (   get_dict(value, Reply, Value0),
            Value0 \== null
        ->  Value = Value0
        ;   Value = _{}
        )
    ;   get_dict(error, Reply, ErrorSpec)
    ->  throw(bridge_host_tool_error{tool:Name, error:ErrorSpec})
    ;   throw(bridge_fault("malformed tool_result frame"))
    ).

:- dynamic(call_id_counter/1).
call_id_counter(0).

next_call_id(CallId) :-
    with_mutex(bridge_call_ids,
               (   retract(call_id_counter(N)),
                   N1 is N + 1,
                   assertz(call_id_counter(N1))),
               format(string(CallId), "call_~d", [N])).
