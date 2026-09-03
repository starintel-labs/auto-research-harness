% Task-specific verification for the current worktree.
:- set_prolog_flag(unknown, error).
:- use_module(library(plunit)).
:- use_module(library(time)).
:- ensure_loaded('facts.kb').

current_successful_observation :-
    repo_state(Head, Digest),
    observation(_, _, exit(0), _, Head, Digest).

current_research_evidence :-
    research_required(false).
current_research_evidence :-
    research_required(true),
    repo_state(Head, Digest),
    brave_search(_, _, _, Head, Digest).

base_complete :-
    task(_),
    current_successful_observation,
    current_research_evidence.

% A claimed requirement is observed when a successful observation with the
% given tag exists against the current repo state.
observed(Tag) :-
    repo_state(Head, Digest),
    observation(Tag, _, exit(0), _, Head, Digest).

all_systems_observed :-
    observed(cl_systems_load).

cl_tests_observed :- observed(cl_tests).
org_validator_observed :- observed(org_validator).
transitions_observed :- observed(cl_tests).
forgejo_observed :- observed(cl_tests).
nix_build_observed :- observed(nix_build).

% Task-specific requirements and invariants.
complete :-
    base_complete,
    claimed(requirement, rlm_systems_load, _),
    all_systems_observed,
    claimed(requirement, cl_tests_pass, _),
    cl_tests_observed,
    claimed(requirement, org_validator_clean, _),
    org_validator_observed,
    claimed(requirement, invalid_candidate_transitions_rejected, _),
    transitions_observed,
    claimed(requirement, forgejo_idempotency, _),
    forgejo_observed,
    claimed(requirement, nix_build, _),
    nix_build_observed.

:- begin_tests(workspace_verification).

test(complete) :-
    complete.

:- end_tests(workspace_verification).

main :-
    catch(call_with_time_limit(30, (run_tests, once(complete))),
          Error,
          (print_message(error, Error), fail)),
    !,
    halt(0).
main :-
    halt(1).

:- initialization(main, main).
