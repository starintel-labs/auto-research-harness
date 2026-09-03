# Auto Research CL Runtime Spec

This folder specifies the migration of StarIntel Auto Research to a reusable Common Lisp runtime.

## Goal

Build a Common Lisp runtime that:

- preserves the existing `lost-rob0t/starintel-auto-research` Org/ledger contracts;
- uses ARADR as the workflow/control semantics;
- uses a reusable CL harness to invoke Prolog-RLM in **direct mode**;
- uses Forgejo as the issue/request plane;
- exposes a website HTTP API over the same CL application API;
- can also be invoked by CLI, cron, Agent Zero, Zara, or StarIntel without caller-specific workflow logic.

## Read order

1. `01-architecture.org`
2. `02-existing-contracts.org`
3. `03-cl-runtime.org`
4. `04-project-dsl.org`
5. `05-prolog-rlm-harness.org`
6. `06-aradr.org`
7. `07-forgejo.org`
8. `08-web-api.org`
9. `09-security-events.org`
10. `10-tests-acceptance.org`

`IMPLEMENT.md` is the short execution prompt for the coding model.

## Hard constraints

- Do not rewrite Auto Research from scratch.
- Do not invent a replacement Org research schema.
- Do not collapse lifecycle and human approval into one state.
- Do not make Agent Zero a dependency.
- Do not implement symbolic Prolog-RLM yet.
- Do not give the LLM unrestricted shell or canonical filesystem writes.
- Do not move the canonical corpus into the website repo.
